#include <string.h>

#include "sim_one_strike.h"

/*
 * Simulate the strike attack by attack, while caching the partial probability tree
 */

static sa_cache_t * sa_c_new(attacker_t A, defender_t D);
static void sa_c_free(sa_cache_t *c);
static void sa_c_detach(const ss_res_t const *ss_res, sa_cache_t *c);
static ss_res_t * sim_attacks(attacker_t A, defender_t D, sa_cache_t *c);

ss_res_t * sim_one_strike_attacks(attacker_t A, defender_t D)
{
	ss_res_t *ss_res;
	sa_cache_t *c = sa_c_new(A, D);
	ss_res = sim_attacks(A, D, c);
	sa_c_detach(ss_res, c);
	sa_c_free(c);
	return ss_res;
}

static inline int min(int a, int b)
{
	return a < b ? a : b;
}

static int min_hp_change(attacker_t A)
{
	if (!A->dmg_min || A->dmg_min == A->dmg_max) return A->dmg_max;
	return min(A->dmg_min, A->dmg_max - A->dmg_min);
}

ss_res_t * new_ss_res(attacker_t A, defender_t D)
{
	int hp_delta = min_hp_change(A);
	int D_hp_step = D->hp/hp_delta + 1;
	int A_len = A->n + 1;
	int D_len = (D->n+1) * D_hp_step;
	ss_res_t * s = malloc(sizeof(ss_res_t)+(A_len+D_len)*sizeof(double));
	memset(s->D, 0, (A_len+D_len)*sizeof(double));
	s->hp_delta = hp_delta;
	s->D_hp_step = D_hp_step;
	s->A_start = 0;
	s->D_start = 0;
	s->A_len = A_len;
	s->D_len =  D_len;
	s->A = s->D + D_len;
	return s;
}

void ss_res_free(ss_res_t *s)
{
	if (!s) return;
	free(s);
}

ss_res_t * ss_res_copy(const ss_res_t * const src)
{
	int alloc = sizeof(ss_res_t) + (src->A_len+src->D_len)*sizeof(double);
	ss_res_t *dst = malloc(alloc);
	memcpy(dst, src, alloc);
	dst->A = dst->D + dst->D_len;
	return dst;
}

void ss_res_mul(ss_res_t * const s, const double p)
{
	int n, i;
	n = s->A_len;
	for (i = s->A_start; i < n; i++)
		if (s->A[i])
			s->A[i] *= p;
	n = s->D_len;
	for (i = s->D_start; i < n; i++)
		if (s->D[i])
			s->D[i] *= p;
}

static inline int max(int a, int b)
{
	return a > b ? a : b;
}

void ss_res_add(ss_res_t * const sum, const ss_res_t * const term, const double p)
{
	int i, n;
	sum->A_len = max(sum->A_len, term->A_len);
	sum->D_len = max(sum->D_len, term->D_len);
	sum->A_start = min(sum->A_start, term->A_start);
	sum->D_start = min(sum->D_start, term->D_start);
	n = term->A_len;
	for (i = term->A_start; i < n; ++i)
		if (term->A[i])
			sum->A[i] += term->A[i] * p;
	n = term->D_len;
	for (i = term->D_start; i < n; ++i)
		if (term->D[i])
			sum->D[i] += term->D[i] * p;
}

static sa_cache_t * sa_c_new(const attacker_t A, const defender_t D)
{
	sa_cache_t *c;
	c = malloc(sizeof(sa_cache_t));
	c->hp_delta = min_hp_change(A);
	c->d_step = D->hp/c->hp_delta + 1;
	c->a_step = c->d_step * (D->n + 1);
	c->len = c->a_step * (A->n + 1);
	c->r = calloc(c->len, sizeof(ss_res_t*));
	c->kd_c = kd_cache_new();
	return c;
}

void sa_c_free(sa_cache_t *c)
{
	int i;
	for (i = 0; i < c->len; i++)
		if (c->r[i])
			ss_res_free(c->r[i]);
	free(c->r);
	kd_cache_free(c->kd_c);
	free(c);
}

int hit, miss;
static ss_res_t * sa_c_get(attacker_t A, defender_t D, const sa_cache_t * const c)
{
	ss_res_t * r = c->r[A->n * c->a_step + D->n * c->d_step + D->hp_remaining/c->hp_delta];
	r ? hit++ : miss++;
	return r;
}

static void sa_c_put(attacker_t A, defender_t D, ss_res_t *r, sa_cache_t * const c)
{
	c->r[A->n * c->a_step + D->n * c->d_step + D->hp_remaining/c->hp_delta] = r;
	//printf("PUT: Na %i, Nd %i, hp %i\n", A->n, D->n, D->hp_remaining);
}

// Transfers ownership of the ss_res memory to the caller
static void sa_c_detach(const ss_res_t * const ss_res, sa_cache_t * const c)
{
	int i;
	for (i = c->len-1; i >= 0; --i)
		if (c->r[i] == ss_res) {
			c->r[i] = NULL;
			return;
		}
}

static void for_each_sub_sim(plist_t *p1, attacker_t A, defender_t D, ss_res_t *ss_res, sa_cache_t * const c)
{
	ss_res_t *s;
	const int Na_in = A->n;
	const int Nd_in = D->n;
	const int hp_in = D->hp_remaining;
	ss_res->A_len = 1;
	ss_res->D_len = 1;
	for (; p1; p1 = free_p_element(p1)) {
		A->n = p1->Na;
		D->n = p1->Nd;
		D->hp_remaining = p1->hp_remaining;
		s = sim_attacks(A, D, c);
		ss_res_add(ss_res, s, p1->p);
	}
	A->n = Na_in;
	D->n = Nd_in;
	D->hp_remaining = hp_in;
}

static ss_res_t * sim_attacks(attacker_t A, defender_t D, sa_cache_t * const c)
{
	ss_res_t *r_out, *r1, *r2;

	//printf("Na: %i, Nd: %i, hp: %i\n", A->n, D->n, D->hp_remaining);

	if ((r_out = sa_c_get(A, D, c)))
		return r_out;

	if (D->n == 0) {
		r_out = new_ss_res(A, D);
		r_out->A[A->n] = 1;
		r_out->A_start = A->n;
		r_out->D_start = r_out->D_len;
		sa_c_put(A, D, r_out, c);
		return r_out;
	}
	if (A->n == 0) {
		r_out = new_ss_res(A, D);
		int Di = D->n * r_out->D_hp_step + D->hp_remaining/r_out->hp_delta;
		r_out->D[Di] = 1;
		r_out->A_start = r_out->A_len;
		r_out->D_start = Di;
		sa_c_put(A, D, r_out, c);
		return r_out;
	}

	plist_t *sure_kill = kill_defenders(A, D, c->kd_c);
	if (sure_kill) {
		r_out = new_ss_res(A, D);
		r_out->A_start = r_out->A_len;
		r_out->D_start = r_out->D_len;
		for_each_sub_sim(sure_kill, A, D, r_out, c);
		sa_c_put(A, D, r_out, c);
		return r_out;
	}

	const int hp_in = D->hp_remaining;
	const int Dn_in = D->n;
	A->n--;

	// Low dmg prob branch
	if (hp_in <= A->dmg_min) {
		D->n = Dn_in - 1;
		D->hp_remaining = D->n ? D->hp : 0;
		r1 = sim_attacks(A, D, c);
		r_out = ss_res_copy(r1);
		goto sa_out;
	} else {
		D->n = Dn_in;
		D->hp_remaining = hp_in - A->dmg_min;

		r1 = sim_attacks(A, D, c);
		r_out = ss_res_copy(r1);
		ss_res_mul(r_out, 1 - A->accuracy);
	}
	// High dmg prob branch
	if (hp_in > A->dmg_max) {
		D->n = Dn_in;
		D->hp_remaining = hp_in - A->dmg_max;
	} else {
		D->n = Dn_in - 1;
		D->hp_remaining = D->n ? D->hp : 0;
	}
	r2 = sim_attacks(A, D, c);
	ss_res_add(r_out, r2, A->accuracy);

sa_out:
	A->n++;
	D->n = Dn_in;
	D->hp_remaining = hp_in;

	sa_c_put(A, D, r_out, c);

	return r_out;
}
