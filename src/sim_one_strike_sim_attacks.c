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
	ss_res_t * ss_res = malloc(sizeof(ss_res_t));
	ss_res->hp_delta = min_hp_change(A);
	ss_res->D_hp_step = D->hp/ss_res->hp_delta + 1;
	ss_res->A = calloc(A->n+1, sizeof(double));
	ss_res->D = calloc((D->n+1) * ss_res->D_hp_step, sizeof(double));
	return ss_res;
}

void ss_res_free(ss_res_t *s)
{
	if (!s) return;
	free(s->A);
	free(s->D);
	free(s);
}

static sa_cache_t * sa_c_new(attacker_t A, defender_t D)
{
	sa_cache_t *c;
	c = malloc(sizeof(sa_cache_t));
	c->hp_delta = min_hp_change(A);
	c->d_step = D->hp/c->hp_delta + 1;
	c->a_step = c->d_step * (D->n + 1);
	c->len = c->a_step * (A->n + 1);
	c->r = calloc(c->len, sizeof(ss_res_t*));
	return c;
}

void sa_c_free(sa_cache_t *c)
{
	int i;
	for (i = 0; i < c->len; i++)
		ss_res_free(c->r[i]);
	free(c->r);
	free(c);
}

int hit, miss;
static ss_res_t * sa_c_get(attacker_t A, defender_t D, sa_cache_t *c)
{
	ss_res_t * r = c->r[A->n * c->a_step + D->n * c->d_step + D->hp_remaining/c->hp_delta];
	r ? hit++ : miss++;
	return r;
}

static void sa_c_put(attacker_t A, defender_t D, ss_res_t *r, sa_cache_t *c)
{
	c->r[A->n * c->a_step + D->n * c->d_step + D->hp_remaining/c->hp_delta] = r;
	//printf("PUT: Na %i, Nd %i, hp %i\n", A->n, D->n, D->hp_remaining);
}

// Transfers ownership of the ss_res memory to the caller
static void sa_c_detach(const ss_res_t const *ss_res, sa_cache_t *c)
{
	int i;
	for (i = c->len-1; i >= 0; --i)
		if (c->r[i] == ss_res) {
			c->r[i] = NULL;
			return;
		}
}

static ss_res_t * sim_attacks(attacker_t A, defender_t D, sa_cache_t *c)
{
	ss_res_t *r_out, *r1, *r2;
	int Dn_in, hp_in, i, n;
	double p;
	//printf("Na: %i, Nd: %i, hp: %i\n", A->n, D->n, D->hp_remaining);

	if ((r_out = sa_c_get(A, D, c)))
		return r_out;

	r_out = new_ss_res(A, D);
	if (D->n == 0) {
		r_out->A[A->n] = 1;
		sa_c_put(A, D, r_out, c);
		return r_out;
	}
	if (A->n == 0) {
		r_out->D[D->n * r_out->D_hp_step + D->hp_remaining/r_out->hp_delta] = 1;
		sa_c_put(A, D, r_out, c);
		return r_out;
	}
	hp_in = D->hp_remaining;
	Dn_in = D->n;
	A->n--;

	if (hp_in <= A->dmg_min) {
		D->n = Dn_in - 1;
		D->hp_remaining = D->n ? D->hp : 0;
		r1 = sim_attacks(A, D, c);
		memcpy(r_out->A, r1->A, (A->n+1) * sizeof(double));
		memcpy(r_out->D, r1->D, (D->n+1) * r1->D_hp_step * sizeof(double));
		goto sa_out;
	} else {
		D->n = Dn_in;
		D->hp_remaining = hp_in - A->dmg_min;

		r1 = sim_attacks(A, D, c);
		p = 1 - A->accuracy;
		n = A->n+1;
		memcpy(r_out->A, r1->A, n * sizeof(double));
		for (i = 0, n = A->n+1; i < n; i++)
			if (r_out->A[i])
				r_out->A[i] *= p;
		n = (D->n+1)*r1->D_hp_step;
		memcpy(r_out->D, r1->D, n * sizeof(double));
		for (i = 0; i < n; i++)
			if (r_out->D[i])
				r_out->D[i] *= p;
	}
	
	if (hp_in > A->dmg_max) {
		D->n = Dn_in;
		D->hp_remaining = hp_in - A->dmg_max;
	} else {
		D->n = Dn_in - 1;
		D->hp_remaining = D->n ? D->hp : 0;
	}
	r2 = sim_attacks(A, D, c);

	p = A->accuracy;
	for (i = 0, n = A->n+1; i < n; i++)
		if (r2->A[i])
			r_out->A[i] += r2->A[i] * p;
	for (i = 0, n = (D->n+1)*r2->D_hp_step; i < n; i++)
		if (r2->D[i])
			r_out->D[i] += r2->D[i] * p;

sa_out:
	A->n++;
	D->n = Dn_in;
	D->hp_remaining = hp_in;

	sa_c_put(A, D, r_out, c);

	return r_out;
}
