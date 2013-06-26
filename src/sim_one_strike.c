#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <sys/stat.h>

#include "sim_one_strike.h"

#define SWAP_PTRS(a, b) do { void *t = (a); (a) = (b); (b) = t; } while (0)

typedef struct ss_res_ {
	double * restrict A;
	double * restrict D;
	int hp_delta;
	int D_hp_step;
} ss_res_t;

typedef struct {
	int a_step;
	int d_step;
	int hp_delta;
	int len;
	ss_res_t ** restrict r;
} sa_cache_t;

/* function declarations */

static parray_t kill_one_defender(attacker_t A, defender_t D);
static plist_t * attack_one_defender( attacker_t A, defender_t D );
static plist_t * kill_defenders(attacker_t A, defender_t D);
static void for_each_sub_sim(plist_t *p1, attacker_t A, defender_t D, double p_in, const struct ss_res_ *ss_res);
static int sure_kills(attacker_t A, defender_t D);
static void attack_one_defender1(const int n_sure_kill, attacker_t A, const int hp, parray_t parr);
static void sim_sub(attacker_t A, defender_t D, double p_in, const struct ss_res_ *ss_res);
static ss_res_t * sim_attacks(attacker_t A, defender_t D, sa_cache_t *c);
static inline int min(int a, int b);

typedef unsigned long ULONG;
static ULONG binomial(int n, int k);

/* probability calculations */

static int min_hp_change(attacker_t A)
{
	if (!A->dmg_min || A->dmg_min == A->dmg_max) return A->dmg_max;
	return min(A->dmg_min, A->dmg_max - A->dmg_min);
}

static ss_res_t * new_ss_res(attacker_t A, defender_t D)
{
	ss_res_t * ss_res = malloc(sizeof(ss_res_t));
	ss_res->hp_delta = min_hp_change(A);
	ss_res->D_hp_step = D->hp/ss_res->hp_delta + 1;
	ss_res->A = calloc(A->n+1, sizeof(double));
	ss_res->D = calloc((D->n+1) * ss_res->D_hp_step, sizeof(double));
	return ss_res;
}

static void ss_res_free(ss_res_t *s)
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

static void sa_c_free(sa_cache_t *c)
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

plist_t * sim(attacker_t A, defender_t D)
{
	int i, j;
	plist_t *ret, *p, **pnext;
	sa_cache_t *c;
	
	ss_res_t *ss_res;
	/*
	ss_res = new_ss_res(A, D);
	sim_sub(A, D, 1, ss_res);
*/
	c = sa_c_new(A, D);
	ss_res = sim_attacks(A, D, c);

	pnext = &ret;
	for (i = A->n; i >= 0; i--) {
		if (ss_res->A[i] == 0)
			continue;
		*pnext = p = new_plist();
		pnext = &p->next;
		p->Na = i;
		p->Nd = 0;
		p->p = ss_res->A[i];
		p->hp_remaining = 0;
	}

	for (i = 0; i <= D->n; i++) {
		for (j = 0; j < ss_res->D_hp_step; j++){
			if (!ss_res->D[i*ss_res->D_hp_step+j])
				continue;
			*pnext = p = new_plist();
			pnext = &p->next;
			p->Na = 0;
			p->Nd = i;
			p->p = ss_res->D[i*ss_res->D_hp_step+j];
			p->hp_remaining = D->hp - (ss_res->D_hp_step-j-1)*ss_res->hp_delta;
		}
	}
	*pnext = NULL;
	//ss_res_free(ss_res);
	sa_c_free(c);
	return ret;
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

	if (hp_in < A->dmg_min) {
		D->n = Dn_in - 1;
		D->hp_remaining = D->n ? D->hp : 0;
		r1 = sim_attacks(A, D, c);
		memcpy(r_out->A, r1->A, (A->n+1) * sizeof(double));
		memcpy(r_out->D, r1->D, (D->n+1) * r1->D_hp_step * sizeof(double));
		goto sa_out;
	}
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

static void sim_sub(attacker_t A, defender_t D, double p_in, const struct ss_res_ *ss_res)
{
	int sk;
	plist_t *p1 = NULL, *p2;
	if (D->n == 0) {
		ss_res->A[A->n] += p_in;
		return;
	}
	if (A->n == 0) {
		ss_res->D[D->n*ss_res->D_hp_step + D->hp_remaining/ss_res->hp_delta] += p_in;
		return;
	}

	sk = sure_kills(A, D);
	if (sk == 0) { // no sure kills remaining
		p1 = attack_one_defender(A, D);
		for_each_sub_sim(p1, A, D, p_in, ss_res);
		return;
	}
	p1 = kill_defenders(A, D);
	if (sk == D->n) { //all defenders are sure to be killed
		for (p2 = p1; p2; p2 = p2->next)
			ss_res->A[p2->Na] += p2->p*p_in;
		plist_free(p1);
	} else
		for_each_sub_sim(p1, A, D, p_in, ss_res);
}

static void for_each_sub_sim(plist_t *p1, attacker_t A, defender_t D, double p_in, const struct ss_res_ *ss_res)
{
	int Na_in, Nd_in, hp_in;
	Na_in = A->n;
	Nd_in = D->n;
	hp_in = D->hp_remaining;
	for (; p1; p1 = free_p_element(p1)) {
		A->n = p1->Na;
		D->n = p1->Nd;
		D->hp_remaining = p1->hp_remaining;
		sim_sub(A, D, p_in*p1->p, ss_res);
	}
	A->n = Na_in;
	D->n = Nd_in;
	D->hp_remaining = hp_in;
}

static inline int min_attackers(int dmg_min, int hp)
{
	return dmg_min ? (hp + dmg_min - 1) / dmg_min : INT_MAX; // ceil(a/b)
}

static inline int min(int a, int b)
{
	return a < b ? a : b;
}
static inline int max(int a, int b)
{
	return a > b ? a : b;
}

static inline int sure_kills(const attacker_t A, const defender_t D)
{
	return min(D->n, A->n/min_attackers(A->dmg_min, D->hp));
}

typedef struct { // used in kill_defenders()
	int n;
	double p;
} p_term;


static int kill_defenders_get_p0(attacker_t A, defender_t D, p_term **p0, int *p0_len)
{
	parray_t kill_prob;
	int i;

	static int dmg_max, dmg_min, hp;
	static double accuracy;
	static int cache_len;
	static p_term *cache;
	if (A->dmg_max == dmg_max && A->dmg_min == dmg_min &&
		A->accuracy == accuracy && D->hp == hp && cache) {
		*p0 = cache;
		*p0_len = cache_len;
		return 0;
	}
	free(cache);
	kill_prob = kill_one_defender(A, D);
	if (!kill_prob)
		return -1;
	*p0 = malloc(kill_prob->len * sizeof(p_term));
	for (i = 0, *p0_len = 0; i < kill_prob->len; i++) {
		plist_t *pe;
		if ((pe = kill_prob->a[i])) {
			(*p0)[*p0_len].n = pe->Na;
			(*p0)[*p0_len].p = pe->p;
			(*p0_len)++;
		}
	}
	parray_free(kill_prob);
	free(cache);
	cache = *p0;
	cache_len = *p0_len;
	dmg_max = A->dmg_max;
	dmg_min = A->dmg_min;
	hp = D->hp;
	accuracy = A->accuracy;
	return 0;
}

/*
 * Remove defenders that will die for sure, due to number of attackers
 * Return list of {attackers remaining, prob}
 */
plist_t * kill_defenders(attacker_t A, defender_t D)
{
	int dloss, i,j, p1_i, p2_start, p0_len, p_alloc;
	p_term *p0;
	double *p1, *p2;
	dloss = sure_kills(A, D);
	if (!dloss)
		return NULL;

	if (kill_defenders_get_p0(A, D, &p0, &p0_len) < 0)
		return NULL;

	p_alloc = p0[p0_len-1].n*dloss + 1;
	p1 = calloc(p_alloc, sizeof(double));
	p2 = malloc(p_alloc*sizeof(double));
	for (i = 0; i < p0_len; i++)
		p1[p0[i].n] = p0[i].p;

	p2_start = p0[0].n;
	for (i = 1; i < dloss; i++) { // multiply polynomials
		memset(p2, 0, p_alloc*sizeof(double));
		for (p1_i = p2_start; p1_i < p_alloc && p1[p1_i]; p1_i++)
			for (j = 0; j < p0_len; j++)
				p2[p1_i+p0[j].n] += p0[j].p * p1[p1_i];
		p2_start += p0[0].n;
		SWAP_PTRS(p1, p2);
	}
	plist_t *p_out = NULL;
	for (p1_i = 0; p1_i < p_alloc; p1_i++) {
		if (p1[p1_i] == 0)
			continue;
		int Nd = D->n - dloss;
		p_out = set_p_element(new_plist(), A->n - p1_i, Nd, Nd ? D->hp : 0, p1[p1_i], p_out);
	}
	//free(p0); it's cached
	free(p1);
	free(p2);
	return p_out;
}

/* How many attackers are lost after killing one defender */
static parray_t kill_one_defender(attacker_t A, defender_t D)
{
	int Na_min = min_attackers(A->dmg_min, D->hp);
	int i;
	parray_t parr;
	if (Na_min > A->n)
		return NULL;

	parr = new_parray(Na_min);
	attack_one_defender1(Na_min, A, D->hp, parr);
	for (i = 0; i < parr->len; i++) {
		if (!parr->a[i])
			continue;
		if (parr->a[i]->next != NULL)
			return NULL; // we should only get kills, no survivors
	}
	return parr;
}

static int calls;

typedef struct x_ {
	int hp;
	int na_min;
	attacker_t A;
	plist_t *p;
	struct x_ *next;
} aod_cache_t;

static aod_cache_t *aod_cache;

static plist_t * aod_cache_get(int na_min, attacker_t A, int hp)
{
	aod_cache_t *a;
	for (a = aod_cache; a; a = a->next)
		if (a->A == A && a->hp == hp && a->na_min == na_min)
			return plist_copy(a->p);
	return NULL;
}

static void aod_cache_put(int na_min, attacker_t A, int hp, plist_t *p)
{
	aod_cache_t *n = malloc(sizeof(aod_cache_t));
	n->hp = hp;
	n->na_min = na_min;
	n->A = A;
	n->p = plist_copy(p);
	n->next = aod_cache;
	aod_cache = n;
}

static plist_t * attack_one_defender(attacker_t A, defender_t D)
{
	int Na_min;
	plist_t *p, *out;

	Na_min = min(min_attackers(A->dmg_min, D->hp), A->n);
	out = aod_cache_get(Na_min, A ,D->hp);
	if (!out) {
		parray_t pa = new_parray(Na_min);
		attack_one_defender1(Na_min, A, D->hp_remaining, pa);
		out = parray_to_plist(pa);
		aod_cache_put(Na_min, A, D->hp, out);
	}
	for (p = out; p; p = p->next) {
		p->Na = A->n - p->Na;
		p->Nd = D->n - p->Nd;
		if (p->Nd && p->Nd != D->n)
			p->hp_remaining = D->hp;
	}

	return out;
}

static inline int is_kill(int hit, int miss, attacker_t A, int hp)
{
	return hit*A->dmg_max + miss*A->dmg_min >= hp;
}

/* the natural logarithm of the binomial coefficient N over K */
static double ln_binomial(int N, int K)
{
	//return lgamma(N+1)-lgamma(K+1)-lgamma(N-K+1);
	return log(binomial(N, K));
}

static double outcome_prob(int hit, int miss, attacker_t A, int hp)
{
	int perm_last = 0;
	double B, H, M;
	if (miss && is_kill(hit, miss-1, A, hp))
		perm_last = 1;
	/*
	B = ln_binomial(hit+miss-perm_last, hit > 0 ? hit-perm_last : 0);
	H = hit*A->ln_acc;//log(A->accuracy);
	M = miss*A->ln_1m_acc;//(1 - A->accuracy);
	return exp(B+H+M);
	*/
	B = binomial(hit+miss-perm_last, hit > 0 ? hit-perm_last : 0);
	H = pow(A->accuracy, hit);
	M = pow(1 - A->accuracy, miss);
	return B*H*M;
}

static void attack_one_defender1(const int n_sure_kill, attacker_t A, const int hp, parray_t parr)
{
	int hit = 0, dmg, miss;
	double p;

	miss = n_sure_kill;
	if (hp != 40)
		printf("hp: %i\n", hp);
	while (hit <= n_sure_kill) {
		if (hit + miss > n_sure_kill)
			miss = n_sure_kill - hit;
		dmg = hit*A->dmg_max + miss*A->dmg_min;
		p = outcome_prob(hit, miss, A, hp);
		if (dmg < hp) {
			if (hit + miss == n_sure_kill)
				parray_incr_p(hit+miss, 0, hp - dmg, p, parr);
			hit++;
		} else {
			parray_incr_p(hit+miss, 1, 0, p, parr);
			if (miss == 0)
				return;
			miss--;
		}
	}
}

static ULONG binomial(int n, int k)
{
	ULONG r = 1, d = n - k;
	
	if (n==k || k == 0) return 1;
	if (n==0) return 0;
	if (k==1) return n;

	/* choose the smaller of k and n - k */
	if (d > k) { k = d; d = n - k; }
	
	while (n > k) {
		if (r >= UINT_MAX / n) return 0; /* overflown */
		r *= n--;
		
		/* divide (n - k)! as soon as we can to delay overflows */
		while (d > 1 && !(r % d)) r /= d--;
	}
	return r;
}

static ULONG multinomial(int n, int k[], int k_len)
{
	int i;
	ULONG ret = 1;
	int k_sum = 0;
	for (i=0; i < k_len; i++) {
		k_sum += k[i];
		ret *= binomial(k_sum, k[i]);
	}
	return ret;
}

void print_plist(plist_t *p)
{
	double p_tot = 0;
	int len = 0;
	for(;p;p = p->next) {
		p_tot += p->p;
		len++;
		if (len < 300)
		printf("%3i, %3i, %3i, %.3e\n",
			p->Na,
			p->Nd,
			p->hp_remaining,
			p->p);
	}
	printf("List length: %i, total prob: %f\n", len, p_tot);
}
void print_parray(parray_t pa)
{
	int i;
	for (i = 0; i < pa->len; i++)
		print_plist(pa->a[i]);
}

int main(int argc, char **argv)
{
	struct attacker A = {
		.n = 200,
		.dmg_min = 15,
		.dmg_max = 30,
		.accuracy = 0.8,
	};
	struct defender D = {
		.n = 100,
		.hp = 40,
		.hp_remaining = 40,
	};
	plist_t *p;
	parray_t pa;
	//pa = kill_one_defender(&A, &D);
	//pa = attack_one_defender(&A, &D);
	//print_parray(pa);
	for (int i = 0; i < 2; i++) {
		p = sim(&A, &D);
		//p = kill_defenders(&A, &D);
		//print_plist(p);
		plist_free(p);
	}
	printf("OK, hit %i, miss %i, hit%% %.2f\n", hit, miss, 100*(float)hit/(hit+miss));
	return 0;
}

