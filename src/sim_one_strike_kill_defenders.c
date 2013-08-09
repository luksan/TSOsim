#include <limits.h>
#include <string.h>
#include <math.h>

#include "sim_one_strike.h"

#define SWAP_PTRS(a, b) do { void *t = (a); (a) = (b); (b) = t; } while (0)

static parray_t kill_one_defender(attacker_t A, defender_t D);
static void sim_sub(attacker_t A, defender_t D, double p_in, const struct ss_res_ *ss_res);
static plist_t * kill_defenders(attacker_t A, defender_t D);
static void for_each_sub_sim(plist_t *p1, attacker_t A, defender_t D, double p_in, const struct ss_res_ *ss_res);
static int sure_kills(attacker_t A, defender_t D);
static void attack_one_defender1(const int n_sure_kill, attacker_t A, const int hp, parray_t parr);

typedef unsigned long ULONG;
static ULONG binomial(int n, int k);

ss_res_t * sim_one_strike_kill_defenders(attacker_t A, defender_t D)
{
	ss_res_t *ss_res = new_ss_res(A, D);
	sim_sub(A, D, 1, ss_res);
	return ss_res;
}

static inline int min(int a, int b)
{
	return a < b ? a : b;
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
	if (sk == 0 || D->hp != D->hp_remaining) { // no sure kills remaining
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

static void poly_mul(double a[], const int a_start, const int a_len,
		     const double b[], const int b_start, const int b_len)
{
	int x,y, r_len = a_len + b_len - 1;
	double res[r_len];
	memset(res, 0, r_len*sizeof(double));
	for (x = a_start; x < a_len; ++x)
		for (y = b_start; y < b_len; ++y)
			res[x+y] += a[x] * b[y];
	memcpy(a, res, r_len*sizeof(double));
}

static void poly_square(double p[], const int p_start, const int p_len)
{
	int a, b;
	const int r_len = p_len*2 - 1;
	double pt, res[r_len];
	memset(res, 0, r_len*sizeof(double));
	for (a = p_start; a < p_len; ++a) {
		res[a<<1] += p[a]*p[a];
		pt = p[a]*2;
		for (b = a+1; b < p_len; ++b)
			res[a+b] += pt*p[b];
	}
	memcpy(p, res, r_len*sizeof(double));
}

static void poly_pow(const p_term p[], const int p_len, int b, double result[], int *result_len)
{
	const int alloc = p[p_len-1].n*b+1;
	double a[alloc];
	int res_start, a_start, res_len, a_len, i;

	a_start = res_start = p[0].n;
	for (i = 0; i < p_len; ++i)
		result[p[i].n] = p[i].p;
	a_len = res_len = p[p_len-1].n+1;
	if (--b)
		memcpy(a, result, alloc*sizeof(double));
	while (b) {
		if (b&1) {
			poly_mul(result, res_start, res_len, a, a_start, a_len);
			res_start = res_start + a_start;
			res_len = res_len + a_len - 1;
		}
		if ((b >>= 1)) {
			poly_square(a, a_start, a_len);
			a_start <<= 1;
			a_len = a_len * 2 - 1;
		}
	}
	*result_len = res_len;
}

/*
 * Remove defenders that will die for sure, due to number of attackers
 * Return list of {attackers remaining, prob}
 */
plist_t * kill_defenders(attacker_t A, defender_t D)
{
	int dloss, p1_i, p0_len, p1_len;
	p_term *p0;

	dloss = sure_kills(A, D);
	if (!dloss)
		return NULL;

	if (kill_defenders_get_p0(A, D, &p0, &p0_len) < 0)
		return NULL;

	const int alloc = p0[p0_len-1].n * dloss + 1;
	double p1[alloc];
	memset(p1, 0, alloc*sizeof(double));

	poly_pow(p0, p0_len, dloss, p1, &p1_len);

	plist_t *p_out = NULL;
	for (p1_i = 0; p1_i < p1_len; p1_i++) {
		if (p1[p1_i] == 0)
			continue;
		int Nd = D->n - dloss;
		p_out = set_p_element(new_plist(), A->n - p1_i, Nd, Nd ? D->hp : 0, p1[p1_i], p_out);
	}
	//free(p0); it's cached

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

plist_t * attack_one_defender(attacker_t A, defender_t D)
{
	int Na_min;
	plist_t *p, *out;

	Na_min = min(min_attackers(A->dmg_min, D->hp_remaining), A->n);
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

#if 0
/* the natural logarithm of the binomial coefficient N over K */
static double ln_binomial(int N, int K)
{
	//return lgamma(N+1)-lgamma(K+1)-lgamma(N-K+1);
	return log(binomial(N, K));
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
#endif
