#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>

#include "plist.h"

struct attacker {
	int n;
	int dmg_min;
	int dmg_max;
	double accuracy;
};
typedef struct attacker *attacker_t;

struct defender {
	int n;
	int hp;
	int hp_remaining;
};
typedef struct defender *defender_t;

#define SWAP_PTRS(a, b) do { void *t = (a); (a) = (b); (b) = t; } while (0)

/* function declarations */
//double outcome_prob(int hit, int miss, attacker_t A, int hp);
parray_t kill_one_defender(attacker_t A, defender_t D);
parray_t attack_one_defender(attacker_t A, defender_t D);
plist_t * kill_defenders(attacker_t A, defender_t D);
static plist_t * for_each_sub_sim(plist_t *p1, attacker_t A, defender_t D, double p_in);
static int sure_kills(attacker_t A, defender_t D);
static int attack_one_defender1(int n_sure_kill, attacker_t A, defender_t D, parray_t parr);
plist_t * sim_sub(attacker_t A, defender_t D, double p_in);

typedef unsigned long ULONG;
static ULONG binomial(int n, int k);

/* probability calculations */

plist_t * sim(attacker_t A, defender_t D)
{
	return sim_sub(A, D, 1);
}

plist_t * sim_sub(attacker_t A, defender_t D, double p_in)
{
	int sk, i;
	plist_t *p1, *p2, *p3, *p_out = NULL;
	if (A->n == 0 || D->n == 0) {
		p_out = new_plist();
		set_p_element(p_out, A->n, D->n, D->hp_remaining, p_in, NULL);
		return p_out;
	}
	sk = sure_kills(A, D);
	if (sk == 0) { // no sure kills remaining
		parray_t pa = attack_one_defender(A, D);
		for (i = 0; i < pa->len; i++) {
			if (!pa->a[i]) // uninitialized element
				continue;
			p_out = plist_merge(p_out, for_each_sub_sim(pa->a[i], A, D, p_in));
		}
		return p_out;
	}
	p1 = kill_defenders(A, D);
	for (p2 = p1; p2; p2 = p2->next)
		p2->p *= p_in;
	if (sk == D->n) //all defenders are sure to be killed
		return p1;
	return for_each_sub_sim(p1, A, D, 1);
}

static plist_t * for_each_sub_sim(plist_t *p1, attacker_t A, defender_t D, double p_in)
{
	int Na_in, Nd_in, hp_in;
	plist_t *p_out = NULL;
	Na_in = A->n;
	Nd_in = D->n;
	hp_in = D->hp_remaining;
	for (; p1; p1 = p1->next) {
		A->n = p1->Na;
		D->n = p1->Nd;
		D->hp_remaining = p1->hp_remaining;
		p_out = plist_merge(sim_sub(A,D, p_in*p1->p), p_out);
	}
	A->n = Na_in;
	D->n = Nd_in;
	D->hp_remaining = hp_in;
	return p_out;
}

static int min_attackers(int dmg_min, int hp)
{
	if (dmg_min == 0)
		return 500000;
	return ceil(hp/(float)dmg_min);
}
static int min(int a, int b)
{
	if (a < b)
		return a;
	return b;
}

static int sure_kills(attacker_t A, defender_t D)
{
	return min(D->n, A->n/min_attackers(A->dmg_min, D->hp));
}

typedef struct { // used in kill_defenders()
	int n;
	double p;
} p_term;
// sort biggest first
static int cmp_terms(const void *p1, const void *p2)
{
	return ((p_term*)p2)->n - ((p_term*)p1)->n;
}

/*
 * Remove defenders that will die for sure, due to number of attackers
 * Return list of {attackers remaining, prob}
 */
plist_t * kill_defenders(attacker_t A, defender_t D)
{
	parray_t kill_prob;
	int dloss, i,j, p1_i, p2_i, p0_len, p1_len, k_len;
	
	p_term *p0, *p1, *p2, *pt;
	dloss = sure_kills(A, D);
	kill_prob = kill_one_defender(A, D);
	if (!kill_prob)
		return NULL;
	p0 = calloc(kill_prob->len, sizeof(p_term));
	p1 = calloc(kill_prob->len, sizeof(p_term));
	for (i = 0, p0_len = 0; i < kill_prob->len; i++) {
		plist_t *pe;
		if (pe = kill_prob->a[i]) {
			p0[p0_len].n = p1[p0_len].n = i;
			p0[p0_len].p = p1[p0_len].p = pe->p;
			p0_len++;
		}
	}
	p1_len = binomial(dloss+k_len-1, dloss)+1;
	p1 = realloc(p1, p1_len*sizeof(p_term));
	p1[p0_len].n = 0;
	p2 = calloc(p1_len, sizeof(p_term));
	for (i = 1; i < dloss; i++) { // multiply polynomials
		p2_i = 0;
		for (p1_i = 0; p1[p1_i].n > 0; p1_i++) {
			for (j = 0; j < k_len; j++) {
				p2[p2_i].n = p0[j].n + p1[p1_i].n;
				p2[p2_i].p = p0[j].p * p1[p1_i].p;
				p2_i++;
			}
		}
		SWAP_PTRS(p1, p2);
		memset(p2, 0, p1_len);
	}
	p1_len = p2_i;
	qsort(p1, p1_len, sizeof(p_term), cmp_terms); // sort and merge terms with same number of attacker losses
	p2[0] = p1[0];
	for (p1_i = 1, p2_i = 0; p1_i < p1_len; p1_i++) {
		if (p2[p2_i].n == p1[p1_i].n)
			p2[p2_i].p += p1[p1_i].p;
		else {
			p2[++p2_i].n = p1[p1_i].n;
			p2[p2_i].p = p1[p1_i].p;
		}
	}
	plist_t *p_out = NULL, *new, *prev = NULL;
	for (p2_i = 0; p2[p2_i].n > 0; p2_i++) {
		new = new_plist();
		set_p_element(new, A->n - p2[p2_i].n, 0, 0, p2[p2_i].p, NULL);
		plist_append(prev, new);
		prev = new;
	}
	return p_out;
}

/* How many attackers are lost after killing one defender */
parray_t kill_one_defender(attacker_t A, defender_t D)
{
	int Na_min = min_attackers(A->dmg_min, D->hp);
	int hpr = D->hp_remaining;
	int i;
	parray_t parr;
	if (Na_min > A->n)
		return NULL;

	parr = new_parray(Na_min);
	D->hp_remaining = D->hp;
	attack_one_defender1(Na_min, A, D, parr);
	D->hp_remaining = hpr;
	for (i = 0; i < parr->len; i++) {
		if (!parr->a[i])
			continue;
		if (parr->a[i]->next != NULL)
			return NULL; // we should only get kills, no survivors
	}
	return parr;
}
	
parray_t attack_one_defender(attacker_t A, defender_t D)
{
	int Na_min, i;
	plist_t *p;
	Na_min = min(min_attackers(A->dmg_min, D->hp), A->n);
	parray_t pa = new_parray(Na_min);
	attack_one_defender1(Na_min, A, D, pa);
	print_parray(pa);
	printf("%i, %i\n", A->n, D->n);
	for (i = 0; i < pa->len; i++)
		for (p = pa->a[i]; p; p = p->next) {
			p->Na = A->n - p->Na;
			p->Na = D->n - p->Nd;
		}
	print_parray(pa);
	return pa;
}

static inline int is_kill(int hit, int miss, attacker_t A, int hp)
{
	return hit*A->dmg_max + miss*A->dmg_min >= hp;
}

/* the natural logarithm of the binomial coefficient N over K */
static double ln_binomial(double N, double K)
{
	return lgamma(N+1)-lgamma(K+1)-lgamma(N-K+1);
}

static double outcome_prob(int hit, int miss, attacker_t A, int hp)
{
	int perm_last = 0;
	double B, H, M;
	if (miss && is_kill(hit, miss-1, A, hp))
		perm_last = 1;
	B = ln_binomial(hit+miss-perm_last, hit > 0 ? hit-perm_last : 0);
	H = hit*log(A->accuracy);
	M = miss*log(1 - A->accuracy);
	return exp(B+H+M);
}

static int attack_one_defender1(int n_sure_kill, attacker_t A, defender_t D, parray_t parr)
{
	int hit = 0, dmg, hp, miss;
	double p;
	hp = D->hp_remaining;
	miss = n_sure_kill;
	while (hit <= n_sure_kill) {
		if (hit + miss > n_sure_kill) {
			miss--;
			continue;
		}
		dmg = hit*A->dmg_max, + miss*A->dmg_min;
		p = outcome_prob(hit, miss, A, hp);
		if (dmg < hp) {
			if (hit + miss == n_sure_kill)
				parray_incr_p(hit+miss, 0, hp - dmg, p, parr);
			hit++;
		} else {
			parray_incr_p(hit+miss, 1, D->hp, p, parr);
			if (miss == 0)
				return 1;
			miss--;
		}
	}
	return 1;
}

static ULONG binomial(int n, int k)
{
	ULONG r = 1, d = n - k;
	
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
	for(;p;p = p->next)
		printf("%i, %i, %i, %.2f\n",
			p->Na,
			p->Nd,
			p->hp_remaining,
			p->p);
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
	pa = attack_one_defender(&A, &D);
	print_parray(pa);
	printf("OK\n");
	//p = sim(&A, &D);
	//p = kill_defenders(&A, &D);
	//print_plist(p);
	return 0;
}

