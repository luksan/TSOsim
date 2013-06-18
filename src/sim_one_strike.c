#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

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

struct plist_elem {
	int dmg;
	int killed_defenders;
	double p;
	struct plist_elem *next;
};

struct plist {
	int len;
	struct plist_elem **l;
};

typedef struct plist *plist_t;

double outcome_prob(int hit, int miss, attacker_t A, int hp);
plist_t kill_one_defender(attacker_t A, defender_t D);

typedef unsigned long ULONG;
ULONG binomial(int n, int k);


int sim_sub(attacker_t A, defender_t D, double p)
{
	
}

plist_t new_plist(int len)
{
	plist_t l = calloc(1, sizeof(struct plist));
	l->len = len;
	l->l = calloc(len+1, sizeof(struct plist_elem*));
	return l;
}

void update_counter(int n, int dmg, double p, plist_t plist)
{
	struct plist_elem *prev=NULL, *curr, *new;
	for (curr = plist->l[n]; curr && curr->dmg < dmg; prev = curr, curr = curr->next);
	if (curr && curr->dmg == dmg) {
		curr->p += p;
		return;
	} 
	new = calloc(1, sizeof(struct plist_elem));
	new->p = p;
	new->dmg = dmg;
	if (prev)
		prev->next = new;
	else
		plist->l[n] = new;
	new->next = curr;
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

typedef struct {
	int n;
	double p;
} p_term;

static int cmp_terms(const void *p1, const void *p2)
{
	return ((p_term*)p1)->n - ((p_term*)p2)->n;
}

static int sure_kills(attacker_t A, defender_t D)
{
	return min(D->n, A->n/min_attackers(A->dmg_min, D->hp));
}
/*
 * Remove defenders that will die for sure, due to number of attackers
 * Return list of {attackers remaining, prob}
 */
p_term * kill_defenders(attacker_t A, defender_t D)
{
	plist_t kill_prob;
	int dloss, i,j, p1_i, p2_i, p_len, k_len = 0;
	
	p_term *p0, *p1, *p2, *pt;
	dloss = sure_kills(A, D);
	kill_prob = kill_one_defender(A, D);
	if (!kill_prob)
		return NULL;
	p_len = binomial(dloss+k_len-1, dloss)+1;
	p0 = calloc(kill_prob->len, sizeof(p_term));
	p1 = calloc(p_len, sizeof(p_term));
	p2 = calloc(p_len, sizeof(p_term));
	for (i = 0; i < kill_prob->len; i++) {
		struct plist_elem *pe;
		if (pe = kill_prob->l[i]) {
			p0[k_len].n = p1[k_len].n = i;
			p0[k_len].p = p1[k_len].p = pe->p;
			k_len++;
		}
	}	
	for (i = 0; i < dloss; i++) { // multiply polynomials
		p2_i = 0;
		for (p1_i = 0; p1[p1_i].n > 0; p1_i++) {
			for (j = 0; j < k_len; j++) {
				p2[p2_i].n = p0[j].n + p1[p1_i].n;
				p2[p2_i].p = p0[j].p * p1[p1_i].p;
				p2_i++;
			}
		}
		pt = p1; p1 = p2; p2 = pt; // swap p1 and p2
		memset(p2, 0, p_len);
	}
	p_len = p2_i;
	qsort(p1, p1_i, sizeof(p_term), cmp_terms); // sort and merge terms with same number of attacker losses
	p2[0] = p1[0];
	for (p1_i = 1, p2_i = 1; p1_i < p_len; p1_i++) {
		if (p2[p2_i].n == p1[p1_i].n)
			p2[p2_i].p += p1[p1_i].p;
		else {
			p2[p2_i].n = p1[p1_i].n;
			p2[p2_i++].p = p1[p1_i].p;
		}
	}
	for (p2_i = 0; p2[p2_i].n > 0; p2_i++)
		p2[p2_i].n = A->n - p2[p2_i].n; // convert from A_loss to A_remaining
	return p2;
}

/* How many attackers are lost after killing one defender */
plist_t kill_one_defender(attacker_t A, defender_t D)
{
	int Na_min = min_attackers(A->dmg_min, D->hp);
	plist_t plist;
	if (Na_min > A->n)
		return NULL;
	plist = new_plist(Na_min);
	attack_one_defender1(Na_min, A, D, plist);
	return plist;
}
	
plist_t attack_one_defender(attacker_t A, defender_t D)
{
	int Na_min, i;
	Na_min = min(min_attackers(A->dmg_min, D->hp), A->n);
	plist_t plist = new_plist(Na_min);
	attack_one_defender1(Na_min, A, D, plist);
	return plist;
}

static inline int is_kill(int hit, int miss, attacker_t A, int hp)
{
	return hit*A->dmg_max + miss*A->dmg_min >= hp;
}

int attack_one_defender1(int n_sure_kill, attacker_t A, defender_t D, plist_t plist)
{
	int hit = 0, dmg, hp, miss;
	double p;
	hp = D->hp;
	miss = n_sure_kill;
	while (hit <= n_sure_kill) {
		if (hit + miss > n_sure_kill) {
			miss--;
			continue;
		}
		dmg = hit*A->dmg_max, + miss*A->dmg_min;
		p = outcome_prob(hit, miss, A, hp);
		if (dmg < hp) {
			if (hit + miss == A->n)
				update_counter(hit+miss, dmg, p, plist);
			hit++;
		} else {
			update_counter(hit+miss, 0, p, plist);
			if (miss == 0)
				return 1;
			miss--;
		}
	}
	return 1;
}

/* the natural logarithm of the binomial coefficient N over K */
double ln_binomial(double N, double K)
{
	return lgamma(N+1)-lgamma(K+1)-lgamma(N-K+1);
}

ULONG binomial(int n, int k)
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

ULONG multinomial(int n, int k[], int k_len)
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

double outcome_prob(int hit, int miss, attacker_t A, int hp)
{
	int perm_last = 0;
	double B, H, M;
	if (miss && is_kill(hit, miss-1, A, hp))
		perm_last = 1;
	B = ln_binomial(hit+miss-perm_last, hit>0?hit-perm_last:0);
	H = hit*log(A->accuracy);
	M = miss*log(1 - A->accuracy);
	return exp(B+H+M);
}

int main(int argc, char **argv)
{
	return 0;
}

