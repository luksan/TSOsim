#include <math.h>

struct attacker {
	int n;
	int dmg_min;
	int dmg_max;
	double accuracy;
};
typedef struct attacker* attacker_t;

struct defender {
	int n;
	int hp;
	int hp_remaining;
};
typedef struct defender* defender_t;

struct plist_elem {
	int dmg;
	int killed_defenders;
	double p;
	struct plist_elem *next;
};

struct plist {
	int len;
	struct plist_elem l[];
};

typedef struct *plist_elem[] plist_t;

pdict_t new_plist(int len)
{
	return calloc(n, plist_t);
}

void update_counter(int n, int dmg, double p, plist_t plist)
{
	struct plist_elem *prev=NULL, *curr, *new;
	for (curr = plist[n]; curr && curr->dmg < dmg; prev = curr, curr = curr->next);
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
		plist[n] = new;
	new->next = curr;
}

static int min_attackers(int dmg_min, int hp)
{
	if (dmg_min == 0)
		return 500000;
	return ceil(hp/float(dmg_min));
}
static int min(int a, int b)
{
	if (a < b)
		return a;
	return b;
}

void attack_one_defender(attacker_t A, defender_t D)
{
	int Na_min, i;
	Na_min = min(min_attackers(A->dmg_min, D->hp), A->n);
	pdict_t plist = new_plist(Na_min);
	attack_one_defender1(Na_min, A, D, plist);
	for (i = 0; i < Na_min, i++) {



static inline int is_kill(int hit, int miss, attacker_t A, int hp)
{
	return hit*A->dmg_max + miss*A->dmg_min >= hp;
}

int attack_one_defender1(int miss, attacker_t A, defender_t D, struct plist_elem plist[])
{
	int hit = 0, dmg, hp;
	double p;
	hp = D->hp;
	while (hit <= A->n) {
		if (hit + miss > A->n) {
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
double ln_binom(double N, double K)
{
	return lgamma(N+1)-lgamma(K+1)-lgamma(N-K+1);
}

double outcome_prob(int hit, int miss, attacker_t A, int hp)
{
	int perm_last = 0;
	double B, H, M;
	if (miss && is_kill(hit, miss-1, A, hp))
		perm_last = 1;
	B = ln_binom(hit+miss-perm_last, hit>0?hit-perm_last:0);
	H = hit*log(A->accuracy);
	M = miss*log(1 - A->accuracy);
	return exp(B+H+M);
}

int main(int argc, char **argv)
{
	return 0;
}
