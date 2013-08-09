#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/stat.h>

#include "sim_one_strike.h"

/* function declarations */
void print_plist(plist_t *p);

/* probability calculations */

static inline int div_up(int a, int b)
{
	return (a+b-1)/b; // ceil(a/b)
}

double * mk_p_matrix(int size, double p)
{
	double *m = malloc(size*size*sizeof(double));
	int i, j;
	double p1, p2;
	for (i = 0, p1 = 1; i < size; i++, p1*=p)
		for (j = (size-i-1), p2 = pow(1-p, j); j < size; j++, p2*=(1-p))
			m[i*size+j] = p1*p2;
	return m;
}

#define SIM_KILL_DEFENDERS 1
ss_res_t * new_ss_res(attacker_t A, defender_t D);
void ss_res_free(ss_res_t *s);

plist_t * sim(attacker_t A, defender_t D)
{
	int i, j;
	plist_t *ret, *p, **pnext;
	
	ss_res_t *ss_res;
#if SIM_KILL_DEFENDERS
	ss_res = sim_one_strike_kill_defenders(A, D);
#else
	ss_res = sim_one_strike_attacks(A, D);
#endif
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
	ss_res_free(ss_res);
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
	p = sim(&A, &D);
	//sim_dmg_max(&A, &D);
	print_plist(p);
	for (int i = 0; i < 2; i++) {
		//p = sim(&A, &D);
		//p = kill_defenders(&A, &D);
		
		//plist_free(p);
	}
	printf("OK\n");
	return 0;
}

