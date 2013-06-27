#ifndef sim_one_strike
#define sim_one_strike

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

plist_t * sim(attacker_t A, defender_t D);

/* sim_sub() */
typedef struct ss_res_ {
	double * restrict A;
	double * restrict D;
	int hp_delta;
	int D_hp_step;
} ss_res_t;

void sim_sub(attacker_t A, defender_t D, double p_in, const struct ss_res_ *ss_res);

/* sim_attacks() */
typedef struct {
	int a_step;
	int d_step;
	int hp_delta;
	int len;
	ss_res_t ** restrict r;
} sa_cache_t;

ss_res_t * sim_attacks(attacker_t A, defender_t D, sa_cache_t *c);

sa_cache_t * sa_c_new(attacker_t A, defender_t D);
void sa_c_free(sa_cache_t *c);

#endif
