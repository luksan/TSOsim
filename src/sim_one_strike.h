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
	double * A;
	double * D;
	int hp_delta;
	int D_hp_step;
} ss_res_t;

ss_res_t * new_ss_res(attacker_t A, defender_t D);
void ss_res_free(ss_res_t *s);

ss_res_t * sim_one_strike_kill_defenders(attacker_t A, defender_t D);
plist_t * attack_one_defender(attacker_t A, defender_t D);

/* sim_attacks() */
typedef struct {
	int a_step;
	int d_step;
	int hp_delta;
	int len;
	ss_res_t ** r;
} sa_cache_t;

ss_res_t * sim_one_strike_attacks(attacker_t A, defender_t D);

#endif
