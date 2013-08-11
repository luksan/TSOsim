#ifndef sim_one_strike
#define sim_one_strike

#include "plist.h"

#define MAX_UNITS 250

struct kd_cache;
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
	int hp_delta;
	int D_hp_step;
	int A_start;
	int A_len;
	int D_start;
	int D_len;
	double *A;
	double D[];
} ss_res_t;

ss_res_t * new_ss_res(attacker_t A, defender_t D);
void ss_res_free(ss_res_t *s);

ss_res_t * sim_one_strike_kill_defenders(attacker_t A, defender_t D);

struct kd_cache;
struct kd_cache * kd_cache_new();
void kd_cache_free(struct kd_cache *c);

plist_t * kill_defenders(attacker_t A, defender_t D, struct kd_cache* c);

/* sim_attacks() */
typedef struct {
	int a_step;
	int d_step;
	int hp_delta;
	int len;
	ss_res_t ** r;
	struct kd_cache * kd_c;
} sa_cache_t;

ss_res_t * sim_one_strike_attacks(attacker_t A, defender_t D);

#endif
