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

#endif