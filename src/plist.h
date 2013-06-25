#ifndef PLIST_H
#define PLIST_H

#include <stdlib.h>

/* list and array data structures */
struct p_element {
	int Na;
	int Nd;
	int hp_remaining;
	double p;
	struct p_element *next;
};
typedef struct p_element plist_t;

struct parray {
	int len;
	struct p_element *a[];
};

typedef struct parray * parray_t;

plist_t * new_plist(void);

plist_t * free_p_element(plist_t *p);
void plist_free(plist_t *p);

void parray_free(parray_t pa);

parray_t new_parray(int len);

/*
 * Helper to assign all members of a p_element
 */
plist_t * set_p_element(plist_t *elem, int Na, int Nd, int dmg, double p, plist_t *next);

plist_t * parray_to_plist(parray_t pa);
plist_t * plist_copy(plist_t *p_in);

/*
 * Increment or set the p value in the parray
 */
void parray_incr_p(int Na, int Nd, int hp_remaining, double p, parray_t plist);

/*
 * Merge two sorted plists
 */
plist_t * plist_merge(plist_t *list1, plist_t *list2);

#endif