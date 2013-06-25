#include "plist.h"

#include <string.h>
#include <stdio.h>

/* list and array handling */

static plist_t *free_nodes;

static plist_t * free_p_element(plist_t *p)
{
	if (!p)
		return NULL;
	plist_t *n = p->next;
	//free(p);
	p->next = free_nodes;
	free_nodes = p;
	return n;
}

plist_t * new_plist(void)
{
	plist_t *new;
	int i;
	if (free_nodes) {
		new = free_nodes;
		free_nodes = free_nodes -> next;
		//memset(new, 0, sizeof(plist_t));
		return new;
	}
	new = free_nodes = calloc(1000, sizeof(plist_t));
	for (i = 1; i < 1000; i++)
		new = (new->next = &free_nodes[i]);
	return new_plist();
}

void plist_free(plist_t *p)
{
	//while ((p = free_p_element(p)));
	if (!p) return;
	plist_t *t = free_nodes;
	free_nodes = p;
	if (!t) return;
	while (p->next) 
		p = p->next;
	p->next = t;
}

parray_t new_parray(int len)
{
	len++; // allocate one extra element due to 1-indexing
	int data_size = len*sizeof(struct p_element *);
	parray_t l = malloc(sizeof(struct parray) + data_size);
	memset(l, 0, sizeof(struct parray) + data_size);
	l->len = len;
	return l;
}

void parray_free(parray_t pa)
{
	int i;
	if (!pa)
		return;
	for (i = 0; i < pa->len; i++)
		plist_free(pa->a[i]);
	free(pa);
}

plist_t * set_p_element(plist_t *elem, int Na, int Nd, int hp_remaining, double p, plist_t *next)
{
	elem->Na = Na;
	elem->Nd = Nd;
	elem->hp_remaining = hp_remaining;
	elem->p = p;
	elem->next = next;
	return elem;
};

static plist_t * copy_p_element(plist_t *elem) {
	plist_t *new = new_plist();
	memcpy(new, elem, sizeof(plist_t));
	return new;
};

void parray_incr_p(int Na, int Nd, int hp_remaining, double p, parray_t plist)
{
	plist_t  *curr, *new, **old_next;
	if (Na > plist->len) {
		printf("Na out of bounds\n");
		return;
	}
	old_next = &plist->a[Na];
	for (curr = *old_next; curr && curr->hp_remaining < hp_remaining; old_next = &curr->next, curr = curr->next);
	if (curr && curr->hp_remaining == hp_remaining) {
		curr->p += p;
		return;
	}
	new = new_plist();
	set_p_element(new, Na, Nd, hp_remaining, p, curr);
	*old_next = new;
}

#define RET_NE(x) if (p1->x != p2->x) return p1->x - p2->x;
static int cmp_plist(const plist_t *p1, const plist_t *p2)
{
	RET_NE(Na)
	RET_NE(Nd)
	RET_NE(hp_remaining)
	return 0;
}

#define SWAP_PTRS(a, b) do { void *t = (a); (a) = (b); (b) = t; } while (0)
plist_t * plist_merge(plist_t *list1, plist_t *list2)
{
	plist_t *list = NULL, **pnext = &list;
	int cmp;
	if (list2 == NULL)
		return list1;

	while (list1) {
		cmp = cmp_plist(list1, list2);
		if (cmp > 0)
			SWAP_PTRS(list1, list2);
		else if (cmp == 0) {
			list1->p += list2->p;
			list2 = free_p_element(list2);				
		}
		*pnext = list1;
		pnext = &list1->next;
		list1 = *pnext;
		if (!list2)
			return list;
	}
	*pnext = list2;
	return list;
}