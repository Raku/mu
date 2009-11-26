#ifndef SMOP_NAGC_WEAKREF_H
#define SMOP_NAGC_WEAKREF_H

// this creates a new weakref pointing to value
extern SMOP__Object* smop_nagc_weakref_create(SMOP__NAGC__Object* value);

// this de-register this weakref from the original value
extern void smop_nagc_weakref_dereg(SMOP__NAGC__Object* value);

// this is used to notify that the original value no longer exists
extern void smop_nagc_weakref_lostref(SMOP__NAGC__Object* value);

// this is used during destruction, to cleanup the weakreferences
extern void smop_nagc_weakref_cleanup(SMOP__NAGC__Object* value);

extern void smop_nagc_weakref_init();
extern void smop_nagc_weakref_destr();

#endif
