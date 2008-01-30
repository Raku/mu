
#ifndef SMOP_SLIME_H
#define SMOP_SLIME_H

#include <smop.h>


/* Please see
http://www.perlfoundation.org/perl6/index.cgi?default_smop_interpreter_implementation
*/
extern SMOP__Object* SMOP__SLIME__Frame;
extern SMOP__Object* SMOP__SLIME__Node;

extern SMOP__Object* SMOP__SLIME__CurrentFrame;
extern SMOP__Object* SMOP__SLIME__Capturize;
SMOP__Object* SMOP__SLIME__Capturize_create(int invocant, int* positional, int* named, int target);
int SMOP__SLIME__Capturize_invocant(SMOP__Object* obj);
int* SMOP__SLIME__Capturize_positional(SMOP__Object* obj, int* retsize);
int* SMOP__SLIME__Capturize_named(SMOP__Object* obj, int* retsize);
int SMOP__SLIME__Capturize_target(SMOP__Object* obj);

#endif
