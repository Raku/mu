#ifdef SMOP_PROFILE
#include <smop/base.h>
#include <smop/s0native.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>

FILE* SMOP_PROFILE_out;
int SMOP_PROFILE_on;
struct timeval start;

SMOP__Object* SMOP_DISPATCH(SMOP__Object* interpreter,SMOP__Object* object,SMOP__Object* identifier,SMOP__Object* capture) {
  if (SMOP_PROFILE_on) {
      int identifier_size;
      char *s = SMOP__NATIVE__idconst_fetch(identifier,&identifier_size);
      struct timeval t;
      gettimeofday(&t,NULL);
      fprintf(SMOP_PROFILE_out,"%s.%.*s %lf\n",((SMOP__ResponderInterface*)object)->id,identifier_size,s,(double)(t.tv_sec-start.tv_sec) + (double)(t.tv_usec-start.tv_usec)/1000000);
  }
  return (((SMOP__ResponderInterface*)object)->MESSAGE(
    interpreter, ((SMOP__ResponderInterface*)object),
    identifier, capture
  ));
}
void smop_profile_init(void) {
  gettimeofday(&start,NULL);
  SMOP_PROFILE_on = !!(getenv("SMOP_PROFILE"));
  if (getenv("SMOP_PROFILE")) {
    SMOP_PROFILE_out = fopen(getenv("SMOP_PROFILE"),"w");
  }
}
#endif
