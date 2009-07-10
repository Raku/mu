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

SMOP__Object* SMOP_DISPATCH(SMOP__Object* interpreter,SMOP__Object* object,SMOP__Object* identifier,SMOP__Object* capture) {
  if (SMOP_PROFILE_on) {
    int identifier_size = 1;
    char *s = "?";
    char *id = "?";
    if (identifier->RI == SMOP__NATIVE__idconst_create("new")->RI) {
      s = SMOP__NATIVE__idconst_fetch(identifier,&identifier_size);
    } else {
      identifier_size = strlen(s);
    }
    id = ((SMOP__ResponderInterface*)object)->id;

    struct timeval before,after;
    fprintf(SMOP_PROFILE_out,"> \"%s\".\"%.*s\"\n",id,identifier_size,s);

    gettimeofday(&before,NULL);
    SMOP__Object* ret = (((SMOP__ResponderInterface*)object)->MESSAGE(
      interpreter, ((SMOP__ResponderInterface*)object),
      identifier, capture
    ));
    gettimeofday(&after,NULL);


    fprintf(SMOP_PROFILE_out,"< \"%s\".\"%.*s\" took %d\n",id,identifier_size,s,(int)(after.tv_sec-before.tv_sec)*1000 + (int)(after.tv_usec-before.tv_usec));
    return ret;
  } else {
    return (((SMOP__ResponderInterface*)object)->MESSAGE(
      interpreter, ((SMOP__ResponderInterface*)object),
      identifier, capture
    ));

  }
}
void smop_profile_init(void) {
  SMOP_PROFILE_on = !!(getenv("SMOP_PROFILE"));
  if (getenv("SMOP_PROFILE")) {
    SMOP_PROFILE_out = fopen(getenv("SMOP_PROFILE"),"w");
  }
}
#endif
