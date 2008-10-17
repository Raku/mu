#include <smop.h>
#include <smop_s1p.h>
#include <smop_lowlevel.h>
SMOP__Object* smop_dispatch(SMOP__Object* interpreter,SMOP__Object* ri,SMOP__Object* identifier,SMOP__Object* capture) {
  return SMOP_DISPATCH(interpreter,ri,identifier,capture);
}
SMOP__Object* smop_release(SMOP__Object* interpreter,SMOP__Object* obj) {
  return SMOP_RELEASE(interpreter,obj);
}
SMOP__Object* smop_release_with_global(SMOP__Object* obj) {
//  printf("obj.refcount=%d\n",((SMOP_LOWLEVEL_INTERNAL*)obj->data)->ref_cnt);
//  printf("interpreter.refcount=%d\n",((SMOP_LOWLEVEL_INTERNAL*)SMOP__GlobalInterpreter->data)->ref_cnt);
  return SMOP_RELEASE(SMOP__GlobalInterpreter,obj);
}
SMOP__Object* smop_reference(SMOP__Object* interpreter,SMOP__Object* obj) {
  return SMOP_REFERENCE(interpreter,obj);
}
SMOP__Object* smop_ri(SMOP__Object* obj) {
  return (SMOP__Object*) SMOP_RI(obj);
}
SMOP__Object* get_SMOP__GlobalInterpreter(void) {
  return SMOP__GlobalInterpreter;
}
SMOP__Object* get_SMOP__S1P__RootNamespace(void) {
  return SMOP__S1P__RootNamespace;
}
SMOP__Object* smop_get_cvar(char* var) {
  printf("fetching \"%s\"\n",var);
  int i;
  if (strcmp(var,"SMOP__S1P__LexicalScope") == 0) {
    return SMOP__S1P__LexicalScope;
  } else if (strcmp(var,"SMOP__S1P__RootNamespace") == 0) {
    return SMOP__S1P__RootNamespace;
  } else if (strcmp(var,"SMOP__S1P__LexicalPrelude") == 0) {
    return SMOP__S1P__LexicalPrelude;
  } else if (strcmp(var,"SMOP__S1P__Capturize") == 0) {
    return SMOP__S1P__Capturize;
  } else if (strcmp(var,"SMOP__S1P__Scalar") == 0) {
    return SMOP__S1P__Scalar;
  } else {
    printf("unable to fetch \"%s\"\n",var);
    return SMOP__NATIVE__bool_false;
  }
}
