#include <smop.h>
#include <smop_s1p.h>
SMOP__Object* smop_dispatch(SMOP__Object* interpreter,SMOP__Object* ri,SMOP__Object* identifier,SMOP__Object* capture) {
  return SMOP_DISPATCH(interpreter,ri,identifier,capture);
}
SMOP__Object* smop_release(SMOP__Object* interpreter,SMOP__Object* obj) {
  return SMOP_RELEASE(interpreter,obj);
}
SMOP__Object* smop_reference(SMOP__Object* interpreter,SMOP__Object* obj) {
  return SMOP_REFERENCE(interpreter,obj);
}
SMOP__Object* smop_ri(SMOP__Object* obj) {
  return SMOP_RI(obj);
}
SMOP__Object* get_SMOP__GlobalInterpreter(void) {
  return SMOP__GlobalInterpreter;
}
SMOP__Object* get_SMOP__S1P__RootNamespace(void) {
  return SMOP__S1P__RootNamespace;
}
