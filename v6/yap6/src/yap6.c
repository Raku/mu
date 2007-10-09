#include "yap6.h"
#include <stdlib.h>
#include <assert.h>

int main(int arc, char** argv) {

  yap6_ident_dispatcher_init();
  yap6_const_init();

  YAP6__CORE__HashDispatcher* prototype_class_class_dispatcher = calloc(1,sizeof(YAP6__CORE__HashDispatcher));
  assert(prototype_class_class_dispatcher);
  
  YAP6__CORE__Hash* prototype_class_class = calloc(1,sizeof(YAP6__CORE__Hash));
  assert(prototype_class_class);
  
  prototype_class_class->dispatcher = prototype_class_class_dispatcher;

  return 0;
}
