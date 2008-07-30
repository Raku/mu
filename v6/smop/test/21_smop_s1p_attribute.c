#include <stdio.h>
#include <pthread.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <unistd.h>


int main(int argc, char** argv) {
  printf("1..4\n");
  smop_init();

  SMOP__Object* interpreter = SMOP_DISPATCH(SMOP__INTPTR__InterpreterInstance, SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                            SMOP__ID__new, 
                                            SMOP__NATIVE__capture_create(SMOP__INTPTR__InterpreterInstance,
                                                                         SMOP__INTPTR__InterpreterInstance,NULL,NULL));


  // create a new attribute object using the lowlevel call
  SMOP__Object* attribute = SMOP__S1P__Attribute_create(interpreter,
                                                        
  

  // call create_container

  // test the container type

  // test the private_name

  // test the name


  SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance,interpreter);

  smop_destr();

  printf("ok 4 - destroyed.\n");

  return 0;
}
