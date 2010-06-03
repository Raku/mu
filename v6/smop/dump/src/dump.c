#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/dump.h>
#include <stdlib.h>
#include <stdio.h>

SMOP__ResponderInterface* SMOP__DUMP_RI;
SMOP__ResponderInterface* SMOP__DUMP_int_RI;


void smop_dump_init() {
  SMOP__DUMP_RI = calloc(1,sizeof(SMOP__ResponderInterface));
  SMOP__DUMP_RI->MESSAGE = smop_placeholder_message;
  SMOP__DUMP_RI->REFERENCE = smop_noop_reference;
  SMOP__DUMP_RI->RELEASE = smop_noop_release;
  SMOP__DUMP_RI->WEAKREF = smop_noop_weakref;
  SMOP__DUMP_RI->id = "result of DUMP";
  SMOP__DUMP_RI->RI = (SMOP__ResponderInterface *)SMOP__metaRI;

  SMOP__DUMP_int_RI = calloc(1,sizeof(SMOP__ResponderInterface));
  SMOP__DUMP_int_RI->MESSAGE = smop_placeholder_message;
  SMOP__DUMP_int_RI->REFERENCE = smop_noop_reference;
  SMOP__DUMP_int_RI->RELEASE = smop_noop_release;
  SMOP__DUMP_int_RI->WEAKREF = smop_noop_weakref;
  SMOP__DUMP_int_RI->id = "DUMP int";
  SMOP__DUMP_int_RI->RI = (SMOP__ResponderInterface *)SMOP__metaRI;

}
SMOP__Object* smop_dump_create(SMOP__Object** data) {
  int size = 0;
  SMOP__Object** d = data;
  while (*d != NULL) {
    d++;
    size++;
  }
  smop_dump* dump = (smop_dump*) malloc(sizeof(smop_dump));
  dump->size = size;
  dump->data = (SMOP__Object**) malloc(sizeof(SMOP__Object*) * (size+1));
  dump->RI = SMOP__DUMP_RI;
  int i;
  for (i=0;i<size+1;i++) {
    dump->data[i] = data[i];
  }
  return (SMOP__Object*) dump;
}

SMOP__Object* smop_dump_int_create(int value) {
  smop_dump_int* obj = malloc(sizeof(smop_dump_int));
  obj->value = value;
  obj->RI = SMOP__DUMP_int_RI;
  return (SMOP__Object*) obj;
}
SMOP__Object* smop_dump_str_create(char* value) {
  return SMOP__NATIVE__idconst_create(value);
}
SMOP__Object* smop_dump_attr_create(char* value) {
  return SMOP__NATIVE__idconst_create(value);
}

void smop_dump_destr() {
  free(SMOP__DUMP_RI);
  free(SMOP__DUMP_int_RI);
}
void smop_dump_print(SMOP__Object* interpreter,SMOP__Object* obj) {
  SMOP__ResponderInterface* idconst_ri = SMOP__NATIVE__idconst_create("example")->RI;
  if (obj->RI != SMOP__DUMP_RI) {
    printf("smop_dump_json expects a DUMP got %s\n",obj->RI->id);
    return;
  }
  smop_dump* dump = (smop_dump*) obj;
  printf("dumping object (%d):{ \n",dump->size);
  int i;
  for (i=0; i< dump->size;i++) {
    printf("    ");
    if (dump->data[i]->RI == SMOP__DUMP_int_RI) { 
      printf("%d\n",((smop_dump_int*)dump)->value);
    } else if (dump->data[i]->RI == idconst_ri) {
      int len;
      char* str = SMOP__NATIVE__idconst_fetch_with_null(dump->data[i],&len);
      printf("%s\n",str);
      free(str);
    } else {
      printf("unknown %s\n",dump->data[i]->RI->id);
    }
  }
  printf("}\n");
}
