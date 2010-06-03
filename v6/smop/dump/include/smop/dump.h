#ifndef SMOP_DUMP_H
#define SMOP_DUMP_H

extern SMOP__ResponderInterface* SMOP__DUMP_RI;
extern SMOP__ResponderInterface* SMOP__DUMP_int_RI;
typedef struct smop_dump {
  SMOP__Object__BASE
  int size;
  SMOP__Object** data;
} smop_dump;

typedef struct smop_dump_int {
  SMOP__Object__BASE
  int value;
} smop_dump_int;

SMOP__Object* smop_dump_int_create(int value);
SMOP__Object* smop_dump_create(SMOP__Object** data);
SMOP__Object* smop_dump_str_create(char* value);
SMOP__Object* smop_dump_attr_create(char* value);
void smop_dump_print(SMOP__Object* interpreter,SMOP__Object* obj);

#endif
