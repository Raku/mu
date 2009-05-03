void SMOP__S1P__Array_set_elems(SMOP__Object* interpreter,SMOP__Object* array,int elems);
int SMOP__S1P__Array_get_elems(SMOP__Object* interpreter,SMOP__Object* array);
void SMOP__S1P__Array_set_elem(SMOP__Object* interpreter,SMOP__Object* array,int i,SMOP__Object* value);
SMOP__Object* SMOP__S1P__Array_get_elem(SMOP__Object* interpreter,SMOP__Object* array,int i);
SMOP__Object* SMOP__S1P__ArrayProxy_create(SMOP__Object* array,int i);
