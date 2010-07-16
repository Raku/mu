#include <smop/base.h>
#include <smop/s1p.h>
#include <smop/capture.h>
#include <stdlib.h>
#include <smop/s0native.h>
#include <smop/native.h>
#include <smop/dump.h>
#include <stdio.h>

static SMOP__Object* primitive_int_add(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    SMOP__Object* a = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    SMOP__Object* b = SMOP__NATIVE__capture_positional(interpreter,capture,1);
    SMOP__Object* ret = SMOP__NATIVE__int_create(SMOP__NATIVE__int_fetch(a) + SMOP__NATIVE__int_fetch(b));
    SMOP_RELEASE(interpreter, a);
    SMOP_RELEASE(interpreter, b);
    return ret;
}

static SMOP__Object* primitive_int_substract(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    SMOP__Object* a = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    SMOP__Object* b = SMOP__NATIVE__capture_positional(interpreter,capture,1);
    SMOP__Object* ret = SMOP__NATIVE__int_create(SMOP__NATIVE__int_fetch(a) - SMOP__NATIVE__int_fetch(b));
    SMOP_RELEASE(interpreter, a);
    SMOP_RELEASE(interpreter, b);
    return ret;
}

static SMOP__Object* primitive_int_equal(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    SMOP__Object* a = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    SMOP__Object* b = SMOP__NATIVE__capture_positional(interpreter,capture,1);

    SMOP__Object* ret = SMOP__NATIVE__int_fetch(a) == SMOP__NATIVE__int_fetch(b) ? SMOP__NATIVE__bool_true : SMOP__NATIVE__bool_false;

    SMOP_RELEASE(interpreter, a);
    SMOP_RELEASE(interpreter, b);
    return ret;
}

static SMOP__Object* primitive_pointer_equal(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    SMOP__Object* a = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    SMOP__Object* b = SMOP__NATIVE__capture_positional(interpreter,capture,1);

    SMOP__Object* ret = a == b ? SMOP__NATIVE__bool_true : SMOP__NATIVE__bool_false;

    SMOP_RELEASE(interpreter, a);
    SMOP_RELEASE(interpreter, b);
    return ret;
}


static SMOP__Object* primitive_int_less(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    SMOP__Object* a = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    SMOP__Object* b = SMOP__NATIVE__capture_positional(interpreter,capture,1);

    SMOP__Object* ret = SMOP__NATIVE__int_fetch(a) < SMOP__NATIVE__int_fetch(b) ? SMOP__NATIVE__bool_true : SMOP__NATIVE__bool_false;

    SMOP_RELEASE(interpreter, a);
    SMOP_RELEASE(interpreter, b);
    return ret;
}

static SMOP__Object* primitive_idconst_concat(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    SMOP__Object* a = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    SMOP__Object* b = SMOP__NATIVE__capture_positional(interpreter,capture,1);


    int left_len,right_len;
    char *left = SMOP__NATIVE__idconst_fetch(a,&left_len);
    char *right = SMOP__NATIVE__idconst_fetch(b,&right_len);
    char *new_str = malloc(sizeof(char) * (right_len+left_len));
    int i;
    for (i=0;i<left_len;i++) {
      new_str[i] = left[i];
    }
    for (i=0;i<right_len;i++) {
      new_str[left_len+i] = right[i];
    }
    SMOP__Object* ret = SMOP__NATIVE__idconst_createn(new_str,left_len+right_len);

    free(new_str);

    SMOP_RELEASE(interpreter, a);
    SMOP_RELEASE(interpreter, b);
    return ret;
}

static SMOP__Object* primitive_idconst_eq(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    SMOP__Object* a = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    SMOP__Object* b = SMOP__NATIVE__capture_positional(interpreter,capture,1);

    SMOP__Object* ret =  a == b ? SMOP__NATIVE__bool_true : SMOP__NATIVE__bool_false;
    SMOP_RELEASE(interpreter, a);
    SMOP_RELEASE(interpreter, b);

    return ret;
}

static SMOP__Object* primitive_SMOP_RI(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    SMOP__Object* a = SMOP__NATIVE__capture_positional(interpreter,capture,0);

    SMOP__Object* ret = SMOP_REFERENCE(interpreter,SMOP_RI(a));
    SMOP_RELEASE(interpreter, a);

    return ret;
}

static SMOP__Object* primitive_storage_name(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    SMOP__Object* a = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    SMOP__Object* b = SMOP__NATIVE__capture_positional(interpreter,capture,1);
    int len;
    char *attr = SMOP__NATIVE__idconst_fetch_with_null(b,&len);
    char *str = malloc(sizeof(char) * len+17);
    snprintf(str,len+17,"%.16x%s\n",(unsigned int)a,attr);
    free(attr);

    SMOP__Object* ret = SMOP__NATIVE__idconst_createn(str,len+16);
    free(str);
    SMOP_RELEASE(interpreter, a);
    SMOP_RELEASE(interpreter, b);

    return ret;
}


static SMOP__Object* primitive_dump_print(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    SMOP__Object* object = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    SMOP__Object* file_template = SMOP__NATIVE__capture_positional(interpreter,capture,1);
    int len;
    char *template = SMOP__NATIVE__idconst_fetch_with_null(file_template,&len);
    smop_dump_print(interpreter,object,template);
    SMOP_RELEASE(interpreter,object);
    return SMOP__NATIVE__bool_false;
}

static SMOP__Object* primitive_interpreter(SMOP__Object* interpreter,SMOP__Object* ccode,SMOP__Object* capture) {
    return SMOP_REFERENCE(interpreter, interpreter);
}


static void insert_primitive(SMOP__Object* interpreter,SMOP__Object* package,char* name,SMOP__Object* obj) {
  SMOP_DISPATCH(interpreter,
                SMOP_RI(package),
                SMOP__NATIVE__idconst_create("bind_key"),
                SMOP__NATIVE__capture_create(interpreter,
                                             (SMOP__Object*[]) {package,SMOP__NATIVE__idconst_create(name),obj,NULL},
                                             (SMOP__Object*[]) {NULL}));
}


void smop_s1p_insert_primitives(SMOP__Object* interpreter,SMOP__Object* package) {
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&int_add",SMOP__S1P__CCode_create(primitive_int_add));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&int_equal",SMOP__S1P__CCode_create(primitive_int_equal));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&int_less",SMOP__S1P__CCode_create(primitive_int_less));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&int_substract",SMOP__S1P__CCode_create(primitive_int_substract));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&idconst_concat",SMOP__S1P__CCode_create(primitive_idconst_concat));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&idconst_eq",SMOP__S1P__CCode_create(primitive_idconst_eq));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&get_interpreter",SMOP__S1P__CCode_create(primitive_interpreter));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&storage_name",SMOP__S1P__CCode_create(primitive_storage_name));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&ritest",SMOP_REFERENCE(interpreter,SMOP__S1P__ritest));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&pointer_equal",SMOP__S1P__CCode_create(primitive_pointer_equal));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&SMOP_RI",SMOP__S1P__CCode_create(primitive_SMOP_RI));
  insert_primitive(interpreter,SMOP_REFERENCE(interpreter,package),"&dump_print",SMOP__S1P__CCode_create(primitive_dump_print));
}
