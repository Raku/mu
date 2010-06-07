#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/dump.h>
#include <stdlib.h>
#include <stdio.h>

SMOP__ResponderInterface* SMOP__DUMP_RI;
SMOP__ResponderInterface* SMOP__DUMP_int_RI;
SMOP__ResponderInterface* SMOP__DUMP_obj_RI;
SMOP__ResponderInterface* SMOP__DUMP_obj_array_RI;



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
  

  SMOP__DUMP_obj_RI = calloc(1,sizeof(SMOP__ResponderInterface));
  SMOP__DUMP_obj_RI->MESSAGE = smop_placeholder_message;
  SMOP__DUMP_obj_RI->REFERENCE = smop_noop_reference;
  SMOP__DUMP_obj_RI->RELEASE = smop_noop_release;
  SMOP__DUMP_obj_RI->WEAKREF = smop_noop_weakref;
  SMOP__DUMP_obj_RI->id = "DUMP obj";
  SMOP__DUMP_obj_RI->RI = (SMOP__ResponderInterface *)SMOP__metaRI;

  SMOP__DUMP_obj_array_RI = calloc(1,sizeof(SMOP__ResponderInterface));
  SMOP__DUMP_obj_array_RI->MESSAGE = smop_placeholder_message;
  SMOP__DUMP_obj_array_RI->REFERENCE = smop_noop_reference;
  SMOP__DUMP_obj_array_RI->RELEASE = smop_noop_release;
  SMOP__DUMP_obj_array_RI->WEAKREF = smop_noop_weakref;
  SMOP__DUMP_obj_array_RI->id = "DUMP obj array";
  SMOP__DUMP_obj_array_RI->RI = (SMOP__ResponderInterface *)SMOP__metaRI;

  
  /* when SMOP__metaRI is created the dump modules wasn't yet loaded */
  SMOP__metaRI->RI->DUMP = smop_ri_dump;
}

SMOP__Object* smop_ri_dump(SMOP__Object* interpreter,
                                SMOP__ResponderInterface* responder,
                                SMOP__Object* obj) {

  SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*) obj;

  return smop_dump_create((SMOP__Object*[]) {
      smop_dump_attr_create("RI"),
      smop_dump_obj_create((SMOP__Object*)ri->RI),
      smop_dump_attr_create("id"),
      smop_dump_str_create(ri->id),
      NULL
  });
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
  dump->data = (SMOP__Object**) malloc(sizeof(SMOP__Object*) * (size));
  dump->RI = SMOP__DUMP_RI;
  int i;
  for (i=0;i<size;i++) {
    dump->data[i] = data[i];
  }
  return (SMOP__Object*) dump;
}

SMOP__Object* smop_dump_obj_array_create(SMOP__Object** data,int size) {
  smop_dump_obj_array* array = (smop_dump_obj_array*) malloc(sizeof(smop_dump_obj_array));
  array->size = size;
  array->data = (SMOP__Object**) malloc(sizeof(SMOP__Object*) * (size));
  array->RI = SMOP__DUMP_obj_array_RI;
  int i;
  for (i=0;i<size;i++) {
    array->data[i] = data[i];
  }
  return (SMOP__Object*) array;
}


SMOP__Object* smop_dump_int_create(int value) {
  smop_dump_int* obj = malloc(sizeof(smop_dump_int));
  obj->value = value;
  obj->RI = SMOP__DUMP_int_RI;
  return (SMOP__Object*) obj;
}


SMOP__Object* smop_dump_obj_create(SMOP__Object* value) {
  smop_dump_obj* obj = malloc(sizeof(smop_dump_obj));
  obj->value = value;
  obj->RI = SMOP__DUMP_obj_RI;
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

typedef struct list {
  struct list* next;
  SMOP__Object* value;
} list;
static list* list_add(list* l,SMOP__Object* value) {
  list* new_node = malloc(sizeof(list)); 
  new_node->value = value;
  new_node->next = l;
  return new_node;
}
static int in_list(list* l,SMOP__Object* value) {
  while (l) {
    if (l->value == value) return 1;
    l = l->next;
  }
  return 0;
}
static void list_destroy(list* l) {
  while (l) {
    list* old = l;
    l = l->next;
    free(old);
  }
}

void smop_dump_print(SMOP__Object* interpreter,SMOP__Object* obj,char* file) {


  FILE* f = fopen(file,"w");
  if (!f) {
    perror("can't dump the object:");
  }
  fprintf(f,"dump of %p\n",obj);
  list* visited = NULL;
  list* to_visit = list_add(NULL,obj);

  SMOP__ResponderInterface* idconst_ri = SMOP__NATIVE__idconst_create("example")->RI;
  while (to_visit) {
    list* current = to_visit;
    to_visit = to_visit->next;
    if (!current->value->RI->DUMP) {
      printf("no DUMP for %s\n",current->value->RI->id);
      exit(0);
    }
    smop_dump* dump = (smop_dump*) SMOP_DUMP(interpreter,current->value);
    current->next = visited;
    visited = current;

    fprintf(f,"dumping %p {\n",current->value);
    int i;
    for (i=0; i< dump->size;i++) {
      fprintf(f,"    ");
      if (dump->data[i]->RI == SMOP__DUMP_int_RI) { 
        fprintf(f,"%d\n",((smop_dump_int*)dump->data[i])->value);
      } else if (dump->data[i]->RI == SMOP__DUMP_obj_RI) { 
        SMOP__Object* obj = ((smop_dump_obj*)dump->data[i])->value;
        if (obj && !in_list(visited,obj) && !in_list(to_visit,obj)) {
          to_visit = list_add(to_visit,obj);
        }
        fprintf(f,"%p\n",obj);
      } else if (dump->data[i]->RI == idconst_ri) {
        int len;
        char* str = SMOP__NATIVE__idconst_fetch_with_null(dump->data[i],&len);
        /* TODO escape special chars */
        fprintf(f,"\"%s\"\n",str);
        free(str);
      } else if (dump->data[i]->RI == SMOP__DUMP_obj_array_RI) { 
        smop_dump_obj_array* array = (smop_dump_obj_array*) dump->data[i];
        fprintf(f,"[\n");
        int i;
        for (i=0; i< array->size;i++) {
          fprintf(f,"%p\n",(void*)array->data[i]);
          if (array->data[i] && !in_list(visited,array->data[i]) && !in_list(to_visit,array->data[i])) {
            to_visit = list_add(to_visit,array->data[i]);
          }
        }
        fprintf(f,"]\n");
      } else {
        fprintf(f,"unknown %s\n",dump->data[i]->RI->id);
      }
    }
    fprintf(f,"}\n");
  }
  fclose(f);
  printf("dumped\n");
  list_destroy(visited);
}
