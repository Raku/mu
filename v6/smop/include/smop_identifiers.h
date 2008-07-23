#ifndef SMOP_IDENTIFIERS_H
#define SMOP_IDENTIFIERS_H

/* This file list the constant identifiers that are not specific to
 * any of the base or low-level operators. If any of the base or
 * low-level values stringifies to this same values, we'll going to
 * point both to the same value.
 */

extern SMOP__Object* SMOP__ID__back;              /* "back" */
extern SMOP__Object* SMOP__ID__bless;             /* "bless" */
extern SMOP__Object* SMOP__ID__BUILD;             /* "BUILD" */
extern SMOP__Object* SMOP__ID__BUILDALL;          /* "BUILDALL" */
extern SMOP__Object* SMOP__ID__call;              /* "call" */
extern SMOP__Object* SMOP__ID__can;               /* "can" */
extern SMOP__Object* SMOP__ID__capture;           /* "capture" */
extern SMOP__Object* SMOP__ID__clone;             /* "clone" */
extern SMOP__Object* SMOP__ID__continuation;      /* "continuation" */
extern SMOP__Object* SMOP__ID__continues;         /* "continues" */
extern SMOP__Object* SMOP__ID__copy;              /* "copy" */
extern SMOP__Object* SMOP__ID__CREATE;            /* "CREATE" */
extern SMOP__Object* SMOP__ID__current;           /* "current" */
extern SMOP__Object* SMOP__ID__debug;             /* "debug" */
extern SMOP__Object* SMOP__ID__defined;           /* "defined" */
extern SMOP__Object* SMOP__ID__drop;              /* "drop" */
extern SMOP__Object* SMOP__ID__does;              /* "does" */
extern SMOP__Object* SMOP__ID__DESTROY;           /* "DESTROY" */
extern SMOP__Object* SMOP__ID__DESTROYALL;        /* "DESTROYALL" */
extern SMOP__Object* SMOP__ID__dispatch;          /* "dispatch" */
extern SMOP__Object* SMOP__ID__eval;              /* "eval" */
extern SMOP__Object* SMOP__ID__FETCH;             /* "FETCH" */
extern SMOP__Object* SMOP__ID__forget;            /* "forget" */
extern SMOP__Object* SMOP__ID__free;              /* "free" */
extern SMOP__Object* SMOP__ID__goto;              /* "goto" */
extern SMOP__Object* SMOP__ID__has_next;          /* "has_next" */
extern SMOP__Object* SMOP__ID__identifier;        /* "identifier" */
extern SMOP__Object* SMOP__ID__invocant;          /* "invocant" */
extern SMOP__Object* SMOP__ID__isa;               /* "isa" */
extern SMOP__Object* SMOP__ID__jail;              /* "jail" */
extern SMOP__Object* SMOP__ID__lexical;           /* "lexical" */
extern SMOP__Object* SMOP__ID__loop;              /* "loop" */
extern SMOP__Object* SMOP__ID__move_capturize;    /* "move_capturize" */
extern SMOP__Object* SMOP__ID__move_identifier;   /* "move_identifier" */
extern SMOP__Object* SMOP__ID__move_responder;    /* "move_responder" */
extern SMOP__Object* SMOP__ID__name;              /* "name" */
extern SMOP__Object* SMOP__ID__new;               /* "new" */
extern SMOP__Object* SMOP__ID__next;              /* "next" */
extern SMOP__Object* SMOP__ID__past;              /* "past" */
extern SMOP__Object* SMOP__ID__positional;        /* "positional" */
extern SMOP__Object* SMOP__ID__push;              /* "push" */
extern SMOP__Object* SMOP__ID__register;          /* "register" */
extern SMOP__Object* SMOP__ID__responder;         /* "responder" */
extern SMOP__Object* SMOP__ID__result;            /* "result" */
extern SMOP__Object* SMOP__ID__setr;              /* "setr" */
extern SMOP__Object* SMOP__ID__shift;             /* "shift" */
extern SMOP__Object* SMOP__ID__signature;         /* "signature" */
extern SMOP__Object* SMOP__ID__STORE;             /* "STORE" */
extern SMOP__Object* SMOP__ID__outer;             /* "outer" */
extern SMOP__Object* SMOP__ID__perl;              /* "perl" */

extern SMOP__Object* SMOP__ID__HOW_CREATE;        /* "^CREATE" */
extern SMOP__Object* SMOP__ID__HOW_methods;       /* "^methods" */
extern SMOP__Object* SMOP__ID__HOW_attributes;    /* "^attributes" */
extern SMOP__Object* SMOP__ID__HOW_isa;           /* "^isa" */
extern SMOP__Object* SMOP__ID__HOW_can;           /* "^can" */
extern SMOP__Object* SMOP__ID__HOW_has;           /* "^has" */

extern SMOP__Object* SMOP__ID__REPR_CREATE;       /* "^!CREATE" */
extern SMOP__Object* SMOP__ID__REPR_DESTROY;      /* "^!DESTROY" */
extern SMOP__Object* SMOP__ID__REPR_clone;        /* "^!clone" */
extern SMOP__Object* SMOP__ID__REPR_bless;        /* "^!bless" */
extern SMOP__Object* SMOP__ID__REPR_how;          /* "^!how" */
extern SMOP__Object* SMOP__ID__REPR_initialize_instance_storage;         /* "^!initialize_instance_storage" */
extern SMOP__Object* SMOP__ID__REPR_initialize_instance_storage_slot;    /* "^!initialize_instance_storage_slot" */
extern SMOP__Object* SMOP__ID__REPR_destroy_instance_storage;            /* "^!destroy_instance_storage" */
extern SMOP__Object* SMOP__ID__REPR_destroy_instance_storage_slot;       /* "^!destroy_instance_storage_slot" */
extern SMOP__Object* SMOP__ID__REPR_defined;      /* "^!defined" */
extern SMOP__Object* SMOP__ID__REPR_instance;     /* "^!instance" */
extern SMOP__Object* SMOP__ID__REPR_whence;       /* "^!whence" */
extern SMOP__Object* SMOP__ID__REPR_isa;          /* "^!isa" */
extern SMOP__Object* SMOP__ID__REPR_role;         /* "^!role" */
extern SMOP__Object* SMOP__ID__REPR_private_storage;                     /* "^!private_storage" */
extern SMOP__Object* SMOP__ID__REPR_class_storage;                       /* "^!class_storage" */
extern SMOP__Object* SMOP__ID__REPR_methods;      /* "^!methods" */
extern SMOP__Object* SMOP__ID__REPR_attributes;   /* "^!attributes" */
extern SMOP__Object* SMOP__ID__REPR_package;      /* "^!package" */
extern SMOP__Object* SMOP__ID__REPR_properties;   /* "^!properties" */
extern SMOP__Object* SMOP__ID__REPR_can;          /* "^!can" */
extern SMOP__Object* SMOP__ID__REPR_has;          /* "^!has" */

extern SMOP__Object* SMOP__ID__elems;          /* "elems" */
extern SMOP__Object* SMOP__ID__postcircumfix_curly;          /* "postcircumfix:{ }" */
extern SMOP__Object* SMOP__ID__postcircumfix_square;          /* "postcircumfix:{ }" */
extern SMOP__Object* SMOP__ID__infix_eq;                    /* "infix:eq" */ 
#endif
