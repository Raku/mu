#ifndef SMOP_IDENTIFIERS_H
#define SMOP_IDENTIFIERS_H

/* This file list the constant identifiers that are not specific to
 * any of the base or low-level operators. If any of the base or
 * low-level values stringifies to this same values, we'll going to
 * point both to the same value.
 */

extern SMOP__Object* SMOP__ID__capture;           /* "capture" */
extern SMOP__Object* SMOP__ID__continuation;      /* "continuation" */
extern SMOP__Object* SMOP__ID__continues;         /* "continues" */
extern SMOP__Object* SMOP__ID__copy;              /* "copy" */
extern SMOP__Object* SMOP__ID__current;           /* "current" */
extern SMOP__Object* SMOP__ID__debug;             /* "debug" */
extern SMOP__Object* SMOP__ID__DESTROYALL;        /* "DESTROYALL" */
extern SMOP__Object* SMOP__ID__eval;              /* "eval" */
extern SMOP__Object* SMOP__ID__forget;            /* "forget" */
extern SMOP__Object* SMOP__ID__free;              /* "free" */
extern SMOP__Object* SMOP__ID__goto;              /* "goto" */
extern SMOP__Object* SMOP__ID__has_next;          /* "has_next" */
extern SMOP__Object* SMOP__ID__identifier;        /* "identifier" */
extern SMOP__Object* SMOP__ID__jail;              /* "jail" */
extern SMOP__Object* SMOP__ID__lexical;           /* "lexical" */
extern SMOP__Object* SMOP__ID__loop;              /* "loop" */
extern SMOP__Object* SMOP__ID__move_capturize;    /* "move_capturize" */
extern SMOP__Object* SMOP__ID__move_identifier;   /* "move_identifier" */
extern SMOP__Object* SMOP__ID__move_responder;    /* "move_responder" */
extern SMOP__Object* SMOP__ID__new;               /* "new" */
extern SMOP__Object* SMOP__ID__next;              /* "next" */
extern SMOP__Object* SMOP__ID__past;              /* "past" */
extern SMOP__Object* SMOP__ID__push;              /* "push" */
extern SMOP__Object* SMOP__ID__responder;         /* "responder" */
extern SMOP__Object* SMOP__ID__result;            /* "result" */
extern SMOP__Object* SMOP__ID__setr;              /* "setr" */
extern SMOP__Object* SMOP__ID__outer;             /* "outer" */

#endif
