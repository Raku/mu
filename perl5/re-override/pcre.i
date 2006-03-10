#pragma SWIG nowarn=302,451,509


%{
typedef SV* Perl_Scalar;
%}

%typemap(in) Perl_Scalar {
    $1 = (void *)$input;
}

%typemap(out) Perl_Scalar {
    $result = newSVsv((SV *)$1);
    sv_2mortal($result);
    argvi++;
}

%{
#include "pcre.h"

pcre* compile(const char *pat, int opt) {
    pcre *re;
    const char *error;
    int erroffset;
    re = pcre_compile(
        pat, opt,
        &error,
        &erroffset,
        NULL
    );
    return re;
}

Perl_Scalar execute(const pcre *re, const char *str, int opt) {
    int rc;
    int *ovector;
    AV* seq = newAV();
    int i;
    unsigned long int length;

    pcre_fullinfo(
        re,
        NULL,
        PCRE_INFO_SIZE,
        &length
    );

    ovector = malloc(sizeof(int) * length);

    rc = pcre_exec(
        re,         
        NULL,        
        str,
        strlen(str),
        0,
        0,
        ovector,
        30
    );

    if (rc >= 0) {
        for (i = 0; i < (rc*2); i++ ) {
            av_push(seq, newSViv(ovector[i]));
        }
    }

    free(ovector);

    return newRV_inc((SV*)seq);
}

%}

extern void regexp_exechook_insert();
extern void regexp_setup(regexp*, SV*, U32, SV*);
extern void regexp_hook_on();
extern void regexp_hook_off();

extern pcre *compile (const char *, int);
extern Perl_Scalar execute (const pcre *, const char *, int);

extern pcre *pcre_compile(const char *, int, const char **,
              int *, const unsigned char *);
extern int  pcre_config(int, void *);
extern int  pcre_copy_named_substring(const pcre *, const char *,
              int *, int, const char *, char *, int);
extern int  pcre_copy_substring(const char *, int *, int, int,
              char *, int);
extern int  pcre_exec(const pcre *, const pcre_extra *,
              const char *, int, int, int, int *, int);
extern void pcre_free_substring(const char *);
extern void pcre_free_substring_list(const char **);
extern int  pcre_fullinfo(const pcre *, const pcre_extra *, int,
              void *);
extern int  pcre_get_named_substring(const pcre *, const char *,
              int *, int,  const char *, const char **);
extern int  pcre_get_stringnumber(const pcre *, const char *);
extern int  pcre_get_substring(const char *, int *, int, int,
              const char **);
extern int  pcre_get_substring_list(const char *, int *, int,
              const char ***);
extern int  pcre_info(const pcre *, int *, int *);
extern const unsigned char *pcre_maketables(void);
extern pcre_extra *pcre_study(const pcre *, int, const char **);
extern const char *pcre_version(void);

typedef struct pcre_extra {
  unsigned long int flags;        /* Bits for which fields are set */
  void *study_data;               /* Opaque data from pcre_study() */
  unsigned long int match_limit;  /* Maximum number of calls to match() */
  void *callout_data;             /* Data passed back in callouts */
  const unsigned char *tables;    /* Pointer to character tables */
} pcre_extra;

/* The structure for passing out data via the pcre_callout_function. We use a
structure so that new fields can be added on the end in future versions,
without changing the API of the function, thereby allowing old clients to work
without modification. */

typedef struct pcre_callout_block {
  int          version;           /* Identifies version of block */
  /* ------------------------ Version 0 ------------------------------- */
  int          callout_number;    /* Number compiled into pattern */
  int         *offset_vector;     /* The offset vector */
  const char  *subject;           /* The subject being matched */
  int          subject_length;    /* The length of the subject */
  int          start_match;       /* Offset to start of this match attempt */
  int          current_position;  /* Where we currently are in the subject */
  int          capture_top;       /* Max current capture */
  int          capture_last;      /* Most recently closed capture */
  void        *callout_data;      /* Data passed in with the call */
  /* ------------------- Added for Version 1 -------------------------- */
  int          pattern_position;  /* Offset to next item in the pattern */
  int          next_item_length;  /* Length of next item in the pattern */
  /* ------------------------------------------------------------------ */
} pcre_callout_block;
