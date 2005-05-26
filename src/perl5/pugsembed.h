#include "perl5.h"
#include <HsFFI.h>

#ifndef PugsValDefined
#define PugsValDefined 1
/* #define PUGS_EXTERN extern */
#define PUGS_EXTERN  
typedef HsStablePtr Val;
#endif

PUGS_EXTERN Val *pugs_Eval ( char *code );
PUGS_EXTERN Val *pugs_Apply ( Val *sub, Val *inv, Val **args );

extern Val *pugs_IvToVal ( IV iv );
extern Val *pugs_NvToVal ( NV iv );
extern Val *pugs_PvToVal ( char *pv );

Val *pugs_SvToVal ( SV *sv );
SV  *pugs_MkValRef ( Val *val );

extern Val *pugs_MkSvRef  ( SV *sv );
extern SV  *pugs_ValToSv ( Val *val );

