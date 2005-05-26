#include "perl5.h"

typedef void Val;

extern Val *pugs_Eval ( char *code );
extern Val *pugs_Apply ( Val *sub, Val *inv, Val **args );

extern Val *pugs_IvToVal ( IV iv );
extern Val *pugs_NvToVal ( NV iv );
extern Val *pugs_PvToVal ( char *pv );

extern Val *pugs_MkSvRef  ( SV *sv );
extern SV  *pugs_ValToSv ( Val *val );

Val *pugs_SvToVal ( SV *sv );
SV  *pugs_MkValRef ( Val *val );

