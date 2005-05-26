#include "EXTERN.h"
#include "perl.h"
#include "embed.h"

PerlInterpreter * perl5_init ( int argc, char **argv );
char * perl5_SvPV ( SV * sv );
int perl5_SvIV ( SV * sv );
double perl5_SvNV ( SV * sv );
bool perl5_SvTRUE ( SV * sv );
SV * perl5_newSVpv ( char * pv );
SV * perl5_newSViv ( int iv );
SV * perl5_newSVnv ( double iv );
SV * perl5_call(char *subname, int argc, SV** args, SV *env, int cxt);
bool perl5_can(SV *inv, char *subname);
SV * perl5_eval(char *code, SV *env, int cxt);
SV * perl5_get_sv ( const char *name );
void * perl5_set_svref ( const char *name, void *sv );
