#ifndef WCSUBST_INCL

#define WCSUBST_INCL

#include <stdlib.h>

int stg_hack_u_iswupper(int wc);
int stg_hack_u_iswdigit(int wc);
int stg_hack_u_iswalpha(int wc);
int stg_hack_u_iswcntrl(int wc);
int stg_hack_u_iswspace(int wc);
int stg_hack_u_iswprint(int wc);
int stg_hack_u_iswlower(int wc);

int stg_hack_u_iswalnum(int wc);

int stg_hack_u_towlower(int wc);
int stg_hack_u_towupper(int wc);
int stg_hack_u_towtitle(int wc);

int stg_hack_u_gencat(int wc);

#endif

