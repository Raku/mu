#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

MODULE = Data::Bind                PACKAGE = Data::Bind

void
_av_store(SV *av_ref, I32 key, SV *val)
  CODE:
{
    /* XXX many checks */
    AV *av = (AV *)SvRV(av_ref);
    /* XXX unref the old one in slot? */
    av_store(av, key, SvREFCNT_inc(SvRV(val)));
}
