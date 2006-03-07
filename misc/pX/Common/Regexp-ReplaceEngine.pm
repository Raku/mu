=pod
Objective:

  Permit swapping in an alternate regexp engine,
  using calls to p5 subs,
  which can be defined differently in different packages.

This would permit
  ./perl -we 'use Regexp::Perl6; print "a" =~ /<word>**{1}/;'

Status:

  Perl subs get called for regexp comp and exec.
  The hooks are not package-specific.

Design:

  The comp hook sub should return a subref, which gets written onto a
  hopefully otherwise unused section of a regexp struct.  Another
  hopefully unused part of the struct is overwritten with a flag
  value, so a regexp* thus created can be recognized, and regexp*s
  created with the normal perl engine can continue working.

  Upon exec, the subref should return match information.  Match
  information gets inserted in $1 etal.  These will be limited to
  strings, rather than Match objects.

  A set of hooks should be defined in CORE which just call back to the
  original comp and exec (not implemented).  The C hooks call for the
  sub hooks in the caller package, and thus get CORE if none are defined.

  The C hooks (except for comp, obviously) look for the flag value,
  and call the original Perl_ funcs if it is not present.

Risks:

  Trying to properly do (non-leaking) memory management, via reverse
  engineering (almost completely undocumented source), in the presence
  of copy-on-write and such complications, could be a bear.

  A source filter approach, while likely flakier (modulo memory issues),
  might suffice, be easier to create, and be more portable.
  http://www.perlmonks.org/index.pl?node_id=531465

  It may not be possible to get Match objects (only strings) into
  $1,etal, or even into the return value.  In that event, it may not
  be possible to get a plausibly scoped alternative working.

  Using $/ might be non-trivial, as only some forms of redefinition
  make its magic not stick.

  The implementation is at best an XS-based module (currently Inline::C),
  and is thus not very portable to broken (no compiler) microsoft systems.

  The implementation reaches into perlguts, which limits the range of
  perl versions it will work with, and raises maintenance concerns.
  That said, the use of internals is rather limited, and in a rather
  stable are of the code, so this may not be a real problem.

  The regexp engine and the perl compiler have an overly friendly
  relationship.  There may end up being a gotcha, where one needs
  something to happen or not happen, but the relevant line of code is
  off in perl runtime guts where it can't plausibly be reached to
  change.

  There may be no place to put the flag value?  And then prove
  implausible to realloc the regexp struct to create one?

Possible next steps:

  Get per-package hooks working.

Notes:

  http://perl.plover.com/Rx/paper/

  Some files of interest in the perl sources:
    ./regexp.h ./regcomp.c ./regexec.c
    ./ext/re/re.xs ./ext/re/re.pm
    http://cvs.perl.org/viewcvs/perl5/mirror/

  And of course man perlapi, etc.

  The perl regexp engine was long ago derived from Henry Spencer's
  regexp library.  http://arglist.com/regex/  While the code has diverged,
  at least the library has some documentation, which can help point you
  in the right direction.

  The regexp compiler hook generally gets the pattern _after_ variable
  interpolation has occured.  But not in the case of m'pattern'.
  Which might come in hand when doing p6 regex.


Notes on functions:

regexp* regcompp (pTHX_ char* exp, char* xend, PMOP* pm)

what is pm?
XXX finish

I32      regexec (pTHX_ regexp* prog, char* stringarg, char* strend,
                  char* strbeg, I32 minend, SV* screamer,
                  void* data, U32 flags)

retval
 0 failure
 1 success
yes?

XXX finish


XXX What exactly _is_ intuit, and do we care?



Notes on struct regex:

  typedef struct regexp {

The regular expression:

  I32 prelen;             /* length of precomp */
  char *precomp;          /* pre-compilation regular expression */
  U32 nparens;            /* number of parentheses */

The match result:

  I32 sublen;             /* Length of string pointed by subbeg */
  char *subbeg;           /* saved or original string 
                                   so \digit works forever. */

  I32 *startp;
  I32 *endp;

for $n (eg $1 => n=1), 1<=n<=nparens,
startp[n] is the starting offset in subbeg,
endp[n] is the offset of the last char within the capture.
n=0 is the total match?

  U32 lastparen;          /* last paren matched */
  U32 lastcloseparen;     /* last paren matched */

Other things the world messes with:

  I32 refcnt;

What invariants?  Don't know.


  I32 minlen;             /* mininum possible length of $& */

Must be zero.  Otherwise the run core thinks too much.


  U32 reganch;            /* Internal use only +
                             Tainted information used by regexec? */

Flags.  Despite the comment, _lots_ of folks look at it.  The one
field labeled "Internal use only" is one of the most widely used. ;)
Zero seems safe.  But more nuanced treatment will be needed if utf8 is
to be supported.

  regnode program[1];     /* Unwarranted chumminess with compiler. */

I don't know.

Internals:

  regnode *regstclass;

Points into ->data.
sv.c's Perl_re_dup sets it to NULL without looking.
No-one else touches it.

  struct reg_substr_data *substrs;

        struct reg_substr_data {
          struct reg_substr_datum data[3];
        };
        struct reg_substr_datum {
            I32 min_offset;
            I32 max_offset;
            SV *substr;         /* non-utf8 variant */
            SV *utf8_substr;    /* utf8 variant */
        };

min_offset and max_offset would be plausible places to hide data.
Perl_re_dup just copies them.

  struct reg_data *data;  /* Additional data. */

        struct reg_data {
            U32 count;
            U8 *what;
            void* data[1];
        };

Leave something real here and don't touch it.
Perl_re_dup knows a lot about it.


#ifdef PERL_COPY_ON_WRITE
  SV *saved_copy;         /* If non-NULL, SV which is COW from original */
#endif

I don't know.
Possible danger?

  U32 *offsets;           /* offset annotations 20001228 MJD */

offsets[0] specifies the 2*len+1 length of the array.
Other than that, we can do anything we want with it.
This is perhaps the most flexible place to allocate large data
which Perl_re_dup will copy for us.

    New(0, ret->offsets, 2*len+1, U32);
    Copy(r->offsets, ret->offsets, 2*len+1, U32);


} regexp;


=cut

sub regexp_hook_exec {
  my($s)=@_;
  print STDERR "execing on string (".(defined($s)?"'$s'":"undef").").\n";
  return reverse(1,undef,undef,0,3,0,2);
#  return (undef,undef,1);
}

sub regexp_hook_comp {
  my($re)=@_;
  print STDERR "compiling pattern '$re'.\n";
  return (1,\&regexp_hook_exec);
}

hooks_in();
#my $qr = eval("qr/foobar2/");
#"abcd" =~ $qr;
eval <<'END';
my $m = "asdf" =~ /foobar3/;
print "match? ",$m ? "yes" : "no","\n";
print ' $` ',defined($`)?"\"$`\"":"undef","\n";
print ' $& ',defined($&)?"\"$&\"":"undef","\n";
print ' $\' ',defined($')?"\"$'\"":"undef","\n";
print ' $-[0] ',defined($-[0])?"$-[0]":"undef","\n";
print ' $+[0] ',defined($+[0])?"$+[0]":"undef","\n";
print ' $1 ',defined($1)?"\"$1\" $-[1] $+[1]":"undef","\n";
print ' $2 ',defined($2)?"\"$2\" $-[2] $+[2]":"undef","\n";
print ' $3 ',defined($3)?"\"$3\" $-[3] $+[3]":"undef","\n";
print ' $^N ',defined($^N)?"\"$^N\"":"undef","\n";
END

use Data::Dumper;
sub printDump {
  print Dumper(@_);
}

use Inline C => <<'END';
/*
 *  Debugging aids 
 */
void print_sv(SV* sv) {
  fprintf(stderr,"%ld\n",(unsigned long)sv);
}
void printDump(SV* sv) {
  dSP;

  ENTER;
  SAVETMPS;
  PUSHMARK(SP);
  XPUSHs(sv);
  PUTBACK;
  call_pv("main::printDump", 0);
  SPAGAIN;
  PUTBACK;
  FREETMPS;
  LEAVE;
}
void regexp_describe (regexp* r) {
  fprintf(stderr,
          "\n"
          "REGEXP* %lu\n"
          "startp  %lu (I32*)\n"
          "endp    %lu (I32*)\n"
          "regstclass %lu (regnode*)\n"
          "substrs %lu (struct reg_substr_data*)\n"
          "precomp %lu \"%s\" (char*)\n"
          "data    %lu (struct reg_data*)\n"
          "subbeg  %lu \"%s\" (char*)\n"
#ifdef PERL_COPY_ON_WRITE
          "saved_copy %lu (SV*)\n"
#endif
          "offsets %lu (U32*)\n"
          "sublen %d (I32)\n"
          "refcnt %d (I32)\n"
          "minlen %d (I32)\n"
          "prelen %d (I32)\n"
          "nparens        %lu (U32)\n"
          "lastparen      %lu (U32)\n"
          "lastcloseparen %lu (U32)\n"
          "reganch %lu (U32)\n"
          "program %lu (&regnode)\n"
          ,
          r,
          (ulong)r->startp,(ulong)r->endp,(ulong)r->regstclass,
          (ulong)r->substrs,(ulong)r->precomp,r->precomp,
          (ulong)r->data,(ulong)r->subbeg,r->subbeg,
#ifdef PERL_COPY_ON_WRITE
          (ulong)r->saved_copy,
#endif
          (ulong)r->offsets,
          (int)r->sublen,(int)r->refcnt,(int)r->minlen,(int)r->prelen,
          (ulong)r->nparens,(ulong)r->lastparen,(ulong)r->lastcloseparen,
          (ulong)r->reganch,
          (ulong)&(r->program[0])
          );
#ifdef PERL_COPY_ON_WRITE
  printDump(r->saved_copy);
#endif
  fprintf(stderr,"offsets: %lu\n",(ulong)r->offsets[0]);
  {
    int n;
    for(n=0;n<r->offsets[0];n++) {
      fprintf(stderr," %d: pos %lu len %lu end %lu\n",
        n,
        r->offsets[2*n-1],r->offsets[2*n],
        r->offsets[2*n-1]+r->offsets[2*n]-1);
    }
  }
  fprintf(stderr,"\n");
}


/*
 *  The hooks
 */
#define OFFSET_LEN 3
#define RECOGNITION_PATTERN 0xEce32d63
#define REGEXP_IS_HOOKED(r) \
   (r->offsets[0]==OFFSET_LEN && r->offsets[1]==RECOGNITION_PATTERN)

regexp* hook_regcompp (pTHX_ char* exp, char* xend, PMOP* pm)
{
  regexp* r;
  SV* f;
  int ret;
  int len;
  int npar;
  dSP;

  fprintf(stderr,"hook_regcompp\n");

  ENTER;
  SAVETMPS;
  PUSHMARK(SP);
  XPUSHs(sv_2mortal(newSVpvn(exp,xend-exp)));
  PUTBACK;
  ret = call_pv("regexp_hook_comp", G_ARRAY);
  if(ret != 2) {
    fprintf(stderr,"regexp_hook_comp failed\n"); exit(0);
  }
  SPAGAIN;
  f = POPi; /*XXX - POPs doesnt work, POPi does, neither expected :( */
  npar = POPi;
  PUTBACK;
  FREETMPS;
  LEAVE;

  r = Perl_pregcomp(aTHX_ "","",pm);

  /* regexp_describe(r); */

  Safefree(r->precomp);
  r->prelen = xend - exp;
  r->precomp = savepvn(exp,r->prelen);

  r->nparens = npar;

  Renew(r->startp, npar+1, I32);
  Renew(r->endp, npar+1, I32);

  r->minlen = 0;
  r->reganch = 0;

  len = OFFSET_LEN;
  Renew(r->offsets, 2*len+1, U32);
  r->offsets[0] = len;

  r->offsets[1] = RECOGNITION_PATTERN;
  *((SV**)&(r->offsets[2])) = f;
  /* Do not put anything in offsets[3]. */

  /* XXX Does regcomp.c zero any of the match fields? */

  fprintf(stderr,"def %ld %ld\n",(ulong)r,(ulong)f);
  return r;
}


I32 hook_regexec (pTHX_ regexp* r, char* stringarg, char* strend,
                  char* strbeg, I32 minend, SV* screamer,
                  void* data, U32 flags)
{
  SV* f;
  int ret;
  IV matched;
  dSP;

  if(!REGEXP_IS_HOOKED(r)) {
    return Perl_regexec_flags(r,stringarg,strend,strbeg,
                              minend,screamer,data,flags);
  } 

  fprintf(stderr,"hook_regexec (%ld %ld %ld %ld) %d\n",(ulong)r,(ulong)stringarg,(ulong)strend,(ulong)strbeg,strend-strbeg);


  Safefree(r->subbeg);
  r->sublen = strend-strbeg;
  r->subbeg = savepvn(strbeg,r->sublen);

  f = *((SV**)&(r->offsets[2]));

  ENTER;
  SAVETMPS;
  PUSHMARK(SP);
  XPUSHs(newSVpv(strbeg,strend-strbeg));
  PUTBACK;

  fprintf(stderr,"exec hook %ld\n",(ulong)f);
  ret = call_sv(f, G_ARRAY);
  fprintf(stderr,"exec hook survived.\n");
  if(ret < 1) {
    fprintf(stderr,"regexp_hook_exec failed - didnt return anything\n"); exit(0);
  }
  SPAGAIN;
  matched = POPi;

  { /* fail captures */
    int i;
    for(i=0;i<=r->nparens;i++) {
      r->startp[i] = -1;
      r->endp[i] = -1;
    }
    r->lastparen = r->lastcloseparen = 0;
  }

  if(matched) {
    SV* lp;
    SV* lcp; 
    int i;
    if(ret < 3 || ret != 3 + 2 * (r->nparens+1)) {
      fprintf(stderr,"regexp_hook_exec failed - paren info broken\n"); exit(0);
    }
    lp = POPs;
    lcp = POPs;
    for(i=0;i<=r->nparens;i++) {
      r->startp[i] = POPi;
      r->endp[i] = POPi;
    }
    r->lastparen = (lp == &PL_sv_undef) ? SvIV(lp) : r->nparens;
    r->lastcloseparen = (lcp == &PL_sv_undef) ? SvIV(lcp) : r->nparens;
  }

  PUTBACK;
  FREETMPS;
  LEAVE;

  return matched ? 1 : 0;
}


void hook_regfree (pTHX_ struct regexp* r)
{
  fprintf(stderr,"free %ld\n",(ulong)r);
  if(1) {
    Perl_pregfree(aTHX_ r);
    fprintf(stderr,"free survived.\n");
  } else {
    fprintf(stderr,"free skipped.\n");
  }
  return;
}

char* hook_re_intuit_start (pTHX_ regexp *prog, SV *sv, char *strpos,
                            char *strend, U32 flags,
                            struct re_scream_pos_data_s *data)
{
  if(!REGEXP_IS_HOOKED(prog)) {
    return Perl_re_intuit_start(prog,sv,strpos,strend,flags,data);
  }
  fprintf(stderr,"hook_re_intuit_start - shouldn't be here\n");
  return NULL;
}

SV*   hook_re_intuit_string (pTHX_ regexp *prog)
{
  if(!REGEXP_IS_HOOKED(prog)) {
    return Perl_re_intuit_string(prog);
  }
  fprintf(stderr,"hook_re_intuit_string - shouldn't be here\n");
  return &PL_sv_undef;
}

void hooks_in() {
  Inline_Stack_Vars;
  PL_regcompp = &hook_regcompp;
  PL_regexecp = &hook_regexec;
  PL_regint_start = &hook_re_intuit_start;
  PL_regint_string = &hook_re_intuit_string;
  PL_regfree = &hook_regfree;
  Inline_Stack_Void;
}
END


