#S#  grammar STD:ver<6.0.0.alpha>:auth<http://perl.org>;
grammar STD:ver<6.0.0.alpha>:auth<http://perl.org>;
#S#  
#S#  # should some of these be parser instance attributes?
#S#  my $LANG is context;
#S#  my $PKGDECL is context = "";
#S#  my $PKG is context = "";
#S#  my @PKGS;
#S#  my $GOAL is context = "(eof)";
#S#  my $PARSER is context<rw>;
#S#  my $ACTIONS is context<rw>;
#S#  my $IN_DECL is context<rw>;
#S#  my $SIGIL is context<rw>;
#S#  my %ROUTINES;
#S#  my $ORIG is context;
#S#  my @MEMOS is context;
#S#  my $VOID is context<rw>;
#S#  my @PADS;
my $LANG is context;
my $PKGDECL is context = "";
my $PKG is context = "";
my @PKGS;
my $GOAL is context = "(eof)";
my $PARSER is context<rw>;
my $ACTIONS is context<rw>;
my $IN_DECL is context<rw>;
my $SIGIL is context<rw>;
my %ROUTINES;
my $ORIG is context;
my @MEMOS is context;
my $VOID is context<rw>;
my @PADS;
#S#  
#S#  # random rule for debugging, please ignore
#S#  token foo {
#S#     'foo' <.ws> 'bar' <.ws> 'baz'
#S#  }
#S#  
#S#  =begin comment overview
#S#  
#S#  This file is designed to be either preprocessed into a grammar with
#S#  action statements or used as-is without any preprocessing.  The {*}
#S#  notation is a no-op action block, but can be identified uniquely via a
#S#  combination of the preceding token or rule name plus any additional text
#S#  following a #= comment.  We put this into a comment rather than using
#S#  a macro so that bootstrap compilers don't have to worry about macros
#S#  yet, and to keep the main grammar relatively uncluttered by action
#S#  statements.  Note that the preprocessor can certainly generate accesses
#S#  to the match state within the action block, so we need not mention it
#S#  explicitly.
#S#  
#S#  Also, some rules are named by syntactic category plus an additonal symbol
#S#  specified in adverbial form, either in bare :name form or in :sym<name>
#S#  form.  (It does not matter which form you use for identifier symbols,
#S#  except that to specify a symbol "sym" you must use the :sym<sym> form
#S#  of adverb.)  If you use the <sym> rule within the rule, it will parse the
#S#  symbol at that point.  At the final reduction point of a rule, if $sym
#S#  has been set, that is used as the final symbol name for the rule.  This
#S#  need not match the symbol specified as part the rule name; that is just
#S#  for disambiguating the name.  However, if no $sym is set, the original
#S#  symbol will be used by default.
#S#  
#S#  Note that rules automatically get an implicit {*} at their return, so
#S#  for the TOP rule the implicit action name is also simply "TOP".
#S#  
#S#  Another nod toward preprocessing is that blocks that contain nested braces
#S#  are delimited by double braces so that the preprocessor does not need to
#S#  understand Perl 6 code.
#S#  
#S#  This grammar relies on transitive longest-token semantics, though
#S#  initially we made a feeble attempt to order rules so a procedural
#S#  interpretation of alternation could usually produce a correct parse.
#S#  (This is becoming less true over time.)
#S#  
#S#  =end comment overview
#S#  
#S#  method TOP ($STOP = undef) {
#S#      if defined $STOP {
#S#          my $GOAL is context = $STOP;
#S#          self.unitstop($STOP).comp_unit;
#S#      }
#S#      else {
#S#          self.comp_unit;
#S#      }
#S#  }
method TOP ($STOP = undef) {
    if defined $STOP {
        my $GOAL is context = $STOP;
        self.unitstop($STOP).comp_unit;
    }
    else {
        self.comp_unit;
    }
}
#S#  
#S#  
#S#  # XXX shouldn't need this, it should all be defined/imported by the prelude
#S#  
#S#  my @basetypenames = qw[
#S#      Object Any Junction Whatever
#S#      Capture Match Signature Proxy Matcher
#S#      Package Module Class Role Grammar
#S#      Scalar Array Hash KeyHash KeySet KeyBag
#S#      Pair List Seq Range Set Bag Mapping
#S#      Void Undef Failure Exception
#S#      Code Block Routine Sub Macro
#S#      Method Submethod Regex
#S#  
#S#      Str Blob
#S#      Char Byte Codepoint Grapheme StrPos StrLen Version
#S#  
#S#      Num Complex
#S#      num complex
#S#  
#S#      Int  int   int1  int2  int4 int8  int16  int32  int64
#S#      Rat  rat   rat1  rat2  rat4 rat8  rat16  rat32  rat64
#S#      UInt uint uint1 uint2 uint4 uint8 uint16 uint32 uint64
#S#      Buf  buf   buf1  buf2  buf4 buf8  buf16  buf32  buf64
#S#  
#S#      Bit Bool
#S#      bit bool
#S#  
#S#      Order Increasing Decreasing
#S#      Ordered Callable Positional Associative Abstraction
#S#      Ordering KeyExtractor Comparator OrderingPair
#S#  
#S#      IO
#S#  
#S#      KitchenSink
#S#  ];
my @basetypenames = qw[
    Object Any Junction Whatever
    Capture Match Signature Proxy Matcher
    Package Module Class Role Grammar
    Scalar Array Hash KeyHash KeySet KeyBag
    Pair List Seq Range Set Bag Mapping
    Void Undef Failure Exception
    Code Block Routine Sub Macro
    Method Submethod Regex

    Str Blob
    Char Byte Codepoint Grapheme StrPos StrLen Version

    Num Complex
    num complex

    Int  int   int1  int2  int4 int8  int16  int32  int64
    Rat  rat   rat1  rat2  rat4 rat8  rat16  rat32  rat64
    UInt uint uint1 uint2 uint4 uint8 uint16 uint32 uint64
    Buf  buf   buf1  buf2  buf4 buf8  buf16  buf32  buf64

    Bit Bool
    bit bool

    Order Increasing Decreasing
    Ordered Callable Positional Associative Abstraction
    Ordering KeyExtractor Comparator OrderingPair

    IO

    KitchenSink
];
#S#  push @basetypenames, "True", "False", "Bool::True", "Bool::False";  # in quotes lest gimme5 translate them
push @basetypenames, "True", "False", "Bool::True", "Bool::False";  # in quotes lest gimme5 translate them
#S#  
#S#  method is_type ($name) {
#S#      for reverse @PADS {
#S#          return True if $_.{$name};
#S#      }
#S#      return False;
#S#  }
method is_type ($name) {
    for reverse @PADS {
        return 1 if $_.{$name}; #XXX  True if $_.{$name};
    }
    return 0; #XXX  False;
}
#S#  
#S#  method add_type ($name) {
#S#      my $typename = main::mangle($name);
#S#      my $qualname = ($+PKG // 'GLOBAL') ~ '::' ~ $typename;
#S#      @PADS[*-1]{$typename} = 'TYPE';
#S#      @PADS[*-1]{$qualname} = 'TYPE';
#S#      @PADS[*-1]{$name} = 'TYPE';
#S#  }
method add_type ($name) {
    my $typename = main::mangle($name); #XXX??? a cheat?
    my $qualname = ($+PKG // 'GLOBAL') ~ '::' ~ $typename;
    @PADS[-1]{$typename} = 'TYPE'; #XXX *-1
    @PADS[-1]{$qualname} = 'TYPE'; #XXX *-1
    @PADS[-1]{$name} = 'TYPE'; #XXX *-1
}
#S#  
#S#  # XXX likewise for routine defs
#S#  
#S#  my @baseroutinenames = qw[
#S#      WHAT WHERE HOW WHICH VAR WHO WHENCE new
#S#      any all none one
#S#      not true
#S#  
#S#      die exit warn
#S#      caller want
#S#      eval evalfile
#S#      callsame callwith nextsame nextwith lastcall
#S#      defined undefine item list slice eager hyper
#S#  
#S#      cat classify
#S#      quotemeta
#S#      chr ord
#S#      p5chop chop p5chomp chomp trim
#S#      index rindex substr
#S#      join split comb pack unpack
#S#      uc ucfirst lc lcfirst
#S#      normalize
#S#      nfc nfd nfkc nfkd
#S#      samecase sameaccent
#S#      capitalize
#S#      chars graphs codes bytes
#S#  
#S#      say print open close printf sprintf slurp unlink link symlink
#S#      elems grep map first reduce sort uniq push reverse take splice
#S#      lines getc
#S#  
#S#      zip each roundrobin caller
#S#      return leave pop shift unshift reduce
#S#      keys values hash kv key value pairs pair
#S#  
#S#      sign abs floor ceiling round truncate
#S#      exp log log10 sqrt roots
#S#      rand srand pick
#S#      cis unpolar
#S#  
#S#      sin cos tan asin acos atan sec cosec cotan asec acosec
#S#      acotan sinh cosh tanh asinh acosh atanh sech cosech cotanh
#S#      asech acosech acotanh atan2
#S#  
#S#      plan is ok dies_ok lives_ok skip todo pass flunk force_todo use_ok
#S#      isa_ok cmp_ok diag is_deeply isnt like skip_rest unlike nonce
#S#      skip_rest eval_dies_ok eval_lives_ok approx is_approx throws_ok version_lt
#S#  
#S#      gmtime localtime time times
#S#      gethost getpw chroot getlogin
#S#      run runinstead
#S#      fork wait kill sleep
#S#  ];
#S#  push @baseroutinenames, "HOW", "fail", "temp", "let";
#S#  
#S#  # please don't add: ref length bless delete exists
my @baseroutinenames = qw[
    WHAT WHERE HOW WHICH VAR WHO WHENCE new
    any all none one
    not true

    die exit warn
    caller want
    eval evalfile
    callsame callwith nextsame nextwith lastcall
    defined undefine item list slice eager hyper

    cat classify
    quotemeta
    chr ord
    p5chop chop p5chomp chomp trim
    index rindex substr
    join split comb pack unpack
    uc ucfirst lc lcfirst
    normalize
    nfc nfd nfkc nfkd
    samecase sameaccent
    capitalize
    chars graphs codes bytes

    say print open close printf sprintf slurp unlink link symlink
    elems grep map first reduce sort uniq push reverse take splice
    lines getc

    zip each roundrobin caller
    return leave pop shift unshift reduce
    keys values hash kv key value pairs pair

    sign abs floor ceiling round truncate
    exp log log10 sqrt roots
    rand srand pick
    cis unpolar

    sin cos tan asin acos atan sec cosec cotan asec acosec
    acotan sinh cosh tanh asinh acosh atanh sech cosech cotanh
    asech acosech acotanh atan2

    plan is ok dies_ok lives_ok skip todo pass flunk force_todo use_ok
    isa_ok cmp_ok diag is_deeply isnt like skip_rest unlike nonce
    skip_rest eval_dies_ok eval_lives_ok approx is_approx throws_ok version_lt

    gmtime localtime time times
    gethost getpw chroot getlogin
    run runinstead
    fork wait kill sleep
];
push @baseroutinenames, "HOW", "fail", "temp", "let";
#S#  
#S#  my @routinenames;
#S#  my %routinenames;
#S#  
#S#  sub init_pads {
#S#      @PKGS = ();
#S#      %ROUTINES = ();
#S#  
#S#      @PADS = ();
#S#      @PADS[0] = {};
#S#      for @basetypenames {
#S#          @PADS[0]{$_} = 'TYPE';
#S#          @PADS[0]{'&' ~ $_} = 'CODE';
#S#      }
#S#      for @baseroutinenames {
#S#          @PADS[0]{'&' ~ $_} = 'CODE';
#S#      }
#S#  }
#S#  
#S#  method is_routine ($name) {
#S#      my $aname;
#S#      if substr($name,0,1) eq '&' {
#S#          $aname = $name;
#S#      }
#S#      else {
#S#          $aname = '&' ~ $name;
#S#      }
#S#      for reverse @PADS {
#S#          return True if $_.{$aname};
#S#          return True if $_.{$name}; # type as routine?
#S#      }
#S#      return False;
#S#  }
#S#  
#S#  method add_routine ($name) {
#S#      @PADS[*-1]{'&' ~ $name} = 'CODE';
#S#  }
my @routinenames;
my %routinenames;

sub init_pads {
    @PKGS = []; #XXX ();
    %ROUTINES = {}; #XXX ();

    @PADS = []; #XXX ();
    @PADS[0] = {};
    for @basetypenames {
        @PADS[0]{$_} = 'TYPE';
        @PADS[0]{'&' ~ $_} = 'CODE';
    }
    for @baseroutinenames {
        @PADS[0]{'&' ~ $_} = 'CODE';
    }
}

method is_routine ($name) {
    my $aname;
    if substr($name,0,1) eq '&' {
        $aname = $name;
    }
    else {
        $aname = '&' ~ $name;
    }
    for reverse @PADS {
        return 1 if $_.{$aname}; #XXX True
        return 1 if $_.{$name}; # type as routine? #XXX True
    }
    return 0; #XXX  False;
}

method add_routine ($name) {
    @PADS[-1]{'&' ~ $name} = 'CODE'; #XXX *-1
}
#S#  
#S#  # The internal precedence levels are *not* part of the public interface.
#S#  # The current values are mere implementation; they may change at any time.
#S#  # Users should specify precedence only in relation to existing levels.
#S#  
#S#  constant %term            = (:prec<z=>);
#S#  constant %methodcall      = (:prec<y=>);
#S#  constant %autoincrement   = (:prec<x=>);
#S#  constant %exponentiation  = (:prec<w=>, :assoc<right>, :assign);
#S#  constant %symbolic_unary  = (:prec<v=>);
#S#  constant %multiplicative  = (:prec<u=>, :assoc<left>,  :assign);
#S#  constant %additive        = (:prec<t=>, :assoc<left>,  :assign);
#S#  constant %replication     = (:prec<s=>, :assoc<left>,  :assign);
#S#  constant %concatenation   = (:prec<r=>, :assoc<list>,  :assign);
#S#  constant %junctive_and    = (:prec<q=>, :assoc<list>,  :assign);
#S#  constant %junctive_or     = (:prec<p=>, :assoc<list>,  :assign);
#S#  constant %named_unary     = (:prec<o=>);
#S#  constant %nonchaining     = (:prec<n=>, :assoc<non>);
#S#  constant %chaining        = (:prec<m=>, :assoc<chain>, :bool);
#S#  constant %tight_and       = (:prec<l=>, :assoc<list>,  :assign);
#S#  constant %tight_or        = (:prec<k=>, :assoc<list>,  :assign);
#S#  constant %conditional     = (:prec<j=>, :assoc<right>);
#S#  constant %item_assignment = (:prec<i=>, :assoc<right>);
#S#  constant %loose_unary     = (:prec<h=>);
#S#  constant %comma           = (:prec<g=>, :assoc<list>, :nextterm<nulltermish>);
#S#  constant %list_infix      = (:prec<f=>, :assoc<list>,  :assign);
#S#  constant %list_assignment = (:prec<i=>, :sub<e=>, :assoc<right>);
#S#  constant %list_prefix     = (:prec<e=>);
#S#  constant %loose_and       = (:prec<d=>, :assoc<list>,  :assign);
#S#  constant %loose_or        = (:prec<c=>, :assoc<list>,  :assign);
#S#  constant %sequencer      = (:prec<b=>, :assoc<left>, :nextterm<statement>);
#S#  constant %LOOSEST         = (:prec<a=!>);
#S#  constant %terminator      = (:prec<a=>, :assoc<list>);
constant %term            = (:prec<z=>);
constant %methodcall      = (:prec<y=>);
constant %autoincrement   = (:prec<x=>);
constant %exponentiation  = (:prec<w=>, :assoc<right>, :assign);
constant %symbolic_unary  = (:prec<v=>);
constant %multiplicative  = (:prec<u=>, :assoc<left>,  :assign);
constant %additive        = (:prec<t=>, :assoc<left>,  :assign);
constant %replication     = (:prec<s=>, :assoc<left>,  :assign);
constant %concatenation   = (:prec<r=>, :assoc<list>,  :assign);
constant %junctive_and    = (:prec<q=>, :assoc<list>,  :assign);
constant %junctive_or     = (:prec<p=>, :assoc<list>,  :assign);
constant %named_unary     = (:prec<o=>);
constant %nonchaining     = (:prec<n=>, :assoc<non>);
constant %chaining        = (:prec<m=>, :assoc<chain>, :bool);
constant %tight_and       = (:prec<l=>, :assoc<list>,  :assign);
constant %tight_or        = (:prec<k=>, :assoc<list>,  :assign);
constant %conditional     = (:prec<j=>, :assoc<right>);
constant %item_assignment = (:prec<i=>, :assoc<right>);
constant %loose_unary     = (:prec<h=>);
constant %comma           = (:prec<g=>, :assoc<list>, :nextterm<nulltermish>);
constant %list_infix      = (:prec<f=>, :assoc<list>,  :assign);
constant %list_assignment = (:prec<i=>, :sub<e=>, :assoc<right>);
constant %list_prefix     = (:prec<e=>);
constant %loose_and       = (:prec<d=>, :assoc<list>,  :assign);
constant %loose_or        = (:prec<c=>, :assoc<list>,  :assign);
constant %sequencer      = (:prec<b=>, :assoc<left>, :nextterm<statement>);
constant %LOOSEST         = (:prec<a=!>);
constant %terminator      = (:prec<a=>, :assoc<list>);
#S#  
#S#  # "epsilon" tighter than terminator
#S#  #constant $LOOSEST = %LOOSEST<prec>;
#S#  constant $LOOSEST = "a=!"; # XXX preceding line is busted
#S#  constant $item_assignment_prec = 'i=';
constant $LOOSEST = "a=!";
constant $item_assignment_prec = 'i=';
#S#  
#S#  
#S#  role PrecOp {
#S#  
#S#      # This is hopefully called on a match to mix in operator info by type.
#S#      method coerce (Match $m) {
#S#          # $m but= ::?CLASS;
#S#          my $var = self.WHAT ~ '::o';
#S#          my $d = %::($var); 
#S#          if not $d<transparent> {
#S#              for keys(%$d) { $m<O>{$_} = $d.{$_} };
#S#              $m.deb("coercing to " ~ self) if $*DEBUG +& DEBUG::EXPR;
#S#          }
#S#          $m<O><kind> = self.WHAT;
#S#          return $m;
#S#      }
#S#  
#S#  } # end role
role PrecOp {
} # end role
#S#  
#S#  class Hyper does PrecOp {
#S#   our %o = (:transparent);
#S#  } # end class
#S#  
#S#  class Term does PrecOp {
#S#      our %o = %term;
#S#  } # end class
#S#  class Methodcall does PrecOp {
#S#      our %o = %methodcall;
#S#  } # end class
#S#  class Autoincrement does PrecOp {
#S#      our %o = %autoincrement;
#S#  } # end class
#S#  class Exponentiation does PrecOp {
#S#      our %o = %exponentiation;
#S#  } # end class
#S#  class Symbolic_unary does PrecOp {
#S#      our %o = %symbolic_unary;
#S#  } # end class
#S#  class Multiplicative does PrecOp {
#S#      our %o = %multiplicative;
#S#  } # end class
#S#  class Additive does PrecOp {
#S#      our %o = %additive;
#S#  } # end class
#S#  class Replication does PrecOp {
#S#      our %o = %replication;
#S#  } # end class
#S#  class Concatenation does PrecOp {
#S#      our %o = %concatenation;
#S#  } # end class
#S#  class Junctive_and does PrecOp {
#S#      our %o = %junctive_and;
#S#  } # end class
#S#  class Junctive_or does PrecOp {
#S#      our %o = %junctive_or;
#S#  } # end class
#S#  class Named_unary does PrecOp {
#S#      our %o = %named_unary;
#S#  } # end class
#S#  class Nonchaining does PrecOp {
#S#      our %o = %nonchaining;
#S#  } # end class
#S#  class Chaining does PrecOp {
#S#      our %o = %chaining;
#S#  } # end class
#S#  class Tight_and does PrecOp {
#S#      our %o = %tight_and;
#S#  } # end class
#S#  class Tight_or does PrecOp {
#S#      our %o = %tight_or;
#S#  } # end class
#S#  class Conditional does PrecOp {
#S#      our %o = %conditional;
#S#  } # end class
#S#  class Item_assignment does PrecOp {
#S#      our %o = %item_assignment;
#S#  } # end class
#S#  class Loose_unary does PrecOp {
#S#      our %o = %loose_unary;
#S#  } # end class
#S#  class Comma does PrecOp {
#S#      our %o = %comma;
#S#  } # end class
#S#  class List_infix does PrecOp {
#S#      our %o = %list_infix;
#S#  } # end class
#S#  class List_assignment does PrecOp {
#S#      our %o = %list_assignment;
#S#  } # end class
#S#  class List_prefix does PrecOp {
#S#      our %o = %list_prefix;
#S#  } # end class
#S#  class Loose_and does PrecOp {
#S#      our %o = %loose_and;
#S#  } # end class
#S#  class Loose_or does PrecOp {
#S#      our %o = %loose_or;
#S#  } # end class
#S#  class Sequencer does PrecOp {
#S#      our %o = %sequencer;
#S#  } # end class
#S#  class Terminator does PrecOp {
#S#      our %o = %terminator;
#S#  } # end class
class Hyper does PrecOp {
 our %o = (:transparent);
} # end class

class Term does PrecOp {
    our %o = %term;
} # end class
class Methodcall does PrecOp {
    our %o = %methodcall;
} # end class
class Autoincrement does PrecOp {
    our %o = %autoincrement;
} # end class
class Exponentiation does PrecOp {
    our %o = %exponentiation;
} # end class
class Symbolic_unary does PrecOp {
    our %o = %symbolic_unary;
} # end class
class Multiplicative does PrecOp {
    our %o = %multiplicative;
} # end class
class Additive does PrecOp {
    our %o = %additive;
} # end class
class Replication does PrecOp {
    our %o = %replication;
} # end class
class Concatenation does PrecOp {
    our %o = %concatenation;
} # end class
class Junctive_and does PrecOp {
    our %o = %junctive_and;
} # end class
class Junctive_or does PrecOp {
    our %o = %junctive_or;
} # end class
class Named_unary does PrecOp {
    our %o = %named_unary;
} # end class
class Nonchaining does PrecOp {
    our %o = %nonchaining;
} # end class
class Chaining does PrecOp {
    our %o = %chaining;
} # end class
class Tight_and does PrecOp {
    our %o = %tight_and;
} # end class
class Tight_or does PrecOp {
    our %o = %tight_or;
} # end class
class Conditional does PrecOp {
    our %o = %conditional;
} # end class
class Item_assignment does PrecOp {
    our %o = %item_assignment;
} # end class
class Loose_unary does PrecOp {
    our %o = %loose_unary;
} # end class
class Comma does PrecOp {
    our %o = %comma;
} # end class
class List_infix does PrecOp {
    our %o = %list_infix;
} # end class
class List_assignment does PrecOp {
    our %o = %list_assignment;
} # end class
class List_prefix does PrecOp {
    our %o = %list_prefix;
} # end class
class Loose_and does PrecOp {
    our %o = %loose_and;
} # end class
class Loose_or does PrecOp {
    our %o = %loose_or;
} # end class
class Sequencer does PrecOp {
    our %o = %sequencer;
} # end class
class Terminator does PrecOp {
    our %o = %terminator;
} # end class
#S#  
#S#  # Categories are designed to be easily extensible in derived grammars
#S#  # by merely adding more rules in the same category.  The rules within
#S#  # a given category start with the category name followed by a differentiating
#S#  # adverbial qualifier to serve (along with the category) as the longer name.
#S#  
#S#  # The endsym context, if specified, says what to implicitly check for in each
#S#  # rule right after the initial <sym>.  Normally this is used to make sure
#S#  # there's appropriate whitespace.  # Note that endsym isn't called if <sym>
#S#  # isn't called.
#S#  
#S#  my $endsym is context = "null";
#S#  my $endargs is context = -1;
#S#  
#S#  proto token category { <...> }
#S#  
#S#  token category:category { <sym> }
#S#  
#S#  token category:sigil { <sym> }
#S#  proto token sigil { <...> }
#S#  
#S#  token category:twigil { <sym> }
#S#  proto token twigil { <...> }
#S#  
#S#  token category:special_variable { <sym> }
#S#  proto token special_variable { <...> }
#S#  
#S#  token category:version { <sym> }
#S#  proto token version { <...> }
#S#  
#S#  token category:module_name { <sym> }
#S#  proto token module_name { <...> }
#S#  
#S#  token category:term { <sym> }
#S#  proto token term { <...> }
#S#  
#S#  token category:quote { <sym> }
#S#  proto token quote () { <...> }
#S#  
#S#  token category:prefix { <sym> }
#S#  proto token prefix is unary is defequiv(%symbolic_unary) { <...> }
#S#  
#S#  token category:infix { <sym> }
#S#  proto token infix is binary is defequiv(%additive) { <...> }
#S#  
#S#  token category:postfix { <sym> }
#S#  proto token postfix is unary is defequiv(%autoincrement) { <...> }
#S#  
#S#  token category:dotty { <sym> }
#S#  proto token dotty (:$endsym is context = 'unspacey') { <...> }
#S#  
#S#  token category:circumfix { <sym> }
#S#  proto token circumfix { <...> }
#S#  
#S#  token category:postcircumfix { <sym> }
#S#  proto token postcircumfix is unary { <...> }  # unary as far as EXPR knows...
#S#  
#S#  token category:quote_mod { <sym> }
#S#  proto token quote_mod { <...> }
#S#  
#S#  token category:trait_verb { <sym> }
#S#  proto token trait_verb (:$endsym is context = 'spacey') { <...> }
#S#  
#S#  token category:trait_auxiliary { <sym> }
#S#  proto token trait_auxiliary (:$endsym is context = 'spacey') { <...> }
#S#  
#S#  token category:type_declarator { <sym> }
#S#  proto token type_declarator () { <...> }
#S#  
#S#  token category:scope_declarator { <sym> }
#S#  proto token scope_declarator (:$endsym is context = 'nofun') { <...> }
#S#  
#S#  token category:package_declarator { <sym> }
#S#  proto token package_declarator () { <...> }
#S#  
#S#  token category:multi_declarator { <sym> }
#S#  proto token multi_declarator () { <...> }
#S#  
#S#  token category:routine_declarator { <sym> }
#S#  proto token routine_declarator () { <...> }
#S#  
#S#  token category:regex_declarator { <sym> }
#S#  proto token regex_declarator () { <...> }
#S#  
#S#  token category:statement_prefix { <sym> }
#S#  proto rule  statement_prefix () { <...> }
#S#  
#S#  token category:statement_control { <sym> }
#S#  proto rule  statement_control (:$endsym is context = 'spacey') { <...> }
#S#  
#S#  token category:statement_mod_cond { <sym> }
#S#  proto rule  statement_mod_cond (:$endsym is context = 'nofun') { <...> }
#S#  
#S#  token category:statement_mod_loop { <sym> }
#S#  proto rule  statement_mod_loop (:$endsym is context = 'nofun') { <...> }
#S#  
#S#  token category:infix_prefix_meta_operator { <sym> }
#S#  proto token infix_prefix_meta_operator is binary { <...> }
#S#  
#S#  token category:infix_postfix_meta_operator { <sym> }
#S#  proto token infix_postfix_meta_operator ($op) is binary { <...> }
#S#  
#S#  token category:infix_circumfix_meta_operator { <sym> }
#S#  proto token infix_circumfix_meta_operator is binary { <...> }
#S#  
#S#  token category:postfix_prefix_meta_operator { <sym> }
#S#  proto token postfix_prefix_meta_operator is unary { <...> }
#S#  
#S#  token category:prefix_postfix_meta_operator { <sym> }
#S#  proto token prefix_postfix_meta_operator is unary { <...> }
#S#  
#S#  token category:prefix_circumfix_meta_operator { <sym> }
#S#  proto token prefix_circumfix_meta_operator is unary { <...> }
#S#  
#S#  token category:terminator { <sym> }
#S#  proto token terminator { <...> }
#S#  
#S#  token unspacey { <.unsp>? }
#S#  token spacey { <?before \s | '#'> }
#S#  token nofun { <!before '(' | '.(' | '\\' > }
#S#  
#S#  # Lexical routines
#S#  
#S#  token ws {
#S#      :my @stub = return self if @+MEMOS[self.pos]<ws> :exists;
#S#      :my $startpos = self.pos;
#S#  
#S#      :dba('whitespace')
#S#      [
#S#          | \h+ <![#\s\\]> { @+MEMOS[$¢.pos]<ws> = $startpos; }   # common case
#S#          | <?before \w> <?after \w> :::
#S#              { @+MEMOS[$startpos]<ws> = undef; }
#S#              <!>        # must \s+ between words
#S#      ]
#S#      ||
#S#      [
#S#      | <.unsp>
#S#      | <.vws> <.heredoc>
#S#      | <.unv>
#S#      | $ { $¢.moreinput }
#S#      ]*
#S#  
#S#      {{
#S#          if ($¢.pos == $startpos) {
#S#              @+MEMOS[$¢.pos]<ws> = undef;
#S#          }
#S#          else {
#S#              @+MEMOS[$¢.pos]<ws> = $startpos;
#S#              @+MEMOS[$¢.pos]<endstmt> = @+MEMOS[$startpos]<endstmt>
#S#                  if @+MEMOS[$startpos]<endstmt> :exists;
#S#          }
#S#      }}
#S#  }
#S#  
#S#  token unsp {
#S#      \\ <?before [\s|'#'] >
#S#      :dba('unspace')
#S#      [
#S#      | <.vws>                     {*}                             #= vwhite
#S#      | <.unv>                  {*}                                #= unv
#S#      | $ { $¢.moreinput }
#S#      ]*
#S#  }
#S#  
#S#  token vws {
#S#      :dba('vertical whitespace')
#S#      \v
#S#      [ '#DEBUG -1' { say "DEBUG"; $STD::DEBUG = $*DEBUG = -1; } ]?
#S#  }
#S#  
#S#  # We provide two mechanisms here:
#S#  # 1) define $+moreinput, or
#S#  # 2) override moreinput method
#S#  method moreinput () {
#S#      $+moreinput.() if $+moreinput;
#S#  }
#S#  
#S#  token unv {
#S#     :dba('horizontal whitespace')
#S#     [
#S#     | \h+                 {*}                                    #= hwhite
#S#     | <?before '='> ^^ <.pod_comment>  {*}                    #= pod
#S#     | \h* '#' [
#S#           |  <?opener>
#S#              [ <!after ^^ . > || <.panic: "Can't use embedded comments in column 1"> ]
#S#              <.quibble($¢.cursor_fresh( ::STD::Q ))>   {*}                               #= embedded
#S#           | {} \N*            {*}                                 #= end
#S#           ]
#S#      ]
#S#  }
#S#  
#S#  token ident {
#S#      <.alpha> \w*
#S#  }
#S#  
#S#  token apostrophe {
#S#      <[ ' \- ]>
#S#  }
#S#  
#S#  token identifier {
#S#      <.ident> [ <.apostrophe> <.ident> ]*
#S#  }
#S#  
#S#  # XXX We need to parse the pod eventually to support $= variables.
#S#  
#S#  token pod_comment {
#S#      ^^ '=' <.unsp>?
#S#      [
#S#      | 'begin' \h+ <identifier> ::
#S#          [
#S#          ||  .*? "\n=" <.unsp>? 'end' \h+ $<identifier> » \N*          {*} #= tagged
#S#          ||  .*?                                                       {*} #= end
#S#          ]
#S#      | 'begin' » :: \h* [ $$ || '#' || <.panic: "Unrecognized token after =begin"> ]
#S#          [ .*?  "\n=" <.unsp>? 'end' » \N* || <.panic: "=begin without =end"> ]   {*}       #= anon
#S#      | :: 
#S#          [ <?before .*? ^^ '=cut' » > <.panic: "Obsolete pod format, please use =begin/=end instead"> ]?
#S#          \N*                                           {*}       #= misc
#S#      ]
#S#  }
#S#  
#S#  # Top-level rules
#S#  
#S#  # Note: we only check for the stopper.  We don't check for ^ because
#S#  # we might be embedded in something else.
#S#  rule comp_unit {
#S#      :my $begin_compunit is context = 1;
#S#      :my $endargs        is context<rw> = -1;
#S#  
#S#      :my $LANG is context;
#S#      :my $PKGDECL is context = "";
#S#      :my $PKG is context = "";
#S#      :my $GOAL is context = "(eof)";
#S#      :my $PARSER is context<rw>;
#S#      :my $IN_DECL is context<rw>;
#S#  
#S#      { init_pads(); }
#S#  
#S#      <statementlist>
#S#      [ <?unitstopper> || <.panic: "Can't understand next input--giving up"> ]
#S#      # "CHECK" time...
#S#      {{
#S#          if @COMPILING::WORRIES {
#S#              warn "Potential difficulties:\n  " ~ join( "\n  ", @COMPILING::WORRIES) ~ "\n";
#S#          }
#S#  
#S#          my %UNKNOWN;
#S#          for keys(%ROUTINES) {
#S#              next if $¢.is_routine($_);
#S#              %UNKNOWN{$_} = %ROUTINES{$_};
#S#          }
#S#          if %UNKNOWN {
#S#              warn "Unknown routines:\n";
#S#              for sort keys(%UNKNOWN) {
#S#                  warn "\t$_ called at ", %UNKNOWN{$_}, "\n";
#S#              }
#S#          }
#S#      }}
#S#  }
#S#  
#S#  # Note: because of the possibility of placeholders we can't determine arity of
#S#  # the block syntactically, so this must be determined via semantic analysis.
#S#  # Also, pblocks used in an if/unless statement do not treat $_ as a placeholder,
#S#  # while most other blocks treat $_ as equivalent to $^x.  Therefore the first
#S#  # possible place to check arity is not here but in the rule that calls this
#S#  # rule.  (Could also be done in a later pass.)
#S#  
#S#  token pblock {
#S#      :dba('parameterized block')
#S#      [ <lambda> <signature> ]? <block>
#S#  }
#S#  
#S#  token lambda { '->' | '<->' }
#S#  
#S#  # Look for an expression followed by a required lambda.
#S#  token xblock {
#S#      :my $GOAL is context = '{';
#S#      <EXPR>
#S#      <.ws>
#S#      <pblock>
#S#  }
#S#  
#S#  token block {
#S#      '{' ~ '}' <statementlist>
#S#  
#S#      [
#S#      | <?before \h* $$>  # (usual case without comments)
#S#          { @+MEMOS[$¢.pos]<endstmt> = 2; } {*}                    #= endstmt simple 
#S#      | \h* <.unsp>? <?before <[,:]>> {*}                         #= normal 
#S#      | <.unv>? $$
#S#          { @+MEMOS[$¢.pos]<endstmt> = 2; } {*}                    #= endstmt complex
#S#      | <.unsp>? { @+MEMOS[$¢.pos]<endargs> = 1; } {*}             #= endargs
#S#      ]
#S#  }
#S#  
#S#  token regex_block {
#S#      :my $lang = ::Regex;
#S#      :my $GOAL is context = '}';
#S#  
#S#      [ <quotepair> <.ws>
#S#          {
#S#              my $kv = $<quotepair>[*-1];
#S#              $lang = $lang.tweak($kv.<k>, $kv.<v>)
#S#                  or self.panic("Unrecognized adverb :" ~ $kv.<k> ~ '(' ~ $kv.<v> ~ ')');
#S#          }
#S#      ]*
#S#  
#S#      '{'
#S#      <nibble( $¢.cursor_fresh($lang).unbalanced('}') )>
#S#      [ '}' || <.panic: "Unable to parse regex; couldn't find right brace"> ]
#S#  
#S#      [
#S#      | <?before \h* $$>  # (usual case without comments)
#S#          { @+MEMOS[$¢.pos]<endstmt> = 2; } {*}                    #= endstmt simple 
#S#      | \h* <.unsp>? <?before <[,:]>> {*}                         #= normal 
#S#      | <.unv>? $$
#S#          { @+MEMOS[$¢.pos]<endstmt> = 2; } {*}                    #= endstmt complex
#S#      | <.unsp>? { @+MEMOS[$¢.pos]<endargs> = 1; }   {*}           #= endargs
#S#      ]
#S#  }
#S#  
#S#  # statement semantics
#S#  rule statementlist {
#S#      :my $PARSER is context<rw> = self;
#S#      :dba('statement list')
#S#      [
#S#      | $
#S#      | <?before <[\)\]\}]> >
#S#      | [<statement><eat_terminator> ]*
#S#      ]
#S#  }
#S#  
#S#  # embedded semis, context-dependent semantics
#S#  rule semilist {
#S#      :dba('semicolon list')
#S#      [
#S#      | <?before <[\)\]\}]> >
#S#      | [<statement><eat_terminator> ]*
#S#      ]
#S#  }
#S#  
#S#  
#S#  token label {
#S#      <identifier> ':' <?before \s> <.ws>
#S#  
#S#      [ <?{ $¢.is_type($<identifier>.text) }>
#S#        <.panic("You tried to use an existing typename as a label")>
#S#  #      <suppose("You tried to use an existing name $/{'identifier'} as a label")>
#S#      ]?
#S#  
#S#      # add label as a pseudo type
#S#      {{ $¢.add_type($<identifier>.text); }}
#S#  
#S#  }
#S#  
#S#  token statement {
#S#      :my $endargs is context = -1;
#S#      <!before <[\)\]\}]> >
#S#  
#S#      # this could either be a statement that follows a declaration
#S#      # or a statement that is within the block of a code declaration
#S#      <!!{ $¢ = $+PARSER.bless($¢); }>
#S#  
#S#      [
#S#      | <label> <statement>                        {*}            #= label
#S#      | <statement_control>                        {*}            #= control
#S#      | <EXPR> {*}                                                #= expr
#S#          :dba('statement end')
#S#          [
#S#          || <?{ (@+MEMOS[$¢.pos]<endstmt> // 0) == 2 }>   # no mod after end-line curly
#S#          ||
#S#              :dba('statement modifier')
#S#              <.ws>
#S#              [
#S#              | <statement_mod_loop> {*}                              #= mod loop
#S#              | <statement_mod_cond> {*}                              #= mod cond
#S#                  :dba('statement modifier loop')
#S#                  [
#S#                  || <?{ (@+MEMOS[$¢.pos]<endstmt> // 0) == 2 }>
#S#                  || <.ws> <statement_mod_loop>? {*}                  #= mod condloop
#S#                  ]
#S#              ]?
#S#          ]
#S#          {*}                                                     #= modexpr
#S#      | <?before ';'> {*}                                         #= null
#S#      ]
#S#  }
#S#  
#S#  
#S#  token eat_terminator {
#S#      [
#S#      || ';'
#S#      || <?{ @+MEMOS[$¢.pos]<endstmt> }> <.ws>
#S#      || <?terminator>
#S#      || $
#S#      || {{ if @+MEMOS[$¢.pos]<ws> { $¢.pos = @+MEMOS[$¢.pos]<ws>; } }}   # undo any line transition
#S#          <.panic: "Syntax error">
#S#      ]
#S#  }
#S#  
#S#  token statement_control:use {
#S#      <sym> :s
#S#      [
#S#      | <version>
#S#      | <module_name><arglist>?
#S#          {{
#S#              my $longname = $<module_name><longname>;
#S#              $¢.add_type($longname.text);
#S#          }}
#S#      ]
#S#  }
#S#  
#S#  
#S#  token statement_control:no {
#S#      <sym> :s
#S#      <module_name><arglist>?
#S#  }
#S#  
#S#  
#S#  token statement_control:if {
#S#      <sym> :s
#S#      <xblock>
#S#      [$<elsif> = (
#S#          'elsif'<?spacey> <xblock>       {*}                #= elsif
#S#      )]*
#S#      [$<else> = (
#S#          'else'<?spacey> <pblock>       {*}             #= else
#S#      )]?
#S#  }
#S#  
#S#  
#S#  token statement_control:unless {
#S#      <sym> :s
#S#      <xblock>
#S#      [ <!before 'else'> || <.panic: "unless does not take \"else\" in Perl 6; please rewrite using \"if\""> ]
#S#  }
#S#  
#S#  
#S#  token statement_control:while {
#S#      <sym> :s
#S#      [ <?before '(' ['my'? '$'\w+ '=']? '<' '$'?\w+ '>' ')'>   #'
#S#          <.panic: "This appears to be Perl 5 code"> ]?
#S#      <xblock>
#S#  }
#S#  
#S#  
#S#  token statement_control:until {
#S#      <sym> :s
#S#      <xblock>
#S#  }
#S#  
#S#  
#S#  token statement_control:repeat {
#S#      <sym> :s
#S#      [
#S#          | ('while'|'until')
#S#            <xblock>
#S#          | <block>                      {*}                      #= block wu
#S#            ('while'|'until') <EXPR>         {*}                      #= expr wu
#S#      ]
#S#  }
#S#  
#S#  
#S#  token statement_control:loop {
#S#      <sym> :s
#S#      $<eee> = (
#S#          '('
#S#              <e1=EXPR>? ';'   {*}                            #= e1
#S#              <e2=EXPR>? ';'   {*}                            #= e2
#S#              <e3=EXPR>?       {*}                            #= e3
#S#          ')'                      {*}                            #= eee
#S#      )?
#S#      <block>                     {*}                             #= block
#S#  }
#S#  
#S#  
#S#  token statement_control:for {
#S#      <sym> :s
#S#      [ <?before 'my'? '$'\w+ '(' >
#S#          <.panic: "This appears to be Perl 5 code"> ]?
#S#      <xblock>
#S#  }
#S#  
#S#  token statement_control:given {
#S#      <sym> :s
#S#      <xblock>
#S#  }
#S#  token statement_control:when {
#S#      <sym> :s
#S#      <xblock>
#S#  }
#S#  rule statement_control:default {<sym> <block> }
#S#  
#S#  rule statement_control:BEGIN   {<sym> <block> }
#S#  rule statement_control:CHECK   {<sym> <block> }
#S#  rule statement_control:INIT    {<sym> <block> }
#S#  rule statement_control:END     {<sym> <block> }
#S#  rule statement_control:START   {<sym> <block> }
#S#  rule statement_control:ENTER   {<sym> <block> }
#S#  rule statement_control:LEAVE   {<sym> <block> }
#S#  rule statement_control:KEEP    {<sym> <block> }
#S#  rule statement_control:UNDO    {<sym> <block> }
#S#  rule statement_control:FIRST   {<sym> <block> }
#S#  rule statement_control:NEXT    {<sym> <block> }
#S#  rule statement_control:LAST    {<sym> <block> }
#S#  rule statement_control:PRE     {<sym> <block> }
#S#  rule statement_control:POST    {<sym> <block> }
#S#  rule statement_control:CATCH   {<sym> <block> }
#S#  rule statement_control:CONTROL {<sym> <block> }
#S#  rule statement_control:TEMP    {<sym> <block> }
#S#  
#S#  rule term:BEGIN   {<sym> <block> }
#S#  rule term:CHECK   {<sym> <block> }
#S#  rule term:INIT    {<sym> <block> }
#S#  rule term:START   {<sym> <block> }
#S#  rule term:ENTER   {<sym> <block> }
#S#  rule term:FIRST   {<sym> <block> }
#S#  
#S#  rule modifier_expr { <EXPR> }
#S#  
#S#  rule statement_mod_cond:if     {<sym> <modifier_expr> {*} }     #= if
#S#  rule statement_mod_cond:unless {<sym> <modifier_expr> {*} }     #= unless
#S#  rule statement_mod_cond:when   {<sym> <modifier_expr> {*} }     #= when
#S#  
#S#  rule statement_mod_loop:while {<sym> <modifier_expr> {*} }      #= while
#S#  rule statement_mod_loop:until {<sym> <modifier_expr> {*} }      #= until
#S#  
#S#  rule statement_mod_loop:for   {<sym> <modifier_expr> {*} }      #= for
#S#  rule statement_mod_loop:given {<sym> <modifier_expr> {*} }      #= given
#S#  
#S#  token module_name:normal {
#S#      <longname>
#S#      [ :dba('generic role') <?{ ($+PKGDECL//'') eq 'role' }> '[' ~ ']' <signature> ]?
#S#  }
#S#  
#S#  token module_name:deprecated { 'v6-alpha' }
#S#  
#S#  token vnum {
#S#      \d+ | '*'
#S#  }
#S#  
#S#  token version:sym<v> {
#S#      'v' <?before \d> :: <vnum> ** '.' '+'?
#S#  }
#S#  
#S#  ###################################################
#S#  
#S#  token PRE {
#S#      :dba('prefix or meta-prefix')
#S#      [
#S#      | <prefix>
#S#          { $<O> = $<prefix><O>; $<sym> = $<prefix><sym> }
#S#                                                      {*}         #= prefix
#S#      | <prefix_circumfix_meta_operator>
#S#          { $<O> = $<prefix_circumfix_meta_operator><O>; $<sym> = $<prefix_circumfix_meta_operator>.text }
#S#                                                      {*}         #= precircum
#S#      ]
#S#      # XXX assuming no precedence change
#S#      
#S#      <prefix_postfix_meta_operator>*                 {*}         #= prepost
#S#      <.ws>
#S#  }
#S#  
#S#  # (for when you want to tell EXPR that infix already parsed the term)
#S#  token nullterm {
#S#      <?>
#S#  }
#S#  
#S#  token nulltermish {
#S#      :dba('null term')
#S#      [
#S#      | <?stdstopper>
#S#      | <termish>?
#S#      ]
#S#  }
token nulltermish {
    :dba('null term')
    [
    | <?stdstopper>
    | <termish>?
    ]
}
#S#  
#S#  token termish {
#S#      :dba('prefix or noun')
#S#      [
#S#      | <PRE>+ <noun>
#S#      | <noun>
#S#      ]
#S#  
#S#      # also queue up any postfixes
#S#      :dba('postfix')
#S#      [ <?stdstopper> ||
#S#          <POST>*
#S#      ]
#S#  }
token termish {
    :dba('prefix or noun')
    [
    | <PRE>+ <noun>
    | <noun>
    ]

    # also queue up any postfixes
    :dba('postfix')
#XXXTODO    [ <?stdstopper> ||
#XXXTODO        <POST>*
#XXXTODO    ]
}
#S#  
#S#  token noun {
#S#      [
#S#      | <fatarrow>
#S#      | <variable>
#S#      | <package_declarator>
#S#      | <scope_declarator>
#S#      | <?before 'multi'|'proto'|'only'> <multi_declarator>
#S#      | <routine_declarator>
#S#      | <regex_declarator>
#S#      | <type_declarator>
#S#      | <circumfix>
#S#      | <dotty>
#S#      | <value>
#S#      | <capterm>
#S#      | <sigterm>
#S#      | <term>
#S#      | <statement_prefix>
#S#      | [ <colonpair> <.ws> ]+
#S#      ]
#S#  }
token noun {
    [
    | <fatarrow>
    | <variable>
    | <package_declarator>
    | <scope_declarator>
#XXXTODO?    | <?before 'multi'|'proto'|'only'> <multi_declarator>
    | <routine_declarator>
    | <regex_declarator>
    | <type_declarator>
    | <circumfix>
    | <dotty>
    | <value>
    | <capterm>
    | <sigterm>
    | <term>
    | <statement_prefix>
#XXXTODO?    | [ <colonpair> <.ws> ]+
    ]
}
#S#  
#S#  
#S#  token fatarrow {
#S#      <key=identifier> \h* '=>' <.ws> <val=EXPR(item %item_assignment)>
#S#  }
#S#  
#S#  token colonpair {
#S#      :my $key;
#S#      :my $value;
#S#  
#S#      ':'
#S#      :dba('colon pair')
#S#      [
#S#      | '!' <identifier>
#S#          { $key = $<identifier>.text; $value = 0; }
#S#          {*}                                                     #= false
#S#      | $<num> = [\d+] <identifier>
#S#      | <identifier>
#S#          { $key = $<identifier>.text; }
#S#          [
#S#          || <.unsp>? '.'? <postcircumfix> { $value = $<postcircumfix>; }
#S#          || { $value = 1; }
#S#          ]
#S#          {*}                                                     #= value
#S#      | :dba('signature') '(' ~ ')' <signature>
#S#      | <postcircumfix>
#S#          { $key = ""; $value = $<postcircumfix>; }
#S#          {*}                                                     #= structural
#S#      | $<var> = (<sigil> {} <twigil>? <desigilname>)
#S#          { $key = $<var><desigilname>.text; $value = $<var>; }
#S#          {*}                                                     #= varname
#S#      ]
#S#      { $<k> = $key; $<v> = $value; }
#S#  }
#S#  
#S#  token quotepair {
#S#      :my $key;
#S#      :my $value;
#S#  
#S#      ':'
#S#      :dba('colon pair (restricted)')
#S#      [
#S#      | '!' <identifier>
#S#          { $key = $<identifier>.text; $value = 0; }
#S#          {*}                                                     #= false
#S#      | <identifier>
#S#          { $key = $<identifier>.text; }
#S#          [
#S#          || <.unsp>? '.'? <?before '('> <postcircumfix> { $value = $<postcircumfix>; }
#S#          || { $value = 1; }
#S#          ]
#S#          {*}                                                     #= value
#S#      | $<n>=(\d+) $<id>=(<[a..z]>+)
#S#          { $key = $<id>.text; $value = $<n>.text; }
#S#          {*}                                                     #= nth
#S#      ]
#S#      { $<k> = $key; $<v> = $value; }
#S#  }
#S#  
#S#  token infixish {
#S#      <!stdstopper>
#S#      <!infixstopper>
#S#      :dba('infix or meta-infix')
#S#      [
#S#      | <colonpair> {
#S#              $<fake> = 1;
#S#              $<sym> = ':';
#S#              %<O><prec> = %loose_unary<prec>;
#S#              %<O><assoc> = 'left';
#S#          }
#S#      | <infix>
#S#             { $<O> = $<infix>.<O>; $<sym> = $<infix>.<sym>; }
#S#      | <infix_prefix_meta_operator>
#S#          { $<O> = $<infix_prefix_meta_operator><O>;
#S#            $<sym> = $<infix_prefix_meta_operator><sym>; }
#S#      | <infix_circumfix_meta_operator>
#S#          { $<O> = $<infix_circumfix_meta_operator><O>;
#S#            $<sym> = $<infix_circumfix_meta_operator><sym>; }
#S#      | <infix> <?before '='> <infix_postfix_meta_operator($<infix>)>
#S#             { $<O> = $<infix_postfix_meta_operator>.<O>; $<sym> = $<infix_postfix_meta_operator>.<sym>; }
#S#      ]
#S#  }
#S#  
#S#  # doing fancy as one rule simplifies LTM
#S#  token dotty:sym<.*> ( --> Methodcall) {
#S#      ('.' [ <[+*?=:]> | '^' '!'? ]) :: <.unspacey> <dottyop>
#S#      { $<sym> = $0.item; }
#S#  }
#S#  
#S#  token dotty:sym<.> ( --> Methodcall) {
#S#      <sym> <dottyop>
#S#  }
#S#  
#S#  token privop ( --> Methodcall) {
#S#      '!' <methodop>
#S#  }
#S#  
#S#  token dottyop {
#S#      :dba('dotty method or postfix')
#S#      [
#S#      | <methodop>
#S#      | <!alpha> <postop> # only non-alpha postfixes have dotty form
#S#      ]
#S#  }
#S#  
#S#  # Note, this rule mustn't do anything irreversible because it's used
#S#  # as a lookahead by the quote interpolator.
#S#  
#S#  token POST {
#S#      <!stdstopper>
#S#  
#S#      # last whitespace didn't end here
#S#      <!{ @+MEMOS[$¢.pos]<ws> }>
#S#  
#S#      [ <.unsp> | '\\' ]?
#S#  
#S#      [ ['.' <.unsp>?]? <postfix_prefix_meta_operator> <.unsp>? ]*
#S#  
#S#      :dba('postfix')
#S#      [
#S#      | <dotty>  { $<O> = $<dotty><O> }
#S#      | <privop> { $<O> = $<privop><O> }
#S#      | <postop> { $<O> = $<postop><O> }
#S#      ]
#S#      { $+SIGIL = '@' }
#S#  }
#S#  
#S#  regex prefix_circumfix_meta_operator:reduce (--> List_prefix) {
#S#      $<s> = (
#S#          '['
#S#          [
#S#          | <op=infix> ']' ['«'|<?>]
#S#          | <op=infix_prefix_meta_operator> ']' ['«'|<?>]
#S#          | <op=infix_circumfix_meta_operator> ']' ['«'|<?>]
#S#          | \\<op=infix> ']' ['«'|<?>]
#S#          | \\<op=infix_prefix_meta_operator> ']' ['«'|<?>]
#S#          | \\<op=infix_circumfix_meta_operator> ']' ['«'|<?>]
#S#          ]
#S#      ) <?before \s | '(' >
#S#  
#S#      { $<O> = $<s><op><O>; $<sym> = $<s>.text; }
#S#  
#S#      [ <!{ $<O><assoc> eq 'non' }>
#S#          || <.panic: "Can't reduce a non-associative operator"> ]
#S#  
#S#      [ <!{ $<O><prec> eq %conditional<prec> }>
#S#          || <.panic: "Can't reduce a conditional operator"> ]
#S#  
#S#      { $<O><assoc> = 'unary'; }
#S#  
#S#  }
#S#  
#S#  token prefix_postfix_meta_operator:sym< « >    { <sym> | '<<' }
#S#  
#S#  token postfix_prefix_meta_operator:sym< » >    { <sym> | '>>' }
#S#  
#S#  token infix_prefix_meta_operator:sym<!> ( --> Chaining) {
#S#      <sym> <!before '!'> <infix>
#S#  
#S#      <!!{ $<O> = $<infix><O>; }>
#S#      <!!lex1: 'negation'>
#S#  
#S#      [
#S#      || <!!{ $<O><assoc> eq 'chain'}>
#S#      || <!!{ $<O><assoc> and $<O><bool> }>
#S#      || <.panic: "Only boolean infix operators may be negated">
#S#      ]
#S#  
#S#      <!{ $<O><hyper> and $¢.panic("Negation of hyper operator not allowed") }>
#S#  
#S#  }
#S#  
#S#  method lex1 (Str $s) {
#S#      self.<O>{$s}++ and self.panic("Nested $s metaoperators not allowed");
#S#      self;
#S#  }
#S#  
#S#  token infix_circumfix_meta_operator:sym<X X> ( --> List_infix) {
#S#      X [
#S#      | <infix> X
#S#      | <infix=infix_prefix_meta_operator> X
#S#      | <infix=infix_curcumfix_meta_operator> X
#S#      ]
#S#      <!!{ $<O> = $<infix><O>; }>
#S#      <!!lex1: 'cross'>
#S#  }
#S#  
#S#  token infix_circumfix_meta_operator:sym<« »> ( --> Hyper) {
#S#      [
#S#      | '«' <infix> [ '«' | '»' ]
#S#      | '»' <infix> [ '«' | '»' ]
#S#      | '<<' <infix> [ '<<' | '>>' ]
#S#      | '>>' <infix> [ '<<' | '>>' ]
#S#      ]
#S#      <!!{ $<O> := $<infix><O>; }>
#S#      <!!lex1: 'hyper'>
#S#  }
#S#  
#S#  token infix_postfix_meta_operator:sym<=> ($op --> Item_assignment) {
#S#      '='
#S#      { $<O> = $op<O>; }
#S#      <?lex1: 'assignment'>
#S#  
#S#      [ <?{ ($<O><assoc> // '') eq 'chain' }> <.panic: "Can't make assignment op of boolean operator"> ]?
#S#      [ <?{ ($<O><assoc> // '') eq 'non'   }> <.panic: "Can't make assignment op of non-associative operator"> ]?
#S#  }
#S#  
#S#  token postcircumfix:sym<( )> ( --> Methodcall)
#S#      { :dba('argument list') '(' ~ ')' <semilist> }
#S#  
#S#  token postcircumfix:sym<[ ]> ( --> Methodcall)
#S#      { :dba('subscript') '[' ~ ']' <semilist> }
#S#  
#S#  token postcircumfix:sym<{ }> ( --> Methodcall)
#S#      { :dba('subscript') '{' ~ '}' <semilist> }
#S#  
#S#  token postcircumfix:sym«< >» ( --> Methodcall)
#S#      { '<' <nibble($¢.cursor_fresh( ::STD::Q ).tweak(:q).tweak(:w).balanced('<','>'))> [ '>' || <.panic: "Unable to parse quote-words subscript; couldn't find right angle quote"> ] }
#S#  
#S#  token postcircumfix:sym«<< >>» ( --> Methodcall)
#S#      { '<<' <nibble($¢.cursor_fresh( ::STD::Q ).tweak(:qq).tweak(:ww).balanced('<<','>>'))> [ '>>' || <.panic: "Unable to parse quote-words subscript; couldn't find right double-angle quote"> ] }
#S#  
#S#  token postcircumfix:sym<« »> ( --> Methodcall)
#S#      { '«' <nibble($¢.cursor_fresh( ::STD::Q ).tweak(:qq).tweak(:ww).balanced('«','»'))> [ '»' || <.panic: "Unable to parse quote-words subscript; couldn't find right double-angle quote"> ] }
#S#  
#S#  token postop {
#S#      | <postfix>         { $<O> := $<postfix><O> }
#S#      | <postcircumfix>   { $<O> := $<postcircumfix><O> }
#S#  }
#S#  
#S#  token methodop {
#S#      [
#S#      | <longname>
#S#      | <?before '$' | '@' > <variable>
#S#      | <?before <[ ' " ]> > <quote>
#S#          { $<quote> ~~ /\W/ or $¢.panic("Useless use of quotes") }
#S#      ] <.unsp>? 
#S#  
#S#      :dba('method arguments')
#S#      [
#S#      | '.'? <.unsp>? '(' ~ ')' <semilist>
#S#      | ':' <?before \s> <!{ $+inquote }> <arglist>
#S#      ]?
#S#  }
#S#  
#S#  token arglist {
#S#      :my StrPos $endargs is context<rw> = 0;
#S#      :my $GOAL is context = 'endargs';
#S#      <.ws>
#S#      :dba('argument list')
#S#      [
#S#      | <?stdstopper>
#S#      | <EXPR(item %list_prefix)>
#S#      ]
#S#  }
#S#  
#S#  token circumfix:sym<{ }> ( --> Term) {
#S#      <?before '{' | <lambda> > <pblock>
#S#  }
#S#  
#S#  token variable_declarator {
#S#      :my $IN_DECL is context<rw> = 1;
#S#      <variable>
#S#      { $IN_DECL = 0; }
#S#      [   # Is it a shaped array or hash declaration?
#S#        #  <?{ $<sigil> eq '@' | '%' }>
#S#          <.unsp>?
#S#          $<shape> = [
#S#          | '(' ~ ')' <signature>
#S#          | :dba('shape definition') '[' ~ ']' <semilist>
#S#          | :dba('shape definition') '{' ~ '}' <semilist>
#S#          | <?before '<'> <postcircumfix>
#S#          ]*
#S#      ]?
#S#      <.ws>
#S#  
#S#      <trait>*
#S#  
#S#      <post_constraint>*
#S#  }
#S#  
#S#  rule scoped {
#S#      :dba('scoped declarator')
#S#      [
#S#      | <declarator>
#S#      | <regex_declarator>
#S#      | <package_declarator>
#S#      | <fulltypename>+ <multi_declarator>
#S#      | <multi_declarator>
#S#  #    | <?before <[A..Z]> > <name> <.panic("Apparent type name " ~ $<name>.text ~ " is not declared yet")>
#S#      ]
#S#  }
#S#  
#S#  
#S#  token scope_declarator:my       { <sym> <scoped> }
#S#  token scope_declarator:our      { <sym> <scoped> }
#S#  token scope_declarator:state    { <sym> <scoped> }
#S#  token scope_declarator:constant { <sym> <scoped> }
#S#  token scope_declarator:has      { <sym> <scoped> }
#S#  
#S#  
#S#  token package_declarator:class {
#S#      :my $PKGDECL is context = 'class';
#S#      <sym> <package_def>
#S#  }
#S#  
#S#  token package_declarator:grammar {
#S#      :my $PKGDECL is context = 'grammar';
#S#      <sym> <package_def>
#S#  }
#S#  
#S#  token package_declarator:module {
#S#      :my $PKGDECL is context = 'module';
#S#      <sym> <package_def>
#S#  }
#S#  
#S#  token package_declarator:package {
#S#      :my $PKGDECL is context = 'package';
#S#      <sym> <package_def>
#S#  }
#S#  
#S#  token package_declarator:role {
#S#      :my $PKGDECL is context = 'role';
#S#      <sym> <package_def>
#S#  }
#S#  
#S#  token package_declarator:knowhow {
#S#      :my $PKGDECL is context = 'knowhow';
#S#      <sym> <package_def>
#S#  }
#S#  
#S#  token package_declarator:require {   # here because of declarational aspects
#S#      <sym> <.ws>
#S#      [
#S#      || <module_name> <EXPR>?
#S#      || <EXPR>
#S#      ]
#S#  }
#S#  
#S#  token package_declarator:trusts {
#S#      <sym> <.ws>
#S#      <module_name>
#S#  }
#S#  
#S#  token package_declarator:does {
#S#      <sym> <.ws>
#S#      <typename>
#S#  }
#S#  
#S#  rule package_def {
#S#      :my $longname;
#S#      [
#S#          <module_name>{
#S#              $longname = $<module_name>[0]<longname>;
#S#              $¢.add_type($longname.text);
#S#          }
#S#      ]?
#S#      <trait>*
#S#      [
#S#         <?before '{'>
#S#         {{
#S#             # figure out the actual full package name (nested in outer package)
#S#              my $pkg = $+PKG // "GLOBAL";
#S#              push @PKGS, $pkg;
#S#              if $longname {
#S#                  my $shortname = $longname.<name>.text;
#S#                  $+PKG = $pkg ~ '::' ~ $shortname;
#S#              }
#S#              else {
#S#                  $+PKG = $pkg ~ '::_anon_';
#S#              }
#S#          }}
#S#          <block>
#S#          {{
#S#              $+PKG = pop(@PKGS);
#S#          }}
#S#          {*}                                                     #= block
#S#      || <?{ $+begin_compunit }> {} <?before ';'>
#S#          {
#S#              $longname orelse $¢.panic("Compilation unit cannot be anonymous");
#S#              my $shortname = $longname.<name>.text;
#S#              $+PKG = $shortname;
#S#              $+begin_compunit = 0;
#S#          }
#S#          {*}                                                     #= semi
#S#      || <.panic: "Unable to parse " ~ $+PKGDECL ~ " definition">
#S#      ]
#S#  }
#S#  
#S#  token declarator {
#S#      [
#S#      | <variable_declarator>
#S#      | '(' ~ ')' <signature> <trait>*
#S#      | <routine_declarator>
#S#      | <regex_declarator>
#S#      | <type_declarator>
#S#      ]
#S#  }
#S#  
#S#  token multi_declarator:multi { <sym> <.ws> [ <declarator> || <routine_def> ] }
#S#  token multi_declarator:proto { <sym> <.ws> [ <declarator> || <routine_def> ] }
#S#  token multi_declarator:only  { <sym> <.ws> [ <declarator> || <routine_def> ] }
#S#  token multi_declarator:null  { <declarator> }
#S#  
#S#  token routine_declarator:sub       { <sym> <routine_def> }
#S#  token routine_declarator:method    { <sym> <method_def> }
#S#  token routine_declarator:submethod { <sym> <method_def> }
#S#  token routine_declarator:macro     { <sym> <macro_def> }
#S#  
#S#  token regex_declarator:regex { <sym>       <regex_def> }
#S#  token regex_declarator:token { <sym>       <regex_def> }
#S#  token regex_declarator:rule  { <sym>       <regex_def> }
#S#  
#S#  # Most of these special variable rules are there simply to catch old p5 brainos
#S#  
#S#  token special_variable:sym<$¢> { <sym> }
#S#  
#S#  token special_variable:sym<$!> { <sym> <!before \w> }
#S#  
#S#  token special_variable:sym<$!{ }> {
#S#      # XXX the backslashes are necessary here for bootstrapping, not for P6...
#S#      ( '$!{' :: (.*?) '}' )
#S#      <obs($0.text ~ " variable", 'smart match against $!')>
#S#  }
#S#  
#S#  token special_variable:sym<$/> {
#S#      <sym>
#S#      # XXX assuming nobody ever wants to assign $/ directly anymore...
#S#      [ <?before \h* '=' <![=]> >
#S#          <obs('$/ variable as input record separator',
#S#               "filehandle's :irs attribute")>
#S#      ]?
#S#  }
#S#  
#S#  token special_variable:sym<$~> {
#S#      <sym> :: <?before \s | ',' | '=' | <terminator> >
#S#      <obs('$~ variable', 'Form module')>
#S#  }
#S#  
#S#  token special_variable:sym<$`> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('$` variable', 'explicit pattern before <(')>
#S#  }
#S#  
#S#  token special_variable:sym<$@> {
#S#      <sym> ::
#S#      <obs('$@ variable as eval error', '$!')>
#S#  }
#S#  
#S#  token special_variable:sym<$#> {
#S#      <sym> ::
#S#      [
#S#      || (\w+) <obs("\$#" ~ $0.text ~ " variable", "\@{" ~ $0.text ~ "}.end")>
#S#      || <obs('$# variable', '.fmt')>
#S#      ]
#S#  }
#S#  token special_variable:sym<$$> {
#S#      <sym> <!alpha> :: <?before \s | ',' | <terminator> >
#S#      <obs('$$ variable', '$*PID')>
#S#  }
#S#  token special_variable:sym<$%> {
#S#      <sym> ::
#S#      <obs('$% variable', 'Form module')>
#S#  }
#S#  
#S#  # Note: this works because placeholders are restricted to lowercase
#S#  token special_variable:sym<$^X> {
#S#      <sigil> '^' $<letter> = [<[A..Z]>] \W
#S#      <obscaret($<sigil>.text ~ '^' ~ $<letter>.text, $<sigil>, $<letter>.text)>
#S#  }
#S#  
#S#  token special_variable:sym<$^> {
#S#      <sym> :: <?before \s | ',' | '=' | <terminator> >
#S#      <obs('$^ variable', 'Form module')>
#S#  }
#S#  
#S#  token special_variable:sym<$&> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('$& variable', '$/ or $()')>
#S#  }
#S#  
#S#  token special_variable:sym<$*> {
#S#      <sym> :: <?before \s | ',' | '=' | <terminator> >
#S#      <obs('$* variable', '^^ and $$')>
#S#  }
#S#  
#S#  token special_variable:sym<$)> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('$) variable', '$*EGID')>
#S#  }
#S#  
#S#  token special_variable:sym<$-> {
#S#      <sym> :: <?before \s | ',' | '=' | <terminator> >
#S#      <obs('$- variable', 'Form module')>
#S#  }
#S#  
#S#  token special_variable:sym<$=> {
#S#      <sym> :: <?before \s | ',' | '=' | <terminator> >
#S#      <obs('$= variable', 'Form module')>
#S#  }
#S#  
#S#  token special_variable:sym<@+> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('@+ variable', '.to method')>
#S#  }
#S#  
#S#  token special_variable:sym<%+> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('%+ variable', '.to method')>
#S#  }
#S#  
#S#  token special_variable:sym<$+[ ]> {
#S#      '$+['
#S#      <obs('@+ variable', '.to method')>
#S#  }
#S#  
#S#  token special_variable:sym<@+[ ]> {
#S#      '@+['
#S#      <obs('@+ variable', '.to method')>
#S#  }
#S#  
#S#  token special_variable:sym<@+{ }> {
#S#      '@+{'
#S#      <obs('%+ variable', '.to method')>
#S#  }
#S#  
#S#  token special_variable:sym<@-> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('@- variable', '.from method')>
#S#  }
#S#  
#S#  token special_variable:sym<%-> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('%- variable', '.from method')>
#S#  }
#S#  
#S#  token special_variable:sym<$-[ ]> {
#S#      '$-['
#S#      <obs('@- variable', '.from method')>
#S#  }
#S#  
#S#  token special_variable:sym<@-[ ]> {
#S#      '@-['
#S#      <obs('@- variable', '.from method')>
#S#  }
#S#  
#S#  token special_variable:sym<%-{ }> {
#S#      '@-{'
#S#      <obs('%- variable', '.from method')>
#S#  }
#S#  
#S#  token special_variable:sym<$+> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('$+ variable', 'Form module')>
#S#  }
#S#  
#S#  token special_variable:sym<${^ }> {
#S#      ( <sigil> '{^' :: (.*?) '}' )
#S#      <obscaret($0.text, $<sigil>, $0.{0}.text)>
#S#  }
#S#  
#S#  # XXX should eventually rely on multi instead of nested cases here...
#S#  method obscaret (Str $var, Str $sigil, Str $name) {
#S#      my $repl = do { given $sigil {
#S#          when '$' {
#S#              given $name {
#S#                  when 'MATCH'         { '$/' }
#S#                  when 'PREMATCH'      { 'an explicit pattern before <(' }
#S#                  when 'POSTMATCH'     { 'an explicit pattern after )>' }
#S#                  when 'ENCODING'      { '$?ENCODING' }
#S#                  when 'UNICODE'       { '$?UNICODE' }  # XXX ???
#S#                  when 'TAINT'         { '$*TAINT' }
#S#                  when 'OPEN'          { 'filehandle introspection' }
#S#                  when 'N'             { '$-1' } # XXX ???
#S#                  when 'L'             { 'Form module' }
#S#                  when 'A'             { 'Form module' }
#S#                  when 'E'             { '$!.extended_os_error' }
#S#                  when 'C'             { 'COMPILING namespace' }
#S#                  when 'D'             { '$*DEBUGGING' }
#S#                  when 'F'             { '$*SYSTEM_FD_MAX' }
#S#                  when 'H'             { '$?FOO variables' }
#S#                  when 'I'             { '$*INPLACE' } # XXX ???
#S#                  when 'O'             { '$?OS or $*OS' }
#S#                  when 'P'             { 'whatever debugger Perl 6 comes with' }
#S#                  when 'R'             { 'an explicit result variable' }
#S#                  when 'S'             { 'the context function' } # XXX ???
#S#                  when 'T'             { '$*BASETIME' }
#S#                  when 'V'             { '$*PERL_VERSION' }
#S#                  when 'W'             { '$*WARNING' }
#S#                  when 'X'             { '$*EXECUTABLE_NAME' }
#S#                  when *               { "a global form such as $sigil*$name" }
#S#              }
#S#          }
#S#          when '%' {
#S#              given $name {
#S#                  when 'H'             { '$?FOO variables' }
#S#                  when *               { "a global form such as $sigil*$name" }
#S#              }
#S#          }
#S#          when * { "a global form such as $sigil*$name" }
#S#      };
#S#      };
#S#      return self.obs("$var variable", $repl);
#S#  }
#S#  
#S#  token special_variable:sym<::{ }> {
#S#      '::' <?before '{'>
#S#  }
#S#  
#S#  token special_variable:sym<${ }> {
#S#      ( <[$@%]> '{' :: (.*?) '}' )
#S#      <obs("" ~ $0.text ~ " variable", "{" ~ $<sigil>.text ~ "}(" ~ $0.{0}.text ~ ")")>
#S#  }
#S#  
#S#  token special_variable:sym<$[> {
#S#      <sym> :: <?before \s | ',' | '=' | <terminator> >
#S#      <obs('$[ variable', 'user-defined array indices')>
#S#  }
#S#  
#S#  token special_variable:sym<$]> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('$] variable', '$*PERL_VERSION')>
#S#  }
#S#  
#S#  token special_variable:sym<$\\> {
#S#      <sym> :: <?before \s | ',' | '=' | <terminator> >
#S#      <obs('$\\ variable', "the filehandle's :ors attribute")>
#S#  }
#S#  
#S#  token special_variable:sym<$|> {
#S#      <sym> :: <?before \s | ',' | '=' | <terminator> >
#S#      <obs('$| variable', 'Form module')>
#S#  }
#S#  
#S#  token special_variable:sym<$:> {
#S#      <sym> <?before <[\x20\t\n\],=)}]> >
#S#      <obs('$: variable', 'Form module')>
#S#  }
#S#  
#S#  token special_variable:sym<$;> {
#S#      <sym> :: <?before \s | ',' | '=' | <terminator> >
#S#      <obs('$; variable', 'real multidimensional hashes')>
#S#  }
#S#  
#S#  token special_variable:sym<$'> { #'
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('$' ~ "'" ~ 'variable', "explicit pattern after )\x3E")>
#S#  }
#S#  
#S#  token special_variable:sym<$"> {
#S#      <sym> :: <?before \s | ',' | '=' | <terminator> >
#S#      <obs('$" variable', '.join() method')>
#S#  }
#S#  
#S#  token special_variable:sym<$,> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('$, variable', ".join() method")>
#S#  }
#S#  
#S#  token special_variable:sym['$<'] {
#S#      <sym> :: <!before \s* \w+ \s* '>' >
#S#      <obs('$< variable', '$*UID')>
#S#  }
#S#  
#S#  token special_variable:sym«\$>» {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs("$() variable", '$*EUID')>
#S#  }
#S#  
#S#  token special_variable:sym<$.> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('$. variable', "filehandle's .line method")>
#S#  }
#S#  
#S#  token special_variable:sym<$?> {
#S#      <sym> :: <?before \s | ',' | <terminator> >
#S#      <obs('$? variable as child error', '$!')>
#S#  }
#S#  
#S#  # desigilname should only follow a sigil/twigil
#S#  
#S#  token desigilname {
#S#      [
#S#      | <?before '$' > <variable>
#S#      | <longname>
#S#      ]
#S#  }
#S#  
#S#  token variable {
#S#      <?before <sigil> { $+SIGIL ||= $<sigil>.text } > {}
#S#      [
#S#      || '&' <twigil>?  <sublongname> {*}                                   #= subnoun
#S#      || <?before '$::('> '$' <name>?
#S#      || '$::' <name>? # XXX
#S#      || '$:' <name>? # XXX
#S#      || [
#S#          | <sigil> <twigil>? <desigilname> {*}                                    #= desigilname
#S#              [ <?{ my $t = $<twigil>; @$t and $t.[0].text eq '.' }>
#S#                  <.unsp>? <?before '('> <postcircumfix> {*}          #= methcall
#S#              ]?
#S#          | <special_variable> {*}                                    #= special
#S#          | <sigil> \d+ {*}                                           #= $0
#S#          # Note: $() can also parse as contextualizer in an expression; should have same effect
#S#          | <sigil> <?before '<' | '('> <postcircumfix> {*}           #= $()
#S#          | <sigil> <?{ $+IN_DECL }> {*}                              #= anondecl
#S#          ]
#S#      ]
#S#  }
#S#  
#S#  # Note, don't reduce on a bare sigil unless you don't want a twigil or
#S#  # you otherwise don't care what the longest token is.
#S#  
#S#  token sigil:sym<$>  { <sym> }
#S#  token sigil:sym<@@> { <sym> }
#S#  token sigil:sym<@>  { <sym> }
#S#  token sigil:sym<%>  { <sym> }
#S#  token sigil:sym<&>  { <sym> }
#S#  
#S#  token twigil:sym<.> { <sym> }
#S#  token twigil:sym<!> { <sym> }
#S#  token twigil:sym<^> { <sym> }
#S#  token twigil:sym<:> { <sym> <!before ':'> }
#S#  token twigil:sym<*> { <sym> }
#S#  token twigil:sym<+> { <sym> }
#S#  token twigil:sym<?> { <sym> }
#S#  token twigil:sym<=> { <sym> }
#S#  
#S#  token deflongname {
#S#      :dba('name to be defined')
#S#      <name>
#S#      # XXX too soon
#S#      [ <colonpair>+ { $¢.add_macro($<name>) if $+IN_DECL; } ]?
#S#      { $¢.add_routine($<name>.text) if $+IN_DECL; }
#S#  }
#S#  
#S#  token longname {
#S#      <name> <colonpair>*
#S#  }
#S#  
#S#  token name {
#S#      [
#S#      | <identifier> <morename>*
#S#      | <morename>+
#S#      ]
#S#  }
#S#  
#S#  token morename {
#S#      '::'
#S#      [
#S#          <?before '(' | <alpha> >
#S#          [
#S#          | <identifier>
#S#          | :dba('indirect name') '(' ~ ')' <EXPR>
#S#          ]
#S#      ]?
#S#  }
#S#  
#S#  token subshortname {
#S#      [
#S#      | <category>
#S#          [ <colonpair>+ { $¢.add_macro($<category>) if $+IN_DECL; } ]?
#S#      | <desigilname> { $¢.add_routine($<desigilname>.text) if $+IN_DECL; }
#S#      ]
#S#  }
#S#  
#S#  token sublongname {
#S#      <subshortname> <sigterm>?
#S#  }
#S#  
#S#  #token subcall {
#S#  #    # XXX should this be sublongname?
#S#  #    <subshortname> <.unsp>? '.'? '(' ~ ')' <semilist>
#S#  #    {*}
#S#  #}
#S#  
#S#  #token packagevar {
#S#  #    # Note: any ::() are handled within <name>, and subscript must be final part.
#S#  #    # A bare ::($foo) is not considered a variable, but ::($foo)::<$bar> is.
#S#  #    # (The point being that we want a sigil either first or last but not both.)
#S#  #    <?before [\w+] ** '::' [ '<' | '«' | '{' ]> ::
#S#  #    <name> '::' <postcircumfix> {*}                            #= FOO::<$x>
#S#  #}
#S#  
#S#  token value {
#S#      [
#S#      | <quote>
#S#      | <number>
#S#      | <version>
#S#  #    | <packagevar>     # XXX see term:name for now
#S#  #    | <fulltypename>   # XXX see term:name for now
#S#      ]
#S#  }
token value {
    [
    | <quote>
    | <number>
    | <version>
#    | <packagevar>     # XXX see term:name for now
#    | <fulltypename>   # XXX see term:name for now
    ]
}
#S#  
#S#  token typename {
#S#      [
#S#      | '::?'<identifier>                 # parse ::?CLASS as special case
#S#      | <longname>
#S#        <?{{
#S#          my $longname = $<longname>.text;
#S#          if substr($longname, 0, 2) eq '::' {
#S#              $¢.add_type(substr($longname, 2));
#S#          }
#S#          else {
#S#              $¢.is_type($longname)
#S#          }
#S#        }}>
#S#      ]
#S#      # parametric type?
#S#      <.unsp>? [ <?before '['> <postcircumfix> ]?
#S#  }
#S#  
#S#  rule fulltypename {<typename>['|'<typename>]*
#S#      [ of <fulltypename> ]?
#S#  }
#S#  
#S#  token number {
#S#      [
#S#      | <dec_number>
#S#      | <integer>
#S#      | <rad_number>
#S#      ]
#S#  }
token number {
    [
    | <dec_number>
    | <integer>
    | <rad_number>
    ]
}
#S#  
#S#  token integer {
#S#      [
#S#      | 0 [ b <[01]>+           [ _ <[01]>+ ]*
#S#          | o <[0..7]>+         [ _ <[0..7]>+ ]*
#S#          | x <[0..9a..fA..F]>+ [ _ <[0..9a..fA..F]>+ ]*
#S#          | d \d+               [ _ \d+]*
#S#          | \d+[_\d+]*
#S#              { $¢.worry("Leading 0 does not indicate octal in Perl 6") }
#S#          ]
#S#      | \d+[_\d+]*
#S#      ]
#S#  }
token integer {
    [
    | 0 [ b <[01]>+           [ _ <[01]>+ ]*
        | o <[0..7]>+         [ _ <[0..7]>+ ]*
        | x <[0..9a..fA..F]>+ [ _ <[0..9a..fA..F]>+ ]*
        | d \d+               [ _ \d+]*
        | \d+[_\d+]*
            { $¢.worry("Leading 0 does not indicate octal in Perl 6") }
        ]
    | \d+[_\d+]*
    ]
}
#S#  
#S#  token radint {
#S#      [
#S#      | <integer>
#S#      | <?before ':'> <rad_number> <?{
#S#                          defined $<rad_number><intpart>
#S#                          and
#S#                          not defined $<rad_number><fracpart>
#S#                      }>
#S#      ]
#S#  }
#S#  
#S#  token escale {
#S#      <[Ee]> <[+\-]>? \d+[_\d+]*
#S#  }
token escale {
    <[Ee]> <[+\-]>? \d+[_\d+]*
}
#S#  
#S#  # careful to distinguish from both integer and 42.method
#S#  token dec_number {
#S#      :dba('decimal number')
#S#      [
#S#      | $<coeff> = [           '.' \d+[_\d+]* ] <escale>?
#S#      | $<coeff> = [\d+[_\d+]* '.' \d+[_\d+]* ] <escale>?
#S#      | $<coeff> = [\d+[_\d+]*                ] <escale>
#S#      ]
#S#  }
#S#  
#S#  token rad_number {
#S#      ':' $<radix> = [\d+] <.unsp>?      # XXX optional dot here?
#S#      {}           # don't recurse in lexer
#S#      :dba('number in radix notation')
#S#      [
#S#      || '<'
#S#              $<intpart> = [ <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* ]
#S#              $<fracpart> = [ '.' <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* ]?
#S#              [ '*' <base=radint> '**' <exp=radint> ]?
#S#         '>'
#S#  #      { make radcalc($<radix>, $<intpart>, $<fracpart>, $<base>, $<exp>) }
#S#      || <?before '['> <postcircumfix>
#S#      || <?before '('> <postcircumfix>
#S#      ]
#S#  }
#S#  
#S#  token octint {
#S#      <[ 0..7 ]>+ [ _ <[ 0..7 ]>+ ]*
#S#  }
token octint {
    <[ 0..7 ]>+ [ _ <[ 0..7 ]>+ ]*
}
#S#  
#S#  token hexint {
#S#      <[ 0..9 a..f A..F ]>+ [ _ <[ 0..9 a..f A..F ]>+ ]*
#S#  }
token hexint {
    <[ 0..9 a..f A..F ]>+ [ _ <[ 0..9 a..f A..F ]>+ ]*
}
#S#  
#S#  our @herestub_queue;
#S#  
#S#  class Herestub {
#S#      has Str $.delim;
#S#      has $.orignode;
#S#      has $.lang;
#S#  } # end class
#S#  
#S#  role herestop {
#S#      token stopper { ^^ {*} $<ws>=(\h*?) $+DELIM \h* <.unv>?? $$ \v? }
#S#  } # end role
#S#  
#S#  # XXX be sure to temporize @herestub_queue on reentry to new line of heredocs
#S#  
#S#  method heredoc () {
#S#      temp $CTX = self.callm if $*DEBUG +& DEBUG::trace_call;
#S#      return if self.peek;
#S#      my $here = self;
#S#      while my $herestub = shift @herestub_queue {
#S#          my $DELIM is context = $herestub.delim;
#S#          my $lang = $herestub.lang.mixin( ::herestop );
#S#          my $doc;
#S#          if ($doc) = $here.nibble($lang) {
#S#              $here = $doc.trim_heredoc();
#S#              $herestub.orignode<doc> = $doc;
#S#          }
#S#          else {
#S#              self.panic("Ending delimiter $DELIM not found");
#S#          }
#S#      }
#S#      return self.cursor($here.pos);  # return to initial type
#S#  }
#S#  
#S#  proto token backslash { <...> }
#S#  proto token escape { <...> }
#S#  token starter { <!> }
#S#  token escape:none { <!> }
#S#  
#S#  token babble ($l) {
#S#      :my $lang = $l;
#S#      :my $start;
#S#      :my $stop;
#S#  
#S#      <.ws>
#S#      [ <quotepair> <.ws>
#S#          {
#S#              my $kv = $<quotepair>[*-1];
#S#              $lang = $lang.tweak($kv.<k>, $kv.<v>)
#S#                  or self.panic("Unrecognized adverb :" ~ $kv.<k> ~ '(' ~ $kv.<v> ~ ')');
#S#          }
#S#      ]*
#S#  
#S#      {
#S#          ($start,$stop) = $¢.peek_delimiters();
#S#          $lang = $start ne $stop ?? $lang.balanced($start,$stop)
#S#                                  !! $lang.unbalanced($stop);
#S#          $<B> = [$lang,$start,$stop];
#S#      }
#S#  }
#S#  
#S#  token quibble ($l) {
#S#      :my ($lang, $start, $stop);
#S#      <babble($l)>
#S#      { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }
#S#  
#S#      $start <nibble($lang)> $stop
#S#  
#S#      {{
#S#          if $lang<_herelang> {
#S#              push @herestub_queue,
#S#                  STD::Herestub.new(
#S#                      delim => $<nibble><nibbles>[0],
#S#                      orignode => $¢,
#S#                      lang => $lang<_herelang>,
#S#                  );
#S#          }
#S#      }}
#S#  }
#S#  
#S#  token sibble ($l, $lang2) {
#S#      :my ($lang, $start, $stop);
#S#      <babble($l)>
#S#      { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }
#S#  
#S#      $start <left=nibble($lang)> $stop 
#S#      [ <?{ $start ne $stop }>
#S#          <.ws>
#S#          [ '=' || <.panic: "Missing '='"> ]
#S#          <.ws>
#S#          <right=EXPR(item %item_assignment)>
#S#      || 
#S#          { $lang = $lang2.unbalanced($stop); }
#S#          <right=nibble($lang)> $stop
#S#      ]
#S#  }
#S#  
#S#  token tribble ($l, $lang2 = $l) {
#S#      :my ($lang, $start, $stop);
#S#      <babble($l)>
#S#      { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }
#S#  
#S#      $start <left=nibble($lang)> $stop 
#S#      [ <?{ $start ne $stop }>
#S#          <.ws> <quibble($lang2)>
#S#      || 
#S#          { $lang = $lang2.unbalanced($stop); }
#S#          <right=nibble($lang)> $stop
#S#      ]
#S#  }
#S#  
#S#  token quasiquibble ($l) {
#S#      :my ($lang, $start, $stop);
#S#      <babble($l)>
#S#      { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }
#S#  
#S#      [
#S#      || <?{ $start eq '{' }> [ :lang($lang) <block> ]
#S#      || $start [ :lang($lang) <statementlist> ] $stop
#S#      ]
#S#  }
#S#  
#S#  # note: polymorphic over many quote languages, we hope
#S#  token nibbler {
#S#      :my $text = '';
#S#      :my @nibbles = ();
#S#      :my $multiline = 0;
#S#      :my $nibble;
#S#      { $<firstpos> = self.pos; }
#S#      [ <!before <stopper> >
#S#          [
#S#          || <starter> <nibbler> <stopper>
#S#                          {
#S#                              my $n = $<nibbler>[*-1]<nibbles>;
#S#                              my @n = @$n;
#S#                              $text ~= $<starter>[*-1].text ~ shift(@n);
#S#                              $text = (@n ?? pop(@n) !! '') ~ $<stopper>[*-1].text;
#S#                              push @nibbles, @n;
#S#                          }
#S#          || <escape>   {
#S#                              push @nibbles, $text, $<escape>[*-1];
#S#                              $text = '';
#S#                          }
#S#          || .
#S#                          {{
#S#                              my $ch = substr($ORIG, $¢.pos-1, 1);
#S#                              $text ~= $ch;
#S#                              if $ch ~~ "\n" {
#S#                                  $multiline++;
#S#                              }
#S#                          }}
#S#          ]
#S#      ]*
#S#      {
#S#          push @nibbles, $text; $<nibbles> = [@nibbles];
#S#          $<lastpos> = $¢.pos;
#S#          $<nibbler> :delete;
#S#          $<escape> :delete;
#S#          $COMPILING::LAST_NIBBLE = $¢;
#S#          $COMPILING::LAST_NIBBLE_MULTILINE = $¢ if $multiline;
#S#      }
#S#  }
#S#  
#S#  # and this is what makes nibbler polymorphic...
#S#  method nibble ($lang) {
#S#      my $outerlang = self.WHAT;
#S#      my $LANG is context = $outerlang;
#S#      self.cursor_fresh($lang).nibbler;
#S#  }
#S#  
#S#  
#S#  token quote:sym<' '>   { "'" <nibble($¢.cursor_fresh( ::STD::Q ).tweak(:q).unbalanced("'"))> "'" }
#S#  token quote:sym<" ">   { '"' <nibble($¢.cursor_fresh( ::STD::Q ).tweak(:qq).unbalanced('"'))> '"' }
#S#  
#S#  token quote:sym<« »>   { '«' <nibble($¢.cursor_fresh( ::STD::Q ).tweak(:qq).tweak(:ww).balanced('«','»'))> '»' }
#S#  token quote:sym«<< >>» { '<<' <nibble($¢.cursor_fresh( ::STD::Q ).tweak(:qq).tweak(:ww).balanced('<<','>>'))> '>>' }
#S#  token quote:sym«< >»   { '<' <nibble($¢.cursor_fresh( ::STD::Q ).tweak(:q).tweak(:w).balanced('<','>'))> '>' }
#S#  
#S#  token quote:sym</ />   {
#S#      '/' <nibble( $¢.cursor_fresh( ::Regex ).unbalanced("/") )> [ '/' || <.panic: "Unable to parse regex; couldn't find final '/'"> ]
#S#      <.old_rx_mods>?
#S#  }
#S#  
#S#  # handle composite forms like qww
#S#  token quote:qq {
#S#      :my $qm;
#S#      'qq'
#S#      [
#S#      | <quote_mod> » <!before '('> { $qm = $<quote_mod>.text } <.ws> <quibble($¢.cursor_fresh( ::STD::Q ).tweak(:qq).tweak($qm => 1))>
#S#      | » <!before '('> <.ws> <quibble($¢.cursor_fresh( ::STD::Q ).tweak(:qq))>
#S#      ]
#S#  }
#S#  token quote:q {
#S#      :my $qm;
#S#      'q'
#S#      [
#S#      | <quote_mod> » <!before '('> { $qm = $<quote_mod>.text } <quibble($¢.cursor_fresh( ::STD::Q ).tweak(:q).tweak($qm => 1))>
#S#      | » <!before '('> <.ws> <quibble($¢.cursor_fresh( ::STD::Q ).tweak(:q))>
#S#      ]
#S#  }
#S#  
#S#  token quote:Q {
#S#      :my $qm;
#S#      'Q'
#S#      [
#S#      | <quote_mod> » <!before '('> { $qm = $<quote_mod>.text } <quibble($¢.cursor_fresh( ::STD::Q ).tweak($qm => 1))>
#S#      | » <!before '('> <.ws> <quibble($¢.cursor_fresh( ::STD::Q ))>
#S#      ]
#S#  }
#S#  
#S#  token quote_mod:w  { <sym> }
#S#  token quote_mod:ww { <sym> }
#S#  token quote_mod:x  { <sym> }
#S#  token quote_mod:to { <sym> }
#S#  token quote_mod:s  { <sym> }
#S#  token quote_mod:a  { <sym> }
#S#  token quote_mod:h  { <sym> }
#S#  token quote_mod:f  { <sym> }
#S#  token quote_mod:c  { <sym> }
#S#  token quote_mod:b  { <sym> }
#S#  
#S#  token quote:rx {
#S#      <sym> » <!before '('>
#S#      <quibble( $¢.cursor_fresh( ::Regex ) )>
#S#      <!old_rx_mods>
#S#  }
#S#  
#S#  token quote:m  {
#S#      <sym> » <!before '('>
#S#      <quibble( $¢.cursor_fresh( ::Regex ) )>
#S#      <!old_rx_mods>
#S#  }
#S#  
#S#  token quote:mm {
#S#      <sym> » <!before '('>
#S#      <quibble( $¢.cursor_fresh( ::Regex ).tweak(:s))>
#S#      <!old_rx_mods>
#S#  }
#S#  
#S#  token quote:s {
#S#      <sym> » <!before '('>
#S#      <pat=sibble( $¢.cursor_fresh( ::Regex ), $¢.cursor_fresh( ::STD::Q ).tweak(:qq))>
#S#      <!old_rx_mods>
#S#  }
#S#  
#S#  token quote:ss {
#S#      <sym> » <!before '('>
#S#      <pat=sibble( $¢.cursor_fresh( ::Regex ).tweak(:s), $¢.cursor_fresh( ::STD::Q ).tweak(:qq))>
#S#      <!old_rx_mods>
#S#  }
#S#  token quote:tr {
#S#      <sym> » <!before '('> <pat=tribble( $¢.cursor_fresh( ::STD::Q ).tweak(:q))>
#S#      <!old_tr_mods>
#S#  }
#S#  
#S#  token old_rx_mods {
#S#      (< i g s m x c e >+) 
#S#      {{
#S#          given $0.text {
#S#              $_ ~~ /i/ and $¢.worryobs('/i',':i');
#S#              $_ ~~ /g/ and $¢.worryobs('/g',':g');
#S#              $_ ~~ /s/ and $¢.worryobs('/s','^^ and $$ anchors');
#S#              $_ ~~ /m/ and $¢.worryobs('/m','. or \N');
#S#              $_ ~~ /x/ and $¢.worryobs('/x','normal default whitespace');
#S#              $_ ~~ /c/ and $¢.worryobs('/c',':c or :p');
#S#              $_ ~~ /e/ and $¢.worryobs('/e','interpolated {...} or s{} = ... form');
#S#              $¢.obs('suffix regex modifiers','prefix adverbs');
#S#          }
#S#      }}
#S#  }
#S#  
#S#  token old_tr_mods {
#S#      (< c d s ] >+) 
#S#      {{
#S#          given $0.text {
#S#              $_ ~~ /c/ and $¢.worryobs('/c',':c');
#S#              $_ ~~ /d/ and $¢.worryobs('/g',':d');
#S#              $_ ~~ /s/ and $¢.worryobs('/s',':s');
#S#              $¢.obs('suffix transliteration modifiers','prefix adverbs');
#S#          }
#S#      }}
#S#  }
#S#  
#S#  
#S#  token quote:quasi {
#S#      <sym> » <!before '('> <quasiquibble($¢.cursor_fresh( ::STD::Quasi ))>
#S#  }
#S#  
#S#  # XXX should eventually be derived from current Unicode tables.
#S#  constant %open2close = (
#S#      "\x0028" => "\x0029", "\x003C" => "\x003E", "\x005B" => "\x005D",
#S#      "\x007B" => "\x007D", "\x00AB" => "\x00BB", "\x0F3A" => "\x0F3B",
#S#      "\x0F3C" => "\x0F3D", "\x169B" => "\x169C", "\x2039" => "\x203A",
#S#      "\x2045" => "\x2046", "\x207D" => "\x207E", "\x208D" => "\x208E",
#S#      "\x2208" => "\x220B", "\x2209" => "\x220C", "\x220A" => "\x220D",
#S#      "\x2215" => "\x29F5", "\x223C" => "\x223D", "\x2243" => "\x22CD",
#S#      "\x2252" => "\x2253", "\x2254" => "\x2255", "\x2264" => "\x2265",
#S#      "\x2266" => "\x2267", "\x2268" => "\x2269", "\x226A" => "\x226B",
#S#      "\x226E" => "\x226F", "\x2270" => "\x2271", "\x2272" => "\x2273",
#S#      "\x2274" => "\x2275", "\x2276" => "\x2277", "\x2278" => "\x2279",
#S#      "\x227A" => "\x227B", "\x227C" => "\x227D", "\x227E" => "\x227F",
#S#      "\x2280" => "\x2281", "\x2282" => "\x2283", "\x2284" => "\x2285",
#S#      "\x2286" => "\x2287", "\x2288" => "\x2289", "\x228A" => "\x228B",
#S#      "\x228F" => "\x2290", "\x2291" => "\x2292", "\x2298" => "\x29B8",
#S#      "\x22A2" => "\x22A3", "\x22A6" => "\x2ADE", "\x22A8" => "\x2AE4",
#S#      "\x22A9" => "\x2AE3", "\x22AB" => "\x2AE5", "\x22B0" => "\x22B1",
#S#      "\x22B2" => "\x22B3", "\x22B4" => "\x22B5", "\x22B6" => "\x22B7",
#S#      "\x22C9" => "\x22CA", "\x22CB" => "\x22CC", "\x22D0" => "\x22D1",
#S#      "\x22D6" => "\x22D7", "\x22D8" => "\x22D9", "\x22DA" => "\x22DB",
#S#      "\x22DC" => "\x22DD", "\x22DE" => "\x22DF", "\x22E0" => "\x22E1",
#S#      "\x22E2" => "\x22E3", "\x22E4" => "\x22E5", "\x22E6" => "\x22E7",
#S#      "\x22E8" => "\x22E9", "\x22EA" => "\x22EB", "\x22EC" => "\x22ED",
#S#      "\x22F0" => "\x22F1", "\x22F2" => "\x22FA", "\x22F3" => "\x22FB",
#S#      "\x22F4" => "\x22FC", "\x22F6" => "\x22FD", "\x22F7" => "\x22FE",
#S#      "\x2308" => "\x2309", "\x230A" => "\x230B", "\x2329" => "\x232A",
#S#      "\x23B4" => "\x23B5", "\x2768" => "\x2769", "\x276A" => "\x276B",
#S#      "\x276C" => "\x276D", "\x276E" => "\x276F", "\x2770" => "\x2771",
#S#      "\x2772" => "\x2773", "\x2774" => "\x2775", "\x27C3" => "\x27C4",
#S#      "\x27C5" => "\x27C6", "\x27D5" => "\x27D6", "\x27DD" => "\x27DE",
#S#      "\x27E2" => "\x27E3", "\x27E4" => "\x27E5", "\x27E6" => "\x27E7",
#S#      "\x27E8" => "\x27E9", "\x27EA" => "\x27EB", "\x2983" => "\x2984",
#S#      "\x2985" => "\x2986", "\x2987" => "\x2988", "\x2989" => "\x298A",
#S#      "\x298B" => "\x298C", "\x298D" => "\x298E", "\x298F" => "\x2990",
#S#      "\x2991" => "\x2992", "\x2993" => "\x2994", "\x2995" => "\x2996",
#S#      "\x2997" => "\x2998", "\x29C0" => "\x29C1", "\x29C4" => "\x29C5",
#S#      "\x29CF" => "\x29D0", "\x29D1" => "\x29D2", "\x29D4" => "\x29D5",
#S#      "\x29D8" => "\x29D9", "\x29DA" => "\x29DB", "\x29F8" => "\x29F9",
#S#      "\x29FC" => "\x29FD", "\x2A2B" => "\x2A2C", "\x2A2D" => "\x2A2E",
#S#      "\x2A34" => "\x2A35", "\x2A3C" => "\x2A3D", "\x2A64" => "\x2A65",
#S#      "\x2A79" => "\x2A7A", "\x2A7D" => "\x2A7E", "\x2A7F" => "\x2A80",
#S#      "\x2A81" => "\x2A82", "\x2A83" => "\x2A84", "\x2A8B" => "\x2A8C",
#S#      "\x2A91" => "\x2A92", "\x2A93" => "\x2A94", "\x2A95" => "\x2A96",
#S#      "\x2A97" => "\x2A98", "\x2A99" => "\x2A9A", "\x2A9B" => "\x2A9C",
#S#      "\x2AA1" => "\x2AA2", "\x2AA6" => "\x2AA7", "\x2AA8" => "\x2AA9",
#S#      "\x2AAA" => "\x2AAB", "\x2AAC" => "\x2AAD", "\x2AAF" => "\x2AB0",
#S#      "\x2AB3" => "\x2AB4", "\x2ABB" => "\x2ABC", "\x2ABD" => "\x2ABE",
#S#      "\x2ABF" => "\x2AC0", "\x2AC1" => "\x2AC2", "\x2AC3" => "\x2AC4",
#S#      "\x2AC5" => "\x2AC6", "\x2ACD" => "\x2ACE", "\x2ACF" => "\x2AD0",
#S#      "\x2AD1" => "\x2AD2", "\x2AD3" => "\x2AD4", "\x2AD5" => "\x2AD6",
#S#      "\x2AEC" => "\x2AED", "\x2AF7" => "\x2AF8", "\x2AF9" => "\x2AFA",
#S#      "\x2E02" => "\x2E03", "\x2E04" => "\x2E05", "\x2E09" => "\x2E0A",
#S#      "\x2E0C" => "\x2E0D", "\x2E1C" => "\x2E1D", "\x3008" => "\x3009",
#S#      "\x300A" => "\x300B", "\x300C" => "\x300D", "\x300E" => "\x300F",
#S#      "\x3010" => "\x3011", "\x3014" => "\x3015", "\x3016" => "\x3017",
#S#      "\x3018" => "\x3019", "\x301A" => "\x301B", "\x301D" => "\x301E",
#S#      "\xFD3E" => "\xFD3F", "\xFE17" => "\xFE18", "\xFE35" => "\xFE36",
#S#      "\xFE37" => "\xFE38", "\xFE39" => "\xFE3A", "\xFE3B" => "\xFE3C",
#S#      "\xFE3D" => "\xFE3E", "\xFE3F" => "\xFE40", "\xFE41" => "\xFE42",
#S#      "\xFE43" => "\xFE44", "\xFE47" => "\xFE48", "\xFE59" => "\xFE5A",
#S#      "\xFE5B" => "\xFE5C", "\xFE5D" => "\xFE5E", "\xFF08" => "\xFF09",
#S#      "\xFF1C" => "\xFF1E", "\xFF3B" => "\xFF3D", "\xFF5B" => "\xFF5D",
#S#      "\xFF5F" => "\xFF60", "\xFF62" => "\xFF63",
#S#  );
constant %open2close = (
    "\x0028" => "\x0029", "\x003C" => "\x003E", "\x005B" => "\x005D",
    "\x007B" => "\x007D", "\x00AB" => "\x00BB", "\x0F3A" => "\x0F3B",
    "\x0F3C" => "\x0F3D", "\x169B" => "\x169C", "\x2039" => "\x203A",
    "\x2045" => "\x2046", "\x207D" => "\x207E", "\x208D" => "\x208E",
    "\x2208" => "\x220B", "\x2209" => "\x220C", "\x220A" => "\x220D",
    "\x2215" => "\x29F5", "\x223C" => "\x223D", "\x2243" => "\x22CD",
    "\x2252" => "\x2253", "\x2254" => "\x2255", "\x2264" => "\x2265",
    "\x2266" => "\x2267", "\x2268" => "\x2269", "\x226A" => "\x226B",
    "\x226E" => "\x226F", "\x2270" => "\x2271", "\x2272" => "\x2273",
    "\x2274" => "\x2275", "\x2276" => "\x2277", "\x2278" => "\x2279",
    "\x227A" => "\x227B", "\x227C" => "\x227D", "\x227E" => "\x227F",
    "\x2280" => "\x2281", "\x2282" => "\x2283", "\x2284" => "\x2285",
    "\x2286" => "\x2287", "\x2288" => "\x2289", "\x228A" => "\x228B",
    "\x228F" => "\x2290", "\x2291" => "\x2292", "\x2298" => "\x29B8",
    "\x22A2" => "\x22A3", "\x22A6" => "\x2ADE", "\x22A8" => "\x2AE4",
    "\x22A9" => "\x2AE3", "\x22AB" => "\x2AE5", "\x22B0" => "\x22B1",
    "\x22B2" => "\x22B3", "\x22B4" => "\x22B5", "\x22B6" => "\x22B7",
    "\x22C9" => "\x22CA", "\x22CB" => "\x22CC", "\x22D0" => "\x22D1",
    "\x22D6" => "\x22D7", "\x22D8" => "\x22D9", "\x22DA" => "\x22DB",
    "\x22DC" => "\x22DD", "\x22DE" => "\x22DF", "\x22E0" => "\x22E1",
    "\x22E2" => "\x22E3", "\x22E4" => "\x22E5", "\x22E6" => "\x22E7",
    "\x22E8" => "\x22E9", "\x22EA" => "\x22EB", "\x22EC" => "\x22ED",
    "\x22F0" => "\x22F1", "\x22F2" => "\x22FA", "\x22F3" => "\x22FB",
    "\x22F4" => "\x22FC", "\x22F6" => "\x22FD", "\x22F7" => "\x22FE",
    "\x2308" => "\x2309", "\x230A" => "\x230B", "\x2329" => "\x232A",
    "\x23B4" => "\x23B5", "\x2768" => "\x2769", "\x276A" => "\x276B",
    "\x276C" => "\x276D", "\x276E" => "\x276F", "\x2770" => "\x2771",
    "\x2772" => "\x2773", "\x2774" => "\x2775", "\x27C3" => "\x27C4",
    "\x27C5" => "\x27C6", "\x27D5" => "\x27D6", "\x27DD" => "\x27DE",
    "\x27E2" => "\x27E3", "\x27E4" => "\x27E5", "\x27E6" => "\x27E7",
    "\x27E8" => "\x27E9", "\x27EA" => "\x27EB", "\x2983" => "\x2984",
    "\x2985" => "\x2986", "\x2987" => "\x2988", "\x2989" => "\x298A",
    "\x298B" => "\x298C", "\x298D" => "\x298E", "\x298F" => "\x2990",
    "\x2991" => "\x2992", "\x2993" => "\x2994", "\x2995" => "\x2996",
    "\x2997" => "\x2998", "\x29C0" => "\x29C1", "\x29C4" => "\x29C5",
    "\x29CF" => "\x29D0", "\x29D1" => "\x29D2", "\x29D4" => "\x29D5",
    "\x29D8" => "\x29D9", "\x29DA" => "\x29DB", "\x29F8" => "\x29F9",
    "\x29FC" => "\x29FD", "\x2A2B" => "\x2A2C", "\x2A2D" => "\x2A2E",
    "\x2A34" => "\x2A35", "\x2A3C" => "\x2A3D", "\x2A64" => "\x2A65",
    "\x2A79" => "\x2A7A", "\x2A7D" => "\x2A7E", "\x2A7F" => "\x2A80",
    "\x2A81" => "\x2A82", "\x2A83" => "\x2A84", "\x2A8B" => "\x2A8C",
    "\x2A91" => "\x2A92", "\x2A93" => "\x2A94", "\x2A95" => "\x2A96",
    "\x2A97" => "\x2A98", "\x2A99" => "\x2A9A", "\x2A9B" => "\x2A9C",
    "\x2AA1" => "\x2AA2", "\x2AA6" => "\x2AA7", "\x2AA8" => "\x2AA9",
    "\x2AAA" => "\x2AAB", "\x2AAC" => "\x2AAD", "\x2AAF" => "\x2AB0",
    "\x2AB3" => "\x2AB4", "\x2ABB" => "\x2ABC", "\x2ABD" => "\x2ABE",
    "\x2ABF" => "\x2AC0", "\x2AC1" => "\x2AC2", "\x2AC3" => "\x2AC4",
    "\x2AC5" => "\x2AC6", "\x2ACD" => "\x2ACE", "\x2ACF" => "\x2AD0",
    "\x2AD1" => "\x2AD2", "\x2AD3" => "\x2AD4", "\x2AD5" => "\x2AD6",
    "\x2AEC" => "\x2AED", "\x2AF7" => "\x2AF8", "\x2AF9" => "\x2AFA",
    "\x2E02" => "\x2E03", "\x2E04" => "\x2E05", "\x2E09" => "\x2E0A",
    "\x2E0C" => "\x2E0D", "\x2E1C" => "\x2E1D", "\x3008" => "\x3009",
    "\x300A" => "\x300B", "\x300C" => "\x300D", "\x300E" => "\x300F",
    "\x3010" => "\x3011", "\x3014" => "\x3015", "\x3016" => "\x3017",
    "\x3018" => "\x3019", "\x301A" => "\x301B", "\x301D" => "\x301E",
    "\xFD3E" => "\xFD3F", "\xFE17" => "\xFE18", "\xFE35" => "\xFE36",
    "\xFE37" => "\xFE38", "\xFE39" => "\xFE3A", "\xFE3B" => "\xFE3C",
    "\xFE3D" => "\xFE3E", "\xFE3F" => "\xFE40", "\xFE41" => "\xFE42",
    "\xFE43" => "\xFE44", "\xFE47" => "\xFE48", "\xFE59" => "\xFE5A",
    "\xFE5B" => "\xFE5C", "\xFE5D" => "\xFE5E", "\xFF08" => "\xFF09",
    "\xFF1C" => "\xFF1E", "\xFF3B" => "\xFF3D", "\xFF5B" => "\xFF5D",
    "\xFF5F" => "\xFF60", "\xFF62" => "\xFF63",
);
#S#  
#S#  token opener {
#S#    <[\x0028 \x003C \x005B
#S#      \x007B \x00AB \x0F3A
#S#      \x0F3C \x169B \x2039
#S#      \x2045 \x207D \x208D
#S#      \x2208 \x2209 \x220A
#S#      \x2215 \x223C \x2243
#S#      \x2252 \x2254 \x2264
#S#      \x2266 \x2268 \x226A
#S#      \x226E \x2270 \x2272
#S#      \x2274 \x2276 \x2278
#S#      \x227A \x227C \x227E
#S#      \x2280 \x2282 \x2284
#S#      \x2286 \x2288 \x228A
#S#      \x228F \x2291 \x2298
#S#      \x22A2 \x22A6 \x22A8
#S#      \x22A9 \x22AB \x22B0
#S#      \x22B2 \x22B4 \x22B6
#S#      \x22C9 \x22CB \x22D0
#S#      \x22D6 \x22D8 \x22DA
#S#      \x22DC \x22DE \x22E0
#S#      \x22E2 \x22E4 \x22E6
#S#      \x22E8 \x22EA \x22EC
#S#      \x22F0 \x22F2 \x22F3
#S#      \x22F4 \x22F6 \x22F7
#S#      \x2308 \x230A \x2329
#S#      \x23B4 \x2768 \x276A
#S#      \x276C \x276E \x2770
#S#      \x2772 \x2774 \x27C3
#S#      \x27C5 \x27D5 \x27DD
#S#      \x27E2 \x27E4 \x27E6
#S#      \x27E8 \x27EA \x2983
#S#      \x2985 \x2987 \x2989
#S#      \x298B \x298D \x298F
#S#      \x2991 \x2993 \x2995
#S#      \x2997 \x29C0 \x29C4
#S#      \x29CF \x29D1 \x29D4
#S#      \x29D8 \x29DA \x29F8
#S#      \x29FC \x2A2B \x2A2D
#S#      \x2A34 \x2A3C \x2A64
#S#      \x2A79 \x2A7D \x2A7F
#S#      \x2A81 \x2A83 \x2A8B
#S#      \x2A91 \x2A93 \x2A95
#S#      \x2A97 \x2A99 \x2A9B
#S#      \x2AA1 \x2AA6 \x2AA8
#S#      \x2AAA \x2AAC \x2AAF
#S#      \x2AB3 \x2ABB \x2ABD
#S#      \x2ABF \x2AC1 \x2AC3
#S#      \x2AC5 \x2ACD \x2ACF
#S#      \x2AD1 \x2AD3 \x2AD5
#S#      \x2AEC \x2AF7 \x2AF9
#S#      \x2E02 \x2E04 \x2E09
#S#      \x2E0C \x2E1C \x3008
#S#      \x300A \x300C \x300E
#S#      \x3010 \x3014 \x3016
#S#      \x3018 \x301A \x301D
#S#      \xFD3E \xFE17 \xFE35
#S#      \xFE37 \xFE39 \xFE3B
#S#      \xFE3D \xFE3F \xFE41
#S#      \xFE43 \xFE47 \xFE59
#S#      \xFE5B \xFE5D \xFF08
#S#      \xFF1C \xFF3B \xFF5B
#S#      \xFF5F \xFF62]>
#S#  }
#S#  
#S#  # assumes whitespace is eaten already
#S#  
#S#  method peek_delimiters {
#S#      my $pos = self.pos;
#S#      my $startpos = $pos;
#S#      my $char = substr($ORIG,$pos++,1);
#S#      if $char ~~ /^\s$/ {
#S#          self.panic("Whitespace not allowed as delimiter");
#S#      }
#S#  
#S#  # XXX not defined yet
#S#  #    <?before <+isPe> > {
#S#  #        self.panic("Use a closing delimiter for an opener is reserved");
#S#  #    }
#S#  
#S#      my $rightbrack = %open2close{$char};
#S#      if not defined $rightbrack {
#S#          return $char, $char;
#S#      }
#S#      while substr($ORIG,$pos,1) eq $char {
#S#          $pos++;
#S#      }
#S#      my $len = $pos - $startpos;
#S#      my $start = $char x $len;
#S#      my $stop = $rightbrack x $len;
#S#      return $start, $stop;
#S#  }
#S#  
#S#  role startstop[$start,$stop] {
#S#      token starter { $start }
#S#      token stopper { $stop }
#S#  } # end role
#S#  
#S#  role stop[$stop] {
#S#      token starter { <!> }
#S#      token stopper { $stop }
#S#  } # end role
#S#  
#S#  role unitstop[$stop] {
#S#      token unitstopper { $stop }
#S#  } # end role
#S#  
#S#  token unitstopper { $ }
#S#  
#S#  method balanced ($start,$stop) { self.mixin( ::startstop[$start,$stop] ); }
#S#  method unbalanced ($stop) { self.mixin( ::stop[$stop] ); }
#S#  method unitstop ($stop) { self.mixin( ::unitstop[$stop] ); }
#S#  
#S#  token codepoint {
#S#      '[' {} ( [<!before ']'> .]*? ) ']'
#S#  }
#S#  
#S#  method truly ($bool,$opt) {
#S#      return self if $bool;
#S#      self.panic("Can't negate $opt adverb");
#S#  }
#S#  
#S#  grammar Q is STD {
grammar Q is STD {
#S#  
#S#      role b1 {
#S#          token escape:sym<\\> { <sym> <item=backslash> }
#S#          token backslash:qq { <?before 'q'> { $<quote> = $¢.cursor_fresh($+LANG).quote(); } }
#S#          token backslash:sym<\\> { <text=sym> }
#S#          token backslash:stopper { <text=stopper> }
#S#          token backslash:a { <sym> }
#S#          token backslash:b { <sym> }
#S#          token backslash:c { <sym>
#S#              [
#S#              | <codepoint>
#S#              | \d+
#S#              | [ <[ ?.._ ]> || <.panic: "Unrecognized \\c character"> ]
#S#              ]
#S#          }
#S#          token backslash:e { <sym> }
#S#          token backslash:f { <sym> }
#S#          token backslash:n { <sym> }
#S#          token backslash:o { <sym> [ <octint> | '[' <octint>**',' ']' ] }
#S#          token backslash:r { <sym> }
#S#          token backslash:t { <sym> }
#S#          token backslash:x { <sym> [ <hexint> | '[' [<.ws><hexint><.ws> ] ** ',' ']' ] }
#S#          token backslash:sym<0> { <sym> }
#S#      } # end role
#S#  
#S#      role b0 {
#S#          token escape:sym<\\> { <!> }
#S#      } # end role
#S#  
#S#      role c1 {
#S#          token escape:sym<{ }> { <?before '{'> [ :lang($+LANG) <block> ] }
#S#      } # end role
#S#  
#S#      role c0 {
#S#          token escape:sym<{ }> { <!> }
#S#      } # end role
#S#  
#S#      role s1 {
#S#          token escape:sym<$> { <?before '$'> [ :lang($+LANG) <variable> <extrapost>? ] || <.panic: "Non-variable \$ must be backslashed"> }
#S#          token special_variable:sym<$"> {
#S#              '$' <stopper>
#S#              <.panic: "Can't use a \$ in the last position of an interpolating string">
#S#          }
#S#  
#S#      } # end role
#S#  
#S#      role s0 {
#S#          token escape:sym<$> { <!> }
#S#          token special_variable:sym<$"> { <!> }
#S#      } # end role
#S#  
#S#      role a1 {
#S#          token escape:sym<@> { <?before '@'> [ :lang($+LANG) <variable> <extrapost> | <!> ] } # trap ABORTBRANCH from variable's ::
#S#      } # end role
#S#  
#S#      role a0 {
#S#          token escape:sym<@> { <!> }
#S#      } # end role
#S#  
#S#      role h1 {
#S#          token escape:sym<%> { <?before '%'> [ :lang($+LANG) <variable> <extrapost> | <!> ] }
#S#      } # end role
#S#  
#S#      role h0 {
#S#          token escape:sym<%> { <!> }
#S#      } # end role
#S#  
#S#      role f1 {
#S#          token escape:sym<&> { <?before '&'> [ :lang($+LANG) <variable> <extrapost> | <!> ] }
#S#      } # end role
#S#  
#S#      role f0 {
#S#          token escape:sym<&> { <!> }
#S#      } # end role
#S#  
#S#      role w1 {
#S#          method postprocess ($s) { $s.comb }
#S#      } # end role
#S#  
#S#      role w0 {
#S#          method postprocess ($s) { $s }
#S#      } # end role
#S#  
#S#      role ww1 {
#S#          method postprocess ($s) { $s.comb }
#S#      } # end role
#S#  
#S#      role ww0 {
#S#          method postprocess ($s) { $s }
#S#      } # end role
#S#  
#S#      role x1 {
#S#          method postprocess ($s) { $s.run }
#S#      } # end role
#S#  
#S#      role x0 {
#S#          method postprocess ($s) { $s }
#S#      } # end role
#S#  
#S#      role q {
#S#          token stopper { \' }
#S#  
#S#          token escape:sym<\\> { <sym> <item=backslash> }
#S#  
#S#          token backslash:qq { <?before 'q'> { $<quote> = $¢.cursor_fresh($+LANG).quote(); } }
#S#          token backslash:sym<\\> { <text=sym> }
#S#          token backslash:stopper { <text=stopper> }
#S#  
#S#          # in single quotes, keep backslash on random character by default
#S#          token backslash:misc { {} (.) { $<text> = "\\" ~ $0.text; } }
#S#  
#S#          # begin tweaks (DO NOT ERASE)
#S#          multi method tweak (:single(:$q)) { self.panic("Too late for :q") }
#S#          multi method tweak (:double(:$qq)) { self.panic("Too late for :qq") }
#S#          # end tweaks (DO NOT ERASE)
#S#  
#S#      } # end role
#S#  
#S#      role qq does b1 does c1 does s1 does a1 does h1 does f1 {
#S#          token stopper { \" }
#S#          # in double quotes, omit backslash on random \W backslash by default
#S#          token backslash:misc { {} [ (\W) { $<text> = $0.text; } | $<x>=(\w) <.panic("Unrecognized backslash sequence: '\\" ~ $<x>.text ~ "'")> ] }
#S#  
#S#          # begin tweaks (DO NOT ERASE)
#S#          multi method tweak (:single(:$q)) { self.panic("Too late for :q") }
#S#          multi method tweak (:double(:$qq)) { self.panic("Too late for :qq") }
#S#          # end tweaks (DO NOT ERASE)
#S#  
#S#      } # end role
#S#  
#S#      role p5 {
#S#          # begin tweaks (DO NOT ERASE)
#S#          multi method tweak (:$g) { self }
#S#          multi method tweak (:$i) { self }
#S#          multi method tweak (:$m) { self }
#S#          multi method tweak (:$s) { self }
#S#          multi method tweak (:$x) { self }
#S#          multi method tweak (:$p) { self }
#S#          multi method tweak (:$c) { self }
#S#          # end tweaks (DO NOT ERASE)
#S#      } # end role
#S#  
#S#      # begin tweaks (DO NOT ERASE)
#S#  
#S#      multi method tweak (:single(:$q)) { self.truly($q,':q'); self.mixin( ::q ); }
#S#  
#S#      multi method tweak (:double(:$qq)) { self.truly($qq, ':qq'); self.mixin( ::qq ); }
#S#  
#S#      multi method tweak (:backslash(:$b))   { self.mixin($b ?? ::b1 !! ::b0) }
#S#      multi method tweak (:scalar(:$s))      { self.mixin($s ?? ::s1 !! ::s0) }
#S#      multi method tweak (:array(:$a))       { self.mixin($a ?? ::a1 !! ::a0) }
#S#      multi method tweak (:hash(:$h))        { self.mixin($h ?? ::h1 !! ::h0) }
#S#      multi method tweak (:function(:$f))    { self.mixin($f ?? ::f1 !! ::f0) }
#S#      multi method tweak (:closure(:$c))     { self.mixin($c ?? ::c1 !! ::c0) }
#S#  
#S#      multi method tweak (:exec(:$x))        { self.mixin($x ?? ::x1 !! ::x0) }
#S#      multi method tweak (:words(:$w))       { self.mixin($w ?? ::w1 !! ::w0) }
#S#      multi method tweak (:quotewords(:$ww)) { self.mixin($ww ?? ::ww1 !! ::ww0) }
#S#  
#S#      multi method tweak (:heredoc(:$to)) { self.truly($to, ':to'); self.cursor_herelang; }
#S#  
#S#      multi method tweak (:$regex) {
#S#          return ::Regex;
#S#      }
#S#  
#S#      multi method tweak (:$trans) {
#S#          return ::Trans;
#S#      }
#S#  
#S#      multi method tweak (*%x) {
#S#          my @k = keys(%x);
#S#          self.panic("Unrecognized quote modifier: " ~ @k);
#S#      }
#S#      # end tweaks (DO NOT ERASE)
#S#  
#S#  
#S#  } # end grammar
} # end grammar
#S#  
#S#  grammar Quasi is STD {
grammar Quasi is STD {
#S#      token term:unquote {
#S#          <starter><starter><starter> <EXPR> <stopper><stopper><stopper>
#S#      }
#S#  
#S#      # begin tweaks (DO NOT ERASE)
#S#      multi method tweak (:$ast) { self; } # XXX some transformer operating on the normal AST?
#S#      multi method tweak (:$lang) { self.cursor_fresh( $lang ); }
#S#      multi method tweak (:$unquote) { self; } # XXX needs to override unquote
#S#      multi method tweak (:$COMPILING) { self; } # XXX needs to lazify the lexical lookups somehow
#S#  
#S#      multi method tweak (*%x) {
#S#          my @k = keys(%x);
#S#          self.panic("Unrecognized quasiquote modifier: " ~ @k);
#S#      }
#S#      # end tweaks (DO NOT ERASE)
#S#  
#S#  } # end grammar
} # end grammar
#S#  
#S#  # Note, backtracks!  So POST must not commit to anything permanent.
#S#  regex extrapost {
#S#      :my $inquote is context = 1;
#S#      <POST>*
#S#      # XXX Shouldn't need a backslash on anything but the right square here
#S#      <?after <[ \] } > ) ]> > 
#S#  }
#S#  
#S#  rule multisig {
#S#      [
#S#          ':'?'(' ~ ')' <signature>
#S#      ]
#S#      ** '|'
#S#  }
#S#  
#S#  rule routine_def {
#S#      :my $IN_DECL is context<rw> = 1;
#S#      [ '&'<deflongname>? | <deflongname> ]? [ <multisig> | <trait> ]*
#S#      <!{
#S#          $¢ = $+PARSER.bless($¢);
#S#          $IN_DECL = 0;
#S#      }>
#S#      <block>:!s
#S#  }
#S#  
#S#  rule method_def {
#S#      [
#S#      | <[ ! ^ ]>?<longname> [ <multisig> | <trait> ]*
#S#      | <multisig> <trait>*
#S#      | <sigil> '.'
#S#          :dba('subscript signature')
#S#          [
#S#          | '(' ~ ')' <signature>
#S#          | '[' ~ ']' <signature>
#S#          | '{' ~ '}' <signature>
#S#          | <?before '<'> <postcircumfix>
#S#          ]
#S#          <trait>*
#S#      | <?>
#S#      ]
#S#      <block>:!s
#S#  }
#S#  
#S#  rule regex_def {
#S#      :my $IN_DECL is context<rw> = 1;
#S#      [ '&'<deflongname>? | <deflongname> ]?
#S#      [ [ ':'?'(' <signature> ')'] | <trait> ]*
#S#      { $IN_DECL = 0; }
#S#      <regex_block>:!s
#S#  }
#S#  
#S#  rule macro_def {
#S#      :my $IN_DECL is context<rw> = 1;
#S#      [ '&'<deflongname>? | <deflongname> ]? [ <multisig> | <trait> ]*
#S#      <!{
#S#          $¢ = $+PARSER.bless($¢);
#S#          $IN_DECL = 0;
#S#      }>
#S#      <block>:!s
#S#  }
#S#  
#S#  rule trait {
#S#      [
#S#      | <trait_verb>
#S#      | <trait_auxiliary>
#S#      | <colonpair>
#S#      ]
#S#  }
#S#  
#S#  token trait_auxiliary:is {
#S#      <sym> <.ws> <longname><postcircumfix>?
#S#  }
#S#  token trait_auxiliary:does {
#S#      :my $PKGDECL is context = 'role';
#S#      <sym> <.ws> <module_name>
#S#  }
#S#  token trait_auxiliary:will {
#S#      <sym> <.ws> <identifier> <.ws> <block>
#S#  }
#S#  
#S#  rule trait_verb:of      {<sym> <fulltypename> }
#S#  rule trait_verb:as      {<sym> <fulltypename> }
#S#  rule trait_verb:returns {<sym> <fulltypename> }
#S#  rule trait_verb:handles {<sym> <EXPR> }
#S#  
#S#  token capterm {
#S#      '\\'
#S#      [
#S#      | '(' <capture>? ')'
#S#      | <?before \S> <termish>
#S#      ]
#S#  }
#S#  
#S#  rule capture {
#S#      <EXPR>
#S#  }
#S#  
#S#  token sigterm {
#S#      ':(' ~ ')' <signature>
#S#  }
#S#  
#S#  rule param_sep { [','|':'|';'|';;'] }
#S#  
#S#  token signature {
#S#      # XXX incorrectly scopes &infix:<x> parameters to outside following block
#S#      :my $IN_DECL is context<rw> = 1;
#S#      :my $zone is context<rw> = 'posreq';
#S#      <.ws>
#S#      [
#S#      | <?before '-->' | ')' | ']' | '{' | ':'<!before ':' > >
#S#      | <parameter>
#S#      ] ** <param_sep>
#S#      <.ws>
#S#      [ '-->' <.ws> <fulltypename> ]?
#S#      { $IN_DECL = 0; $+SIGIL = '@' }
#S#  }
#S#  
#S#  token type_declarator:subset {
#S#      <sym> :s
#S#      <longname> { $¢.add_type($<longname>.text); }
#S#      [ of <fulltypename> ]?
#S#      [where <EXPR(item %chaining)> ]?    # (EXPR can parse multiple where clauses)
#S#  }
#S#  
#S#  token type_declarator:enum {
#S#      :my $l;
#S#      <sym> <.ws>
#S#      [ $l = <longname> :: { $¢.add_type($l.text); } <.ws> ]?
#S#      <EXPR> <.ws>
#S#  }
#S#  
#S#  rule type_constraint {
#S#      [
#S#      | <value>
#S#      | <fulltypename>
#S#      | where <EXPR(item %chaining)>
#S#      ]
#S#  }
#S#  
#S#  rule post_constraint {
#S#      [
#S#      | '[' ~ ']' <signature>
#S#      | '(' ~ ')' <signature>
#S#      | where <EXPR(item %chaining)>
#S#      ]
#S#  }
#S#  
#S#  token named_param {
#S#      :my $GOAL is context = ')';
#S#      ':'
#S#      [
#S#      | <name=identifier> '(' <.ws>
#S#          [ <named_param> | <param_var> <.ws> ]
#S#          [ ')' || <.panic: "Unable to parse named parameter; couldn't find right parenthesis"> ]
#S#      | <param_var>
#S#      ]
#S#  }
#S#  
#S#  token param_var {
#S#      [
#S#      | '[' ~ ']' <signature>
#S#      | '(' ~ ')' <signature>
#S#      | <sigil> <twigil>?
#S#          [
#S#              # Is it a longname declaration?
#S#          || <?{ $<sigil>.text eq '&' }> <?ident> {}
#S#              <identifier=sublongname>
#S#  
#S#          ||  # Is it a shaped array or hash declaration?
#S#              <?{ $<sigil>.text eq '@' || $<sigil>.text eq '%' }>
#S#              <identifier>?
#S#              <?before <[ \< \( \[ \{ ]> >
#S#              <postcircumfix>
#S#  
#S#              # ordinary parameter name
#S#          || <identifier>
#S#          || <[/!]>
#S#  
#S#              # bare sigil?
#S#          ]?
#S#      ]
#S#  }
#S#  
#S#  token parameter {
#S#      :my $kind;
#S#      :my $quant = '';
#S#      :my $q;
#S#  
#S#      # XXX fake out LTM till we get * working right
#S#      <?before
#S#          [
#S#          | <type_constraint>
#S#          | <param_var>
#S#          | '*' <param_var>
#S#          | '|' <param_var>
#S#          | '\\' <param_var>
#S#          | <named_param>
#S#          ]
#S#      >
#S#  
#S#      <type_constraint>*
#S#      [
#S#      | '*' <param_var>   { $quant = '*'; $kind = '*'; }
#S#      | '|' <param_var>   { $quant = '|'; $kind = '*'; }
#S#      | '\\' <param_var>  { $quant = '\\'; $kind = '!'; }
#S#      |   [
#S#          | <param_var>   { $quant = ''; $kind = '!'; }
#S#          | <named_param> { $quant = ''; $kind = '*'; }
#S#          ]
#S#          [
#S#          | '?'           { $quant = '?'; $kind = '?' }
#S#          | '!'           { $quant = '!'; $kind = '!' }
#S#          | <?>
#S#          ]
#S#      | <?>               { $quant = ''; $kind = '!'; }
#S#      ]
#S#  
#S#      <trait>*
#S#  
#S#      <post_constraint>*
#S#  
#S#      [
#S#          <default_value> {{
#S#              given $quant {
#S#                when '!' { $¢.panic("Can't put a default on a required parameter") }
#S#                when '*' { $¢.panic("Can't put a default on a slurpy parameter") }
#S#                when '|' { $¢.panic("Can't put a default on an slurpy capture parameter") }
#S#                when '\\' { $¢.panic("Can't put a default on a capture parameter") }
#S#              }
#S#              $kind = '?';
#S#          }}
#S#      ]?
#S#  
#S#      {
#S#          $<quant> = $quant;
#S#          $<kind> = $kind;
#S#      }
#S#  
#S#      # enforce zone constraints
#S#      {{
#S#          given $kind {
#S#              when '!' {
#S#                  given $+zone {
#S#                      when 'posopt' {
#S#  $¢.panic("Can't put required parameter after optional parameters");
#S#                      }
#S#                      when 'var' {
#S#  $¢.panic("Can't put required parameter after variadic parameters");
#S#                      }
#S#                  }
#S#              }
#S#              when '?' {
#S#                  given $+zone {
#S#                      when 'posreq' { $+zone = 'posopt' }
#S#                      when 'var' {
#S#  $¢.panic("Can't put optional positional parameter after variadic parameters");
#S#                      }
#S#                  }
#S#              }
#S#              when '*' {
#S#                  $+zone = 'var';
#S#              }
#S#          }
#S#      }}
#S#  }
#S#  
#S#  rule default_value {
#S#      '=' <EXPR(item %item_assignment)>
#S#  }
#S#  
#S#  token statement_prefix:do      { <sym> <?before \s> <.ws> <statement> }
#S#  token statement_prefix:try     { <sym> <?before \s> <.ws> <statement> }
#S#  token statement_prefix:gather  { <sym> <?before \s> <.ws> <statement> }
#S#  token statement_prefix:contend { <sym> <?before \s> <.ws> <statement> }
#S#  token statement_prefix:async   { <sym> <?before \s> <.ws> <statement> }
#S#  token statement_prefix:maybe   { <sym> <?before \s> <.ws> <statement> }
#S#  token statement_prefix:lazy    { <sym> <?before \s> <.ws> <statement> }
#S#  
#S#  ## term
#S#  
#S#  token term:sym<::?IDENT> ( --> Term) {
#S#      $<sym> = [ '::?' <identifier> ] »
#S#  }
#S#  
#S#  token term:sym<undef> ( --> Term) {
#S#      <sym> »
#S#      [ <?before \h*'$/' >
#S#          <obs('$/ variable as input record separator',
#S#               "the filehandle's .slurp method")>
#S#      ]?
#S#      [ <?before \h*<variable> >
#S#          <obs('undef as a verb', 'undefine function')>
#S#      ]?
#S#  }
#S#  
#S#  token term:sym<next> ( --> Term)
#S#      { <sym> » <.ws> [<!stdstopper> <termish>]? }
#S#  
#S#  token term:sym<last> ( --> Term)
#S#      { <sym> » <.ws> [<!stdstopper> <termish>]? }
#S#  
#S#  token term:sym<redo> ( --> Term)
#S#      { <sym> » <.ws> [<!stdstopper> <termish>]? }
#S#  
#S#  token term:sym<break> ( --> Term)
#S#      { <sym> » <.ws> [<!stdstopper> <termish>]? }
#S#  
#S#  token term:sym<continue> ( --> Term)
#S#      { <sym> » }
#S#  
#S#  token term:sym<goto> ( --> Term)
#S#      { <sym> » <.ws> <termish> }
#S#  
#S#  token term:sym<self> ( --> Term)
#S#      { <sym> » }
#S#  
#S#  token term:sym<defer> ( --> Term)
#S#      { <sym> » }
#S#  
#S#  token term:rand ( --> Term)
#S#      { <sym> » [ <?before \h+ [\d|'$']> <.obs('rand(N)', 'N.rand or (1..N).pick')> ]? }
#S#  
#S#  token term:e ( --> Term)
#S#      { <sym> » }
#S#  
#S#  token term:i ( --> Term)
#S#      { <sym> » }
#S#  
#S#  token term:pi ( --> Term)
#S#      { <sym> » }
#S#  
#S#  token term:Inf ( --> Term)
#S#      { <sym> » }
#S#  
#S#  token term:NaN ( --> Term)
#S#      { <sym> » }
#S#  
#S#  token term:sym<*> ( --> Term)
#S#      { <sym> }
#S#  
#S#  token term:sym<**> ( --> Term)
#S#      { <sym> }
#S#  
#S#  token circumfix:sigil ( --> Term)
#S#      { :dba('contextualizer') <sigil> '(' ~ ')' <semilist> { $+SIGIL ||= $<sigil>.text } }
#S#  
#S#  #token circumfix:typecast ( --> Term)
#S#  #    { <typename> '(' ~ ')' <semilist> }
#S#  
#S#  token circumfix:sym<( )> ( --> Term)
#S#      { :dba('parenthesized expression') '(' ~ ')' <semilist> }
#S#  
#S#  token circumfix:sym<[ ]> ( --> Term)
#S#      { :dba('array composer') '[' ~ ']' <semilist> }
#S#  
#S#  ## methodcall
#S#  
#S#  token postfix:sym<i> ( --> Methodcall)
#S#      { <sym> » }
#S#  
#S#  token infix:sym<.> ()
#S#      { '.' <[\]\)\},:\s\$"']> <obs('. to concatenate strings', '~')> }
#S#  
#S#  token postfix:sym['->'] ()
#S#      { '->' <obs('-> to call a method', '.')> }
#S#  
#S#  ## autoincrement
#S#  token postfix:sym<++> ( --> Autoincrement)
#S#      { <sym> }
#S#  
#S#  token postfix:sym<--> ( --> Autoincrement)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<++> ( --> Autoincrement)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<--> ( --> Autoincrement)
#S#      { <sym> }
#S#  
#S#  ## exponentiation
#S#  token infix:sym<**> ( --> Exponentiation)
#S#      { <sym> }
#S#  
#S#  ## symbolic unary
#S#  token prefix:sym<!> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<+> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<-> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<~> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<?> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<=> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<~^> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<+^> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<?^> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<^> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  token prefix:sym<|> ( --> Symbolic_unary)
#S#      { <sym> }
#S#  
#S#  
#S#  ## multiplicative
#S#  token infix:sym<*> ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  token infix:sym</> ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  token infix:sym<div> ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  token infix:sym<%> ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  token infix:sym<+&> ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  token infix:sym« +< » ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  token infix:sym« << » ( --> Multiplicative)
#S#      { <sym> <obs('<< to do left shift', '+< or ~<')> }
#S#  
#S#  token infix:sym« >> » ( --> Multiplicative)
#S#      { <sym> <obs('>> to do right shift', '+> or ~>')> }
#S#  
#S#  token infix:sym« +> » ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  token infix:sym<~&> ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  token infix:sym<?&> ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  token infix:sym« ~< » ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  token infix:sym« ~> » ( --> Multiplicative)
#S#      { <sym> }
#S#  
#S#  
#S#  ## additive
#S#  token infix:sym<+> ( --> Additive)
#S#      { <sym> }
#S#  
#S#  token infix:sym<-> ( --> Additive)
#S#      { <sym> }
#S#  
#S#  token infix:sym<+|> ( --> Additive)
#S#      { <sym> }
#S#  
#S#  token infix:sym<+^> ( --> Additive)
#S#      { <sym> }
#S#  
#S#  token infix:sym<~|> ( --> Additive)
#S#      { <sym> }
#S#  
#S#  token infix:sym<~^> ( --> Additive)
#S#      { <sym> }
#S#  
#S#  token infix:sym<?|> ( --> Additive)
#S#      { <sym> }
#S#  
#S#  token infix:sym<?^> ( --> Additive)
#S#      { <sym> }
#S#  
#S#  ## replication
#S#  # Note: no word boundary check after x, relies on longest token for x2 xx2 etc
#S#  token infix:sym<x> ( --> Replication)
#S#      { <sym> }
#S#  
#S#  token infix:sym<xx> ( --> Replication)
#S#      { <sym> }
#S#  
#S#  ## concatenation
#S#  token infix:sym<~> ( --> Concatenation)
#S#      { <sym> }
#S#  
#S#  
#S#  ## junctive and (all)
#S#  token infix:sym<&> ( --> Junctive_and)
#S#      { <sym> }
#S#  
#S#  token infix:sym<also> ( --> Junctive_and)
#S#      { <sym> }
#S#  
#S#  
#S#  ## junctive or (any)
#S#  token infix:sym<|> ( --> Junctive_or)
#S#      { <sym> }
#S#  
#S#  token infix:sym<^> ( --> Junctive_or)
#S#      { <sym> }
#S#  
#S#  
#S#  ## named unary examples
#S#  # (need \s* to win LTM battle with listops)
#S#  token prefix:sleep ( --> Named_unary)
#S#      { <sym> » <?before \s*> }
#S#  
#S#  token prefix:abs ( --> Named_unary)
#S#      { <sym> » <?before \s*> }
#S#  
#S#  token prefix:int ( --> Named_unary)
#S#      { <sym> » <?before \s*> }
#S#  
#S#  token prefix:let ( --> Named_unary)
#S#      { <sym> » <?before \s*> }
#S#  
#S#  token prefix:temp ( --> Named_unary)
#S#      { <sym> » <?before \s*> }
#S#  
#S#  ## nonchaining binary
#S#  token infix:sym« <=> » ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:cmp ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:leg ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:but ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:does ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<..> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<^..> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<..^> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<^..^> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<ff> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<^ff> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<ff^> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<^ff^> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<fff> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<^fff> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<fff^> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<^fff^> ( --> Nonchaining)
#S#      { <sym> }
#S#  
#S#  
#S#  ## chaining binary
#S#  token infix:sym<==> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<!=> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym« < » ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym« <= » ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym« > » ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym« >= » ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<~~> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<!~> ( --> Chaining)
#S#      { <sym> <obs('!~ to do negated pattern matching', '!~~')> }
#S#  
#S#  token infix:sym<=~> ( --> Chaining)
#S#      { <sym> <obs('=~ to do pattern matching', '~~')> }
#S#  
#S#  token infix:sym<eq> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<ne> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<lt> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<le> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<gt> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<ge> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<=:=> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<===> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  token infix:sym<eqv> ( --> Chaining)
#S#      { <sym> }
#S#  
#S#  
#S#  ## tight and
#S#  token infix:sym<&&> ( --> Tight_and)
#S#      { <sym> }
#S#  
#S#  
#S#  ## tight or
#S#  token infix:sym<||> ( --> Tight_or)
#S#      { <sym> }
#S#  
#S#  token infix:sym<^^> ( --> Tight_or)  {
#S#      <sym>
#S#      { $<O><assoc> := 'list' }  # override Tight_or's 'left' associativity
#S#  }
#S#  
#S#  token infix:sym<//> ( --> Tight_or)
#S#      { <sym> }
#S#  
#S#  token infix:sym<min> ( --> Tight_or)
#S#      { <sym> }
#S#  
#S#  token infix:sym<max> ( --> Tight_or)
#S#      { <sym> }
#S#  
#S#  
#S#  ## conditional
#S#  token infix:sym<?? !!> ( --> Conditional) {
#S#      :my $GOAL is context = '!!';
#S#      '??'
#S#      <.ws>
#S#      <EXPR(item %item_assignment)>
#S#      [ '!!' ||
#S#          [
#S#          || <?before '='> <.panic: "Assignment not allowed within ??!!">
#S#          || <?before '::'> <.panic: "Please use !! rather than ::">
#S#          || <?before <infix>>    # Note: a tight infix would have parsed right
#S#              <.panic: "Precedence too loose within ??!!; use ??()!! instead ">
#S#          || <.panic: "Found ?? but no !!; possible precedence problem">
#S#          ]
#S#      ]
#S#  }
#S#  
#S#  token infix:sym<?> ( --> Conditional)
#S#      { <sym> <obs('?: for the conditional operator', '??!!')> }
#S#  
#S#  ## assignment
#S#  # There is no "--> type" because assignment may be coerced to either
#S#  # item assignment or list assignment at "make" time.
#S#  
#S#  token infix:sym<=> ()
#S#  {
#S#      <sym>
#S#      { $¢ = $+SIGIL eq '$' 
#S#          ?? STD::Item_assignment.coerce($¢)
#S#          !! STD::List_assignment.coerce($¢);
#S#      }
#S#  }
#S#  
#S#  token infix:sym<:=> ( --> Item_assignment)
#S#      { <sym> }
#S#  
#S#  token infix:sym<::=> ( --> Item_assignment)
#S#      { <sym> }
#S#  
#S#  token infix:sym<.=> ( --> Item_assignment) {
#S#      <sym> <.ws>
#S#      [ <?before \w+';' | 'new'|'sort'|'subst'|'trans'|'reverse'|'uniq'|'map'|'samecase'|'substr' > || <worryobs('.= as append operator', '~=')> ]
#S#      { $<O><nextterm> = 'dottyop' }
#S#  }
#S#  
#S#  token infix:sym« => » ( --> Item_assignment)
#S#      { <sym> }
#S#  
#S#  # Note, other assignment ops generated by infix_postfix_meta_operator rule
#S#  
#S#  ## loose unary
#S#  token prefix:sym<true> ( --> Loose_unary)
#S#      { <sym> » }
#S#  
#S#  token prefix:sym<not> ( --> Loose_unary)
#S#      { <sym> » }
#S#  
#S#  ## list item separator
#S#  token infix:sym<,> ( --> Comma)
#S#      { <sym> }
#S#  
#S#  token infix:sym<:> ( --> Comma)
#S#      { <sym> }
#S#  
#S#  token infix:sym« p5=> » ( --> Comma)
#S#      { <sym> }
#S#  
#S#  ## list infix
#S#  token infix:sym<X> ( --> List_infix)
#S#      { <sym> }
#S#  
#S#  token infix:sym<Z> ( --> List_infix)
#S#      { <sym> }
#S#  
#S#  token infix:sym<minmax> ( --> List_infix)
#S#      { <sym> }
#S#  
#S#  token infix:sym<...> ( --> List_infix)
#S#      { <sym> }
#S#  
#S#  token term:sym<...> ( --> List_prefix)
#S#      { <sym> <args>? }
#S#  
#S#  token term:sym<???> ( --> List_prefix)
#S#      { <sym> <args>? }
#S#  
#S#  token term:sym<!!!> ( --> List_prefix)
#S#      { <sym> <args>? }
#S#  
#S#  token term:sigil ( --> List_prefix)
#S#  {
#S#      <sigil> <?before \s> <arglist>
#S#      { $<sym> = $<sigil>.item; }
#S#  }
#S#  
#S#  # token term:typecast ( --> List_prefix)
#S#  #     { <typename> <?spacey> <arglist> { $<sym> = $<typename>.item; } }
#S#  
#S#  # force identifier(), identifier.(), etc. to be a function call always
#S#  token term:identifier ( --> Term )
#S#  {
#S#      :my $t;
#S#      <identifier> <?before ['.'?'(']?>
#S#      { $t = $<identifier>.text; }
#S#      <args( $¢.is_type($t) )>
#S#      {{
#S#          %ROUTINES{$t} ~= $¢.lineof($¢.pos) ~ ' ' unless $¢.is_routine($t);
#S#      }}
#S#  }
#S#  
#S#  token term:opfunc ( --> Term )
#S#  {
#S#      <category> <colonpair>+ <args>
#S#  }
#S#  
#S#  token args ($istype = 0) {
#S#      :my $listopish = 0;
#S#      :my $GOAL is context = '';
#S#      [
#S#      | :dba('argument list') '.(' ~ ')' <semilist> {*}             #= func args
#S#      | :dba('argument list') '(' ~ ')' <semilist> {*}              #= func args
#S#      | :dba('argument list') <.unsp> '.'? '(' ~ ')' <semilist> {*} #= func args
#S#      | {} [<?before \s> <!{ $istype }> <.ws> <!infixstopper> <listopargs=arglist> { $listopish = 1 }]?
#S#      ]
#S#  
#S#      :dba('extra arglist after (...):')
#S#      [
#S#      || <?{ $listopish }>
#S#      || ':' <?before \s> <listopargs=arglist>    # either switch to listopiness
#S#      || {{ $<O> = {}; }}   # or allow adverbs (XXX needs hoisting?)
#S#      ]
#S#  }
#S#  
#S#  # names containing :: may or may not be function calls
#S#  # bare identifier without parens also handled here if no other rule parses it
#S#  token term:name ( --> Term)
#S#  {
#S#      <longname>
#S#      [
#S#      ||  <?{
#S#              $¢.is_type($<longname>.text) or substr($<longname>.text,0,2) eq '::'
#S#          }>
#S#          # parametric type?
#S#          <.unsp>? [ <?before '['> <postcircumfix> ]?
#S#          :dba('type parameter')
#S#          [
#S#              '::'
#S#              <?before [ '«' | '<' | '{' | '<<' ] > <postcircumfix>
#S#              {*}                                                 #= packagevar 
#S#          ]?
#S#          {*}                                                     #= typename
#S#  
#S#      # unrecognized names are assumed to be post-declared listops.
#S#      || <args>?
#S#      ]
#S#  }
#S#  
#S#  ## loose and
#S#  token infix:sym<and> ( --> Loose_and)
#S#      { <sym> }
#S#  
#S#  token infix:sym<andthen> ( --> Loose_and)
#S#      { <sym> }
#S#  
#S#  token infix:sym<andthen> ( --> Loose_and)
#S#      { <sym> }
#S#  
#S#  ## loose or
#S#  token infix:sym<or> ( --> Loose_or)
#S#      { <sym> }
#S#  
#S#  token infix:sym<orelse> ( --> Loose_or)
#S#      { <sym> }
#S#  
#S#  token infix:sym<xor> ( --> Loose_or)
#S#      { <sym> }
#S#  
#S#  token infix:sym<orelse> ( --> Loose_or)
#S#       { <sym> }
#S#  
#S#  token infix:sym<;> ( --> Sequencer)
#S#      { <sym> }
#S#  
#S#  token infix:sym« <== » ( --> Sequencer)
#S#      { <sym> }
#S#  
#S#  token infix:sym« ==> » ( --> Sequencer)
#S#      { <sym> {*} }              #'
#S#  
#S#  token infix:sym« <<== » ( --> Sequencer)
#S#      { <sym> }
#S#  
#S#  token infix:sym« ==>> » ( --> Sequencer)
#S#      { <sym> {*} }              #'
#S#  
#S#  ## expression terminator
#S#  # Note: must always be called as <?terminator> or <?before ...<terminator>...>
#S#  
#S#  token terminator:sym<;> ( --> Terminator)
#S#      { ';' }
#S#  
#S#  token terminator:sym<if> ( --> Terminator)
#S#      { 'if' » <.nofun> }
#S#  
#S#  token terminator:sym<unless> ( --> Terminator)
#S#      { 'unless' » <.nofun> }
#S#  
#S#  token terminator:sym<while> ( --> Terminator)
#S#      { 'while' » <.nofun> }
#S#  
#S#  token terminator:sym<until> ( --> Terminator)
#S#      { 'until' » <.nofun> }
#S#  
#S#  token terminator:sym<for> ( --> Terminator)
#S#      { 'for' » <.nofun> }
#S#  
#S#  token terminator:sym<given> ( --> Terminator)
#S#      { 'given' » <.nofun> }
#S#  
#S#  token terminator:sym<when> ( --> Terminator)
#S#      { 'when' » <.nofun> }
#S#  
#S#  token terminator:sym« --> » ( --> Terminator)
#S#      { '-->' {*} }              #'
#S#  
#S#  token terminator:sym<)> ( --> Terminator)
#S#      { <sym> }
#S#  
#S#  token terminator:sym<]> ( --> Terminator)
#S#      { ']' }
#S#  
#S#  token terminator:sym<}> ( --> Terminator)
#S#      { '}' }
#S#  
#S#  token terminator:sym<!!> ( --> Terminator)
#S#      { '!!' <?{ $+GOAL eq '!!' }> }
#S#  
#S#  regex infixstopper {
#S#      :dba('infix stopper')
#S#      [
#S#      | <?before <stopper> >
#S#      | <?before '!!' > <?{ $+GOAL eq '!!' }>
#S#      | <?before '{' | <lambda> > <?{ ($+GOAL eq '{' or $+GOAL eq 'endargs') and @+MEMOS[$¢.pos]<ws> }>
#S#      | <?{ $+GOAL eq 'endargs' and @+MEMOS[$¢.pos]<endargs> }>
#S#      ]
#S#  }
#S#  
#S#  # overridden in subgrammars
#S#  token stopper { <!> }
#S#  
#S#  # hopefully we can include these tokens in any outer LTM matcher
#S#  regex stdstopper {
#S#      :my @stub = return self if @+MEMOS[self.pos]<endstmt> :exists;
#S#      :dba('standard stopper')
#S#      [
#S#      | <?terminator>
#S#      | <?unitstopper>
#S#      | $                                 # unlikely, check last (normal LTM behavior)
#S#      ]
#S#      { @+MEMOS[$¢.pos]<endstmt> ||= 1; }
#S#  }
#S#  
#S#  # A fairly complete operator precedence parser
#S#  
#S#  method EXPR ($preclvl)
#S#  {
#S#      temp $CTX = self.callm if $*DEBUG +& DEBUG::trace_call;
#S#      if self.peek {
#S#          return self._AUTOLEXpeek('EXPR', $retree);
#S#      }
#S#      my $preclim = $preclvl ?? $preclvl.<prec> // $LOOSEST !! $LOOSEST;
#S#      my $inquote is context = 0;
#S#      my $SIGIL is context<rw> = '';
#S#      my @termstack;
#S#      my @opstack;
#S#      my $termish = 'termish';
#S#  
#S#      push @opstack, { 'O' => item %terminator, 'sym' => '' };         # (just a sentinel value)
#S#  
#S#      my $here = self;
#S#      my $S = $here.pos;
#S#      self.deb("In EXPR, at $S") if $*DEBUG +& DEBUG::EXPR;
#S#  
#S#      my &reduce := -> {
#S#          self.deb("entering reduce, termstack == ", +@termstack, " opstack == ", +@opstack) if $*DEBUG +& DEBUG::EXPR;
#S#          my $op = pop @opstack;
#S#          my $sym = $op<sym>;
#S#          given $op<O><assoc> // 'unary' {
#S#              when 'chain' {
#S#                  self.deb("reducing chain") if $*DEBUG +& DEBUG::EXPR;
#S#                  my @chain;
#S#                  push @chain, pop(@termstack);
#S#                  push @chain, $op;
#S#                  while @opstack {
#S#                      last if $op<O><prec> ne @opstack[*-1]<O><prec>;
#S#                      push @chain, pop(@termstack);
#S#                      push @chain, pop(@opstack);
#S#                  }
#S#                  push @chain, pop(@termstack);
#S#                  @chain = reverse @chain if @chain > 1;
#S#                  my $startpos = @chain[0].pos;
#S#                  my $nop = $op.cursor_fresh();
#S#                  $nop<chain> = [@chain];
#S#                  $nop<_arity> = 'CHAIN';
#S#                  push @termstack, $nop._REDUCE($startpos, 'EXPR');
#S#              }
#S#              when 'list' {
#S#                  self.deb("reducing list") if $*DEBUG +& DEBUG::EXPR;
#S#                  my @list;
#S#                  my @delims = $op;
#S#                  push @list, pop(@termstack);
#S#                  while @opstack {
#S#                      self.deb($sym ~ " vs " ~ @opstack[*-1]<sym>) if $*DEBUG +& DEBUG::EXPR;
#S#                      last if $sym ne @opstack[*-1]<sym>;
#S#                      if @termstack and defined @termstack[0] {
#S#                          push @list, pop(@termstack);
#S#                      }
#S#                      else {
#S#                          self.worry("Missing term in " ~ $sym ~ " list");
#S#                      }
#S#                      push @delims, pop(@opstack);
#S#                  }
#S#                  if @termstack and defined @termstack[0] {
#S#                      push @list, pop(@termstack);
#S#                  }
#S#                  elsif $sym ne ',' {
#S#                      self.worry("Missing final term in '" ~ $sym ~ "' list");
#S#                  }
#S#                  @list = reverse @list if @list > 1;
#S#                  my $startpos = @list[0].pos;
#S#                  @delims = reverse @delims if @delims > 1;
#S#                  my $nop = $op.cursor_fresh();
#S#                  $nop<sym> = $sym;
#S#                  $nop<O> = $op<O>;
#S#                  $nop<list> = [@list];
#S#                  $nop<delims> = [@delims];
#S#                  $nop<_arity> = 'LIST';
#S#                  push @termstack, $nop._REDUCE($startpos, 'EXPR');
#S#              }
#S#              when 'unary' {
#S#                  self.deb("reducing") if $*DEBUG +& DEBUG::EXPR;
#S#                  my @list;
#S#                  self.deb("Termstack size: ", +@termstack) if $*DEBUG +& DEBUG::EXPR;
#S#  
#S#                  self.deb($op.dump) if $*DEBUG +& DEBUG::EXPR;
#S#                  $op<arg> = (pop @termstack);
#S#                  if ($op<arg><_from> < $op<_from>) {
#S#                      $op<_from> = $op<arg><_from>;
#S#                  }
#S#                  if ($op<arg><_pos> > $op<_pos>) {
#S#                      $op<_pos> = $op<arg><_pos>;
#S#                  }
#S#                  $op<_arity> = 'UNARY';
#S#                  push @termstack, $op._REDUCE($op<_from>, 'EXPR');
#S#              }
#S#              default {
#S#                  self.deb("reducing") if $*DEBUG +& DEBUG::EXPR;
#S#                  my @list;
#S#                  self.deb("Termstack size: ", +@termstack) if $*DEBUG +& DEBUG::EXPR;
#S#  
#S#                  $op<right> = (pop @termstack);
#S#                  $op<left> = (pop @termstack);
#S#                  $op<_from> = $op<left><_from>;
#S#                  $op<_pos> = $op<right><_pos>;
#S#                  $op<_arity> = 'BINARY';
#S#                  self.deb($op.dump) if $*DEBUG +& DEBUG::EXPR;
#S#                  push @termstack, $op._REDUCE($op<_from>, 'EXPR');
#S#              }
#S#          }
#S#      };
#S#  
#S#    TERM:
#S#      loop {
#S#          self.deb("In loop, at ", $here.pos) if $*DEBUG +& DEBUG::EXPR;
#S#          my $oldpos = $here.pos;
#S#          $here = $here.cursor_fresh();
#S#          $SIGIL = @opstack[*-1]<O><prec> gt $item_assignment_prec ?? '@' !! '';
#S#          my @t = $here.$termish;
#S#  
#S#          if not @t or not $here = @t[0] or ($here.pos == $oldpos and $termish eq 'termish') {
#S#              return ();
#S#              # $here.panic("Failed to parse a required term");
#S#          }
#S#          $termish = 'termish';
#S#  
#S#          # interleave prefix and postfix, pretend they're infixish
#S#          my $M = $here;
#S#  
#S#          # note that we push loose stuff onto opstack before tight stuff
#S#          my @pre;
#S#          my $tmp;
#S#          @pre = @$tmp if $tmp = ( $M<PRE> :delete );
#S#          my @post;
#S#          @post = reverse @$tmp if $tmp = ( $M<POST> :delete );
#S#          while @pre and @post {
#S#              if @post[0]<O><prec> le @pre[0]<O><prec> {
#S#                  push @opstack, shift @post;
#S#              }
#S#              else {
#S#                  push @opstack, shift @pre;
#S#              }
#S#          }
#S#          push @opstack, @pre,@post;
#S#  
#S#          push @termstack, $here;
#S#          self.deb("after push: " ~ (0+@termstack)) if $*DEBUG +& DEBUG::EXPR;
#S#  
#S#          loop {     # while we see adverbs
#S#              $oldpos = $here.pos;
#S#              last TERM if (@+MEMOS[$oldpos]<endstmt> // 0) == 2;
#S#              $here = $here.cursor_fresh.ws;
#S#              my @infix = $here.cursor_fresh.infixish();
#S#              last TERM unless @infix;
#S#              my $infix = @infix[0];
#S#              last TERM unless $infix.pos > $oldpos;
#S#              
#S#              if not $infix<sym> {
#S#                  die $infix.dump if $*DEBUG +& DEBUG::EXPR;
#S#              }
#S#  
#S#              my $inO = $infix<O>;
#S#              my Str $inprec = $inO<prec>;
#S#              if not defined $inprec {
#S#                  self.deb("No prec given in infix!") if $*DEBUG +& DEBUG::EXPR;
#S#                  die $infix.dump if $*DEBUG +& DEBUG::EXPR;
#S#                  $inprec = %terminator<prec>;
#S#              }
#S#  
#S#              last TERM unless $inprec gt $preclim;
#S#  
#S#              $here = $infix.cursor_fresh.ws();
#S#  
#S#              # substitute precedence for listops
#S#              $inO<prec> = $inO<sub> if $inO<sub>;
#S#  
#S#              # Does new infix (or terminator) force any reductions?
#S#              while @opstack[*-1]<O><prec> gt $inprec {
#S#                  reduce();
#S#              }
#S#  
#S#              # Not much point in reducing the sentinels...
#S#              last if $inprec lt $LOOSEST;
#S#  
#S#              # Equal precedence, so use associativity to decide.
#S#              if @opstack[*-1]<O><prec> eq $inprec {
#S#                  given $inO<assoc> {
#S#                      when 'non'   { $here.panic('"' ~ $infix.text ~ '" is not associative') }
#S#                      when 'left'  { reduce() }   # reduce immediately
#S#                      when 'right' { }            # just shift
#S#                      when 'chain' { }            # just shift
#S#                      when 'list'  {              # if op differs reduce else shift
#S#                          reduce() if $infix<sym> !eqv @opstack[*-1]<sym>;
#S#                      }
#S#                      default { $here.panic("Unknown associativity \"$_\" for \"$infix\"") }
#S#                  }
#S#              }
#S#              if $infix<fake> {
#S#                  my $adverbs = @termstack[*-1]<ADV> ||= [];
#S#                  push @$adverbs, $infix<colonpair>;
#S#                  next;  # not really an infix, so keep trying
#S#              }
#S#              else {
#S#                  $termish = $inO<nextterm> if $inO<nextterm>;
#S#                  push @opstack, $infix;
#S#                  last;
#S#              }
#S#          }
#S#      }
#S#      reduce() while +@opstack > 1;
#S#      if @termstack {
#S#          +@termstack == 1 or $here.panic("Internal operator parser error, termstack == " ~ (+@termstack));
#S#          @termstack[0]<_from> = self.pos;
#S#          @termstack[0]<_pos> = $here.pos;
#S#      }
#S#      self._MATCHIFYr($S, "EXPR", @termstack);
#S#  }
#S#  
#S#  #################################################
#S#  ## Regex
#S#  #################################################
#S#  
#S#  grammar Regex is STD {
grammar Regex is STD {
#S#  
#S#      # begin tweaks (DO NOT ERASE)
#S#      multi method tweak (:Perl5(:$P5)) { self.cursor_fresh( ::STD::Q ).mixin( ::q ).mixin( ::p5 ) }
#S#      multi method tweak (:overlap(:$ov)) { self }
#S#      multi method tweak (:exhaustive(:$ex)) { self }
#S#      multi method tweak (:continue(:$c)) { self }
#S#      multi method tweak (:pos(:$p)) { self }
#S#      multi method tweak (:sigspace(:$s)) { self }
#S#      multi method tweak (:ratchet(:$r)) { self }
#S#      multi method tweak (:global(:$g)) { self }
#S#      multi method tweak (:ignorecase(:$i)) { self }
#S#      multi method tweak (:ignoreaccent(:$a)) { self }
#S#      multi method tweak (:samecase(:$ii)) { self }
#S#      multi method tweak (:sameaccent(:$aa)) { self }
#S#      multi method tweak (:$nth) { self }
#S#      multi method tweak (:st(:$nd)) { self }
#S#      multi method tweak (:rd(:$th)) { self }
#S#      multi method tweak (:$x) { self }
#S#      multi method tweak (:$bytes) { self }
#S#      multi method tweak (:$codes) { self }
#S#      multi method tweak (:$graphs) { self }
#S#      multi method tweak (:$chars) { self }
#S#      multi method tweak (:$rw) { self.panic(":rw not implemented") }
#S#      multi method tweak (:$keepall) { self.panic(":keepall not implemented") }
#S#      multi method tweak (:$panic) { self.panic(":panic not implemented") }
#S#      # end tweaks (DO NOT ERASE)
#S#  
#S#      token category:metachar { <sym> }
#S#      proto token metachar { <...> }
#S#  
#S#      token category:backslash { <sym> }
#S#      proto token backslash { <...> }
#S#  
#S#      token category:assertion { <sym> }
#S#      proto token assertion { <...> }
#S#  
#S#      token category:quantifier { <sym> }
#S#      proto token quantifier { <...> }
#S#  
#S#      token category:mod_internal { <sym> }
#S#      proto token mod_internal { <...> }
#S#  
#S#      proto token rxinfix { <...> }
#S#  
#S#      token ws {
#S#          <?{ $+sigspace }>
#S#          || [ <?before \s | '#'> <nextsame> ]?   # still get all the pod goodness, hopefully
#S#      }
#S#  
#S#      token sigspace {
#S#          <?before \s | '#'> [ :lang($¢.cursor_fresh($+LANG)) <.ws> ]
#S#      }
#S#  
#S#      # suppress fancy end-of-line checking
#S#      token codeblock {
#S#          :my $GOAL is context = '}';
#S#          '{' :: [ :lang($¢.cursor_fresh($+LANG)) <statementlist> ]
#S#          [ '}' || <.panic: "Unable to parse statement list; couldn't find right brace"> ]
#S#      }
#S#  
#S#      rule nibbler {
#S#          :my $sigspace    is context<rw> = $+sigspace    // 0;
#S#          :my $ratchet     is context<rw> = $+ratchet     // 0;
#S#          :my $ignorecase is context<rw> = $+ignorecase // 0;
#S#          :my $ignoreaccent    is context<rw> = $+ignoreaccent    // 0;
#S#          [ \s* < || | && & > ]?
#S#          <EXPR>
#S#      }
#S#  
#S#      token termish {
#S#          <.ws>
#S#          <quantified_atom>+
#S#      }
#S#      token infixish {
#S#          <!infixstopper>
#S#          <!stdstopper>
#S#          <rxinfix>
#S#          {
#S#              $<O> = $<rxinfix><O>;
#S#              $<sym> = $<rxinfix><sym>;
#S#          }
#S#      }
#S#  
#S#      token rxinfix:sym<||> ( --> Tight_or ) { <sym> }
#S#      token rxinfix:sym<&&> ( --> Tight_and ) { <sym> }
#S#      token rxinfix:sym<|> ( --> Junctive_or ) { <sym> }
#S#      token rxinfix:sym<&> ( --> Junctive_and ) { <sym> }
#S#      token rxinfix:sym<~> ( --> Additive ) { <sym> }
#S#  
#S#      token quantified_atom {
#S#          <!stopper>
#S#          <!rxinfix>
#S#          <atom>
#S#          [ <.ws> <quantifier>
#S#  #            <?{ $<atom>.max_width }>
#S#  #                || <.panic: "Can't quantify zero-width atom">
#S#          ]?
#S#          <.ws>
#S#      }
#S#  
#S#      token atom {
#S#          :dba('regex atom')
#S#          [
#S#          | \w
#S#          | <metachar> ::
#S#          | <.panic: "Unrecognized regex metacharacter">
#S#          ]
#S#      }
#S#  
#S#      # sequence stoppers
#S#      token metachar:sym« > » { '>'  :: <fail> }
#S#      token metachar:sym<&&>  { '&&' :: <fail> }
#S#      token metachar:sym<&>   { '&'  :: <fail> }
#S#      token metachar:sym<||>  { '||' :: <fail> }
#S#      token metachar:sym<|>   { '|'  :: <fail> }
#S#      token metachar:sym<]>   { ']'  :: <fail> }
#S#      token metachar:sym<)>   { ')'  :: <fail> }
#S#  
#S#      token metachar:quant { <quantifier> <.panic: "quantifier quantifies nothing"> }
#S#  
#S#      # "normal" metachars
#S#  
#S#      token metachar:sigwhite {
#S#          <sigspace>
#S#      }
#S#  
#S#      token metachar:sym<{ }> {
#S#          <?before '{'>
#S#          <codeblock>
#S#          {{ $/<sym> := <{ }> }}
#S#      }
#S#  
#S#      token metachar:mod {
#S#          <mod_internal>
#S#          { $/<sym> := $<mod_internal><sym> }
#S#      }
#S#  
#S#      token metachar:sym<:> {
#S#          <sym>
#S#      }
#S#  
#S#      token metachar:sym<::> {
#S#          <sym>
#S#      }
#S#  
#S#      token metachar:sym<:::> {
#S#          <sym>
#S#      }
#S#  
#S#      token metachar:sym<[ ]> {
#S#          '[' {} [:lang(self.unbalanced(']')) <nibbler>]
#S#          [ ']' || <.panic: "Unable to parse regex; couldn't find right bracket"> ]
#S#          { $/<sym> := <[ ]> }
#S#      }
#S#  
#S#      token metachar:sym<( )> {
#S#          '(' {} [:lang(self.unbalanced(')')) <nibbler>]
#S#          [ ')' || <.panic: "Unable to parse regex; couldn't find right parenthesis"> ]
#S#          { $/<sym> := <( )> }
#S#      }
#S#  
#S#      token metachar:sym« <( » { '<(' }
#S#      token metachar:sym« )> » { ')>' }
#S#  
#S#      token metachar:sym« << » { '<<' }
#S#      token metachar:sym« >> » { '>>' }
#S#      token metachar:sym< « > { '«' }
#S#      token metachar:sym< » > { '»' }
#S#  
#S#      token metachar:qw {
#S#          <?before '<' \s >  # (note required whitespace)
#S#          <quote>
#S#      }
#S#  
#S#      token metachar:sym«< >» {
#S#          '<' <unsp>? {} <assertion>
#S#          [ '>' || <.panic: "regex assertion not terminated by angle bracket"> ]
#S#      }
#S#  
#S#      token metachar:sym<\\> { <sym> <backslash> }
#S#      token metachar:sym<.>  { <sym> }
#S#      token metachar:sym<^^> { <sym> }
#S#      token metachar:sym<^>  { <sym> }
#S#      token metachar:sym<$$> {
#S#          <sym>
#S#          [ (\w+) <obs("\$\$" ~ $0.text ~ " to deref var inside a regex", "\$(\$" ~ $0.text ~ ")")> ]?
#S#      }
#S#      token metachar:sym<$>  {
#S#          '$'
#S#          <?before
#S#          | \s
#S#          | '|'
#S#          | '&'
#S#          | ')'
#S#          | ']'
#S#          | '>'
#S#          | $
#S#          | <stopper>
#S#          >
#S#      }
#S#  
#S#      token metachar:sym<' '> { <?before "'"> [:lang($¢.cursor_fresh($+LANG)) <quote>] }
#S#      token metachar:sym<" "> { <?before '"'> [:lang($¢.cursor_fresh($+LANG)) <quote>] }
#S#  
#S#      token metachar:var {
#S#          <!before '$$'>
#S#          <?before <sigil>>
#S#          [:lang($¢.cursor_fresh($+LANG)) <variable> <.ws> ]
#S#          $<binding> = ( <.ws> '=' <.ws> <quantified_atom> )?
#S#          { $<sym> = $<variable>.item; }
#S#      }
#S#  
#S#      token backslash:unspace { <?before \s> <.SUPER::ws> }
#S#  
#S#      token backslash:sym<0> { '0' <!before <[0..7]> > }
#S#  
#S#      token backslash:A { <sym> <.obs('\\A as beginning-of-string matcher', '^')> }
#S#      token backslash:a { <sym> <.panic: "\\a is allowed only in strings, not regexes"> }
#S#      token backslash:b { :i <sym> }
#S#      token backslash:c { :i <sym>
#S#          [
#S#          | <codepoint>
#S#          | \d+
#S#          | [ <[ ?.._ ]> || <.panic: "Unrecognized \\c character"> ]
#S#          ]
#S#      }
#S#      token backslash:d { :i <sym> }
#S#      token backslash:e { :i <sym> }
#S#      token backslash:f { :i <sym> }
#S#      token backslash:h { :i <sym> }
#S#      token backslash:n { :i <sym> }
#S#      token backslash:o { :i <sym> [ <octint> | '['<octint>[','<octint>]*']' ] }
#S#      token backslash:Q { <sym> <obs('\\Q as quotemeta', 'quotes or literal variable match')> }
#S#      token backslash:r { :i <sym> }
#S#      token backslash:s { :i <sym> }
#S#      token backslash:t { :i <sym> }
#S#      token backslash:v { :i <sym> }
#S#      token backslash:w { :i <sym> }
#S#      token backslash:x { :i <sym> [ <hexint> | '[' [<.ws><hexint><.ws> ] ** ',' ']' ] }
#S#      token backslash:z { <sym> <obs('\\z as end-of-string matcher', '$')> }
#S#      token backslash:Z { <sym> <obs('\\Z as end-of-string matcher', '\\n?$')> }
#S#      token backslash:misc { $<litchar>=(\W) }
#S#      token backslash:oops { <.panic: "Unrecognized regex backslash sequence"> }
#S#  
#S#      token assertion:sym<...> { <sym> }
#S#      token assertion:sym<???> { <sym> }
#S#      token assertion:sym<!!!> { <sym> }
#S#  
#S#      token assertion:sym<?> { <sym> [ <?before '>'> | <assertion> ] }
#S#      token assertion:sym<!> { <sym> [ <?before '>'> | <assertion> ] }
#S#      token assertion:sym<*> { <sym> [ <?before '>'> | <.ws> <nibbler> ] }
#S#  
#S#      token assertion:sym<{ }> { <codeblock> }
#S#  
#S#      token assertion:variable {
#S#          <?before <sigil>>  # note: semantics must be determined per-sigil
#S#          [:lang($¢.cursor_fresh($+LANG).unbalanced('>')) <variable=EXPR(item %LOOSEST)>]
#S#      }
#S#  
#S#      token assertion:method {
#S#          '.' [
#S#              | <?before <alpha> > <assertion>
#S#              | [ :lang($¢.cursor_fresh($+LANG).unbalanced('>')) <dottyop> ]
#S#              ]
#S#      }
#S#  
#S#      token assertion:identifier { <identifier> [               # is qq right here?
#S#                                      | <?before '>' >
#S#                                      | <.ws> <nibbler>
#S#                                      | '=' <assertion>
#S#                                      | ':' <.ws>
#S#                                          [ :lang($¢.cursor_fresh($+LANG).unbalanced('>')) <arglist> ]
#S#                                      | '(' {}
#S#                                          [ :lang($¢.cursor_fresh($+LANG)) <arglist> ]
#S#                                          [ ')' || <.panic: "Assertion call missing right parenthesis"> ]
#S#                                      ]?
#S#      }
#S#  
#S#      # stupid special case
#S#      token assertion:name { <?before \w*'::('> [ :lang($¢.cursor_fresh($+LANG).unbalanced('>')) <name> ]
#S#                                      [
#S#                                      | <?before '>' >
#S#                                      | <.ws> <nibbler>
#S#                                      | '=' <assertion>
#S#                                      | ':' <.ws>
#S#                                          [ :lang($¢.cursor_fresh($+LANG).unbalanced('>')) <arglist> ]
#S#                                      | '(' {}
#S#                                          [ :lang($¢.cursor_fresh($+LANG)) <arglist> ]
#S#                                          [ ')' || <.panic: "Assertion call missing right parenthesis"> ]
#S#                                      ]?
#S#      }
#S#  
#S#      token assertion:sym<[> { <before '[' > <cclass_elem> ** < + - > }
#S#      token assertion:sym<+> { <sym> <cclass_elem> ** < + - > }
#S#      token assertion:sym<-> { <sym> <cclass_elem> ** < + - > }
#S#      token assertion:sym<.> { <sym> }
#S#      token assertion:sym<,> { <sym> }
#S#      token assertion:sym<~~> { <sym> [ <?before '>'> | \d+ | <desigilname> ] }
#S#  
#S#      token assertion:bogus { <.panic: "Unrecognized regex assertion"> }
#S#  
#S#      token cclass_elem {
#S#          <.ws>
#S#          :dba('character class element')
#S#          [
#S#          | <name>
#S#          | <before '['> <quibble($¢.cursor_fresh( ::STD::Q ).tweak(:q))> # XXX parse as q[] for now
#S#          ]
#S#          <.ws>
#S#      }
#S#  
#S#      token mod_arg { :dba('modifier argument') '(' ~ ')' <semilist> }
#S#  
#S#      token mod_internal:sym<:my>    { ':' <?before 'my' \s > [:lang($¢.cursor_fresh($+LANG)) <statement> <eat_terminator> ] }
#S#  
#S#      # XXX needs some generalization
#S#  
#S#      token mod_internal:sym<:i>    { $<sym>=[':i'|':ignorecase'] » { $+ignorecase = 1 } }
#S#      token mod_internal:sym<:!i>   { $<sym>=[':!i'|':!ignorecase'] » { $+ignorecase = 0 } }
#S#      token mod_internal:sym<:i( )> { $<sym>=[':i'|':ignorecase'] <mod_arg> { $+ignorecase = eval $<mod_arg>.text } }
#S#      token mod_internal:sym<:0i>   { ':' (\d+) ['i'|'ignorecase'] { $+ignorecase = $0 } }
#S#  
#S#      token mod_internal:sym<:a>    { $<sym>=[':a'|':ignoreaccent'] » { $+ignoreaccent = 1 } }
#S#      token mod_internal:sym<:!a>   { $<sym>=[':!a'|':!ignoreaccent'] » { $+ignoreaccent = 0 } }
#S#      token mod_internal:sym<:a( )> { $<sym>=[':a'|':ignoreaccent'] <mod_arg> { $+ignoreaccent = eval $<mod_arg>.text } }
#S#      token mod_internal:sym<:0a>   { ':' (\d+) ['a'|'ignoreaccent'] { $+ignoreaccent = $0 } }
#S#  
#S#      token mod_internal:sym<:s>    { ':s' 'igspace'? » { $+sigspace = 1 } }
#S#      token mod_internal:sym<:!s>   { ':!s' 'igspace'? » { $+sigspace = 0 } }
#S#      token mod_internal:sym<:s( )> { ':s' 'igspace'? <mod_arg> { $+sigspace = eval $<mod_arg>.text } }
#S#      token mod_internal:sym<:0s>   { ':' (\d+) 's' 'igspace'? » { $+sigspace = $0 } }
#S#  
#S#      token mod_internal:sym<:r>    { ':r' 'atchet'? » { $+ratchet = 1 } }
#S#      token mod_internal:sym<:!r>   { ':!r' 'atchet'? » { $+ratchet = 0 } }
#S#      token mod_internal:sym<:r( )> { ':r' 'atchet'? » <mod_arg> { $+ratchet = eval $<mod_arg>.text } }
#S#      token mod_internal:sym<:0r>   { ':' (\d+) 'r' 'atchet'? » { $+ratchet = $0 } }
#S#   
#S#      token mod_internal:sym<:Perl5>    { [':Perl5' | ':P5'] [ :lang( $¢.cursor_fresh( ::STD::P5Regex ).unbalanced($+GOAL) ) <nibbler> ] }
#S#  
#S#      token mod_internal:adv {
#S#          <?before ':' <identifier> > [ :lang($¢.cursor_fresh($+LANG)) <quotepair> ] { $/<sym> := «: $<quotepair><key>» }
#S#      }
#S#  
#S#      token mod_internal:oops { ':'\w+ <.panic: "Unrecognized regex modifier"> }
#S#  
#S#      token quantifier:sym<*>  { <sym> <quantmod> }
#S#      token quantifier:sym<+>  { <sym> <quantmod> }
#S#      token quantifier:sym<?>  { <sym> <quantmod> }
#S#      token quantifier:sym<**> { <sym> :: <sigspace>? <quantmod> <sigspace>?
#S#          [
#S#          | \d+ [ '..' [ \d+ | '*' ] ]?
#S#          | <codeblock>
#S#          | <quantified_atom>
#S#          ]
#S#      }
#S#  
#S#      token quantifier:sym<~~> {
#S#          [
#S#          | '!' <sym>
#S#          | <sym>
#S#          ]
#S#          <sigspace> <quantified_atom> }
#S#  
#S#      token quantmod { ':'? [ '?' | '!' | '+' ]? }
#S#  
#S#  } # end grammar
} # end grammar
#S#  
#S#  grammar P5Regex is STD {
grammar P5Regex is STD {
#S#  
#S#      # begin tweaks (DO NOT ERASE)
#S#      multi method tweak (:global(:$g)) { self }
#S#      multi method tweak (:ignorecase(:$i)) { self }
#S#      # end tweaks (DO NOT ERASE)
#S#  
#S#      token category:metachar { <sym> }
#S#      proto token metachar { <...> }
#S#  
#S#      token category:backslash { <sym> }
#S#      proto token backslash { <...> }
#S#  
#S#      token category:assertion { <sym> }
#S#      proto token assertion { <...> }
#S#  
#S#      token category:quantifier { <sym> }
#S#      proto token quantifier { <...> }
#S#  
#S#      token category:mod_internal { <sym> }
#S#      proto token mod_internal { <...> }
#S#  
#S#      proto token rxinfix { <...> }
#S#  
#S#      # suppress fancy end-of-line checking
#S#      token codeblock {
#S#          :my $GOAL is context = '}';
#S#          '{' :: [ :lang($¢.cursor_fresh($+LANG)) <statementlist> ]
#S#          [ '}' || <.panic: "Unable to parse statement list; couldn't find right brace"> ]
#S#      }
#S#  
#S#      rule nibbler {
#S#          :my $ignorecase is context<rw> = $+ignorecase // 0;
#S#          <EXPR>
#S#      }
#S#  
#S#      token termish {
#S#          <.ws>  # XXX assuming old /x here?
#S#          <quantified_atom>+
#S#      }
#S#      token infixish {
#S#          <!infixstopper>
#S#          <!stdstopper>
#S#          <rxinfix>
#S#          {
#S#              $<O> = $<rxinfix><O>;
#S#              $<sym> = $<rxinfix><sym>;
#S#          }
#S#      }
#S#  
#S#      token rxinfix:sym<|> ( --> Junctive_or ) { <sym> }
#S#  
#S#      token quantified_atom {
#S#          <!stopper>
#S#          <!rxinfix>
#S#          <atom>
#S#          [ <.ws> <quantifier>
#S#  #            <?{ $<atom>.max_width }>
#S#  #                || <.panic: "Can't quantify zero-width atom">
#S#          ]?
#S#          <.ws>
#S#      }
#S#  
#S#      token atom {
#S#          [
#S#          | \w
#S#          | <metachar>
#S#          | '\\' :: .
#S#          ]
#S#      }
#S#  
#S#      # sequence stoppers
#S#      token metachar:sym<|>   { '|'  :: <fail> }
#S#      token metachar:sym<)>   { ')'  :: <fail> }
#S#  
#S#      token metachar:quant { <quantifier> <.panic: "quantifier quantifies nothing"> }
#S#  
#S#      # "normal" metachars
#S#  
#S#      token metachar:sym<[ ]> {
#S#          <before '['> <quibble($¢.cursor_fresh( ::STD::Q ).tweak(:q))> # XXX parse as q[] for now
#S#      }
#S#  
#S#      token metachar:sym«(? )» {
#S#          '(?' {} <assertion>
#S#          [ ')' || <.panic: "Perl 5 regex assertion not terminated by parenthesis"> ]
#S#      }
#S#  
#S#      token metachar:sym<( )> {
#S#          '(' {} [:lang(self.unbalanced(')')) <nibbler>]?
#S#          [ ')' || <.panic: "Unable to parse Perl 5 regex; couldn't find right parenthesis"> ]
#S#          { $/<sym> := <( )> }
#S#      }
#S#  
#S#      token metachar:sym<\\> { <sym> <backslash> }
#S#      token metachar:sym<.>  { <sym> }
#S#      token metachar:sym<^>  { <sym> }
#S#      token metachar:sym<$>  {
#S#          '$' <?before \W | $>
#S#      }
#S#  
#S#      token metachar:var {
#S#          <?before <sigil>\w>
#S#          <.panic: "Can't interpolate variable in Perl 5 regex">
#S#      }
#S#  
#S#      token backslash:A { <sym> }
#S#      token backslash:a { <sym> }
#S#      token backslash:b { :i <sym> }
#S#      token backslash:c { :i <sym>
#S#          <[ ?.._ ]> || <.panic: "Unrecognized \\c character">
#S#      }
#S#      token backslash:d { :i <sym> }
#S#      token backslash:e { :i <sym> }
#S#      token backslash:f { :i <sym> }
#S#      token backslash:h { :i <sym> }
#S#      token backslash:l { :i <sym> }
#S#      token backslash:n { :i <sym> }
#S#      token backslash:o { '0' [ <octint> | '{'<octint>[','<octint>]*'}' ]? }
#S#      token backslash:p { :i <sym> '{' <[\w:]>+ '}' }
#S#      token backslash:Q { <sym> }
#S#      token backslash:r { :i <sym> }
#S#      token backslash:s { :i <sym> }
#S#      token backslash:t { :i <sym> }
#S#      token backslash:u { :i <sym> }
#S#      token backslash:v { :i <sym> }
#S#      token backslash:w { :i <sym> }
#S#      token backslash:x { :i <sym> [ <hexint> | '{' [<.ws><hexint><.ws> ] ** ',' '}' ] }
#S#      token backslash:z { :i <sym> }
#S#      token backslash:misc { $<litchar>=(\W) | $<number>=(\d+) }
#S#      token backslash:oops { <.panic: "Unrecognized Perl 5 regex backslash sequence"> }
#S#  
#S#      token assertion:sym<?> { <sym> <codeblock> }
#S#      token assertion:sym<{ }> { <codeblock> }
#S#  
#S#      token assertion:sym«<» { <sym> <?before '=' | '!'> <assertion> }
#S#      token assertion:sym<=> { <sym> [ <?before ')'> | <rx> ] }
#S#      token assertion:sym<!> { <sym> [ <?before ')'> | <rx> ] }
#S#      token assertion:sym«>» { <sym> <rx> }
#S#  
#S#      token rx {
#S#          #[:lang(self.unbalanced(')')) <nibbler>]
#S#          <nibbler>
#S#          [ <?before ')'> || <.panic: "Unable to parse Perl 5 regex; couldn't find right parenthesis"> ]
#S#      }
#S#  
#S#      #token assertion:identifier { <identifier> [               # is qq right here?
#S#      #                                | <?before ')' >
#S#      #                                | <.ws> <nibbler>
#S#      #                               ]
#S#      #                               [ ':' <rx> ]?
#S#      #}
#S#      token p5mod { <[imox]>* }
#S#      token p5mods { <on=p5mod> [ '-' <off=p5mod> ]? }
#S#      token assertion:mod { <p5mods> [               # is qq right here?
#S#                                     | ':' <rx>?
#S#                                     | <?before ')' >
#S#                                     ]
#S#      }
#S#  
#S#      token assertion:bogus { <.panic: "Unrecognized Perl 5 regex assertion"> }
#S#  
#S#      token quantifier:sym<*>  { <sym> <quantmod> }
#S#      token quantifier:sym<+>  { <sym> <quantmod> }
#S#      token quantifier:sym<?>  { <sym> <quantmod> }
#S#      token quantifier:sym<{ }> { '{' \d+ [','\d*]? '}' <quantmod> }
#S#  
#S#      token quantmod { [ '?' | '+' ]? }
#S#  
#S#  } # end grammar
} # end grammar
#S#  
#S#  # The <panic: "message"> rule is called for syntax errors.
#S#  # If there are any <suppose> points, backtrack and retry parse
#S#  # with a different supposition.  If it gets farther than the
#S#  # panic point, print out the supposition ("Looks like you
#S#  # used a Perl5-style shift operator (<<) at line 42.  Maybe
#S#  # you wanted +< or |< instead.")  Or some such...
#S#  # In any event, this is only for better diagnostics, and
#S#  # further compilation is suppressed by the <commit><fail>.
#S#  
#S#  # token panic (Str $s) { <commit> <fail($s)> }
#S#  
#S#  method panic (Str $s) {
#S#      my $m;
#S#      my $here = self;
#S#  
#S#      # Have we backed off recently?
#S#      my $highvalid = self.pos <= $*HIGHWATER;
#S#  
#S#      $here = self.cursor($*HIGHWATER) if $highvalid;
#S#  
#S#      my $first = $here.lineof($COMPILING::LAST_NIBBLE.<firstpos>);
#S#      my $last = $here.lineof($COMPILING::LAST_NIBBLE.<lastpos>);
#S#      if $here.lineof($here.pos) == $last and $first != $last {
#S#          $m ~= "\n(Possible runaway string from line $first)";
#S#      }
#S#      else {
#S#          $first = $here.lineof($COMPILING::LAST_NIBBLE_MULTILINE.<firstpos>);
#S#          $last = $here.lineof($COMPILING::LAST_NIBBLE_MULTILINE.<lastpos>);
#S#          # the bigger the string (in lines), the further back we suspect it
#S#          if $here.lineof($here.pos) - $last < $last - $first  {
#S#              $m ~= "\n(Possible runaway string from line $first to line $last)";
#S#          }
#S#      }
#S#  
#S#      $m ~= "\n" ~ $s;
#S#  
#S#      if $highvalid {
#S#          $m ~= $*HIGHMESS if $*HIGHMESS;
#S#          $*HIGHMESS = $m;
#S#      }
#S#      else {
#S#          # not in backoff, so at "bleeding edge", as it were... therefore probably
#S#          # the exception will be caught and re-panicked later, so remember message
#S#          $*HIGHMESS ~= "\n" ~ $s;
#S#      }
#S#  
#S#      $m ~= $here.locmess;
#S#  
#S#      if $highvalid and %$*HIGHEXPECT {
#S#          my @keys = sort keys %$*HIGHEXPECT;
#S#          if @keys > 1 {
#S#              $m ~= "\n    expecting any of:\n\t" ~ join("\n\t", sort keys %$*HIGHEXPECT);
#S#          }
#S#          else {
#S#              $m ~= "\n    expecting @keys";
#S#          }
#S#      }
#S#      $m ~~ s|Syntax error|Syntax error (two terms in a row?)| if $m ~~ /infix|nofun/;
#S#  
#S#      if @COMPILING::WORRIES {
#S#          $m ~= "\nOther potential difficulties:\n  " ~ join( "\n  ", @COMPILING::WORRIES);
#S#      }
#S#  
#S#      die "############# PARSE FAILED #############" ~ $m ~ "\n";
#S#  }
method panic (Str $s) {
    my $m;
    my $here = self;

    # Have we backed off recently?
    my $highvalid = self.pos <= $*HIGHWATER;

    $here = self.cursor($*HIGHWATER) if $highvalid;

    my $first = $here.lineof($COMPILING::LAST_NIBBLE.<firstpos>);
    my $last = $here.lineof($COMPILING::LAST_NIBBLE.<lastpos>);
    if $here.lineof($here.pos) == $last and $first != $last {
        $m ~= "\n(Possible runaway string from line $first)";
    }
    else {
        $first = $here.lineof($COMPILING::LAST_NIBBLE_MULTILINE.<firstpos>);
        $last = $here.lineof($COMPILING::LAST_NIBBLE_MULTILINE.<lastpos>);
        # the bigger the string (in lines), the further back we suspect it
        if $here.lineof($here.pos) - $last < $last - $first  {
            $m ~= "\n(Possible runaway string from line $first to line $last)";
        }
    }

    $m ~= "\n" ~ $s;

    if $highvalid {
        $m ~= $*HIGHMESS if $*HIGHMESS;
        $*HIGHMESS = $m;
    }
    else {
        # not in backoff, so at "bleeding edge", as it were... therefore probably
        # the exception will be caught and re-panicked later, so remember message
        $*HIGHMESS ~= "\n" ~ $s;
    }

    $m ~= $here.locmess;

    if $highvalid and $*HIGHEXPECT.elems { #XXX  %$*HIGHEXPECT {
        my @keys = sort keys $*HIGHEXPECT; #XXX  %$*HIGHEXPECT;
        if @keys > 1 {
            $m ~= "\n    expecting any of:\n\t" ~ join("\n\t", sort keys $*HIGHEXPECT); #XXX %$*HIGHEXPECT);
        }
        else {
            $m ~= "\n    expecting @keys";
        }
    }

    if @COMPILING::WORRIES {
        $m ~= "\nOther potential difficulties:\n  " ~ join( "\n  ", @COMPILING::WORRIES);
    }

    die "############# PARSE FAILED #############" ~ $m ~ "\n";
}
#S#  
#S#  method worry (Str $s) {
#S#      push @COMPILING::WORRIES, $s ~ self.locmess;
#S#  }
method worry (Str $s) {
    push @COMPILING::WORRIES, $s ~ self.locmess;
}
#S#  
#S#  method locmess () {
#S#      my $pre = substr($+ORIG, 0, self.pos);
#S#      my $line = self.lineof(self.pos);
#S#      $pre = substr($pre, -40, 40);
#S#      1 while $pre ~~ s!.*\n!!;
#S#      my $post = substr($+ORIG, self.pos, 40);
#S#      1 while $post ~~ s!(\n.*)!!;
#S#      " at " ~ $COMPILING::FILE ~ " line $line:\n------> " ~ $Cursor::GREEN ~ $pre ~ $Cursor::RED ~ 
#S#          "$post$Cursor::CLEAR";
#S#  }
method locmess () {
    my $pre = substr($+ORIG, 0, self.pos);
    my $line = self.lineof(self.pos);
    $pre = substr($pre, -40, 40);
    1 while $pre ~~ s!.*\n!!;
    my $post = substr($+ORIG, self.pos, 40);
    1 while $post ~~ s!(\n.*)!!;
    " at " ~ $COMPILING::FILE ~ " line $line:\n------> " ~ $Cursor::GREEN ~ $pre ~ $Cursor::RED ~ 
        "$post$Cursor::CLEAR";
}
#S#  
#S#  method lineof ($p) {
#S#      return 1 unless defined $p;
#S#      my $line = @+MEMOS[$p]<L>;
#S#      return $line if $line;
#S#      $line = 1;
#S#      my $pos = 0;
#S#      my @text = split(/^/,$+ORIG);
#S#      for @text {
#S#          @+MEMOS[$pos++]<L> = $line
#S#              for 1 .. chars($_);
#S#          $line++;
#S#      }
#S#      return @+MEMOS[$p]<L> // 0;
#S#  }
method lineof ($p) {
    return 1 unless defined $p;
    my $line = @+MEMOS[$p]<L>;
    return $line if $line;
    $line = 1;
    my $pos = 0;
    my @text = split(/^/,$+ORIG);
    for @text {
        @+MEMOS[$pos++]<L> = $line
            for 1 .. chars($_);
        $line++;
    }
    return @+MEMOS[$p]<L> // 0;
}
#S#  
#S#  method SETGOAL { }
#S#  method FAILGOAL (Str $stop, Str $name) {
#S#      self.panic("Unable to parse $name; couldn't find final '$stop'");
#S#  }
method SETGOAL { }
method FAILGOAL (Str $stop, Str $name) {
    self.panic("Unable to parse $name; couldn't find final '$stop'");
}
#S#  
#S#  # "when" arg assumes more things will become obsolete after Perl 6 comes out...
#S#  method obs (Str $old, Str $new, Str $when = ' in Perl 6') {
#S#      self.panic("Obsolete use of $old;$when please use $new instead");
#S#  }
method obs (Str $old, Str $new, Str $when = ' in Perl 6') {
    self.panic("Obsolete use of $old;$when please use $new instead");
}
#S#  
#S#  method worryobs (Str $old, Str $new, Str $when = ' in Perl 6') {
#S#      self.worry("Possible obsolete use of $old;$when please use $new instead");
#S#      self;
#S#  }
method worryobs (Str $old, Str $new, Str $when = ' in Perl 6') {
    self.worry("Possible obsolete use of $old;$when please use $new instead");
    self;
}
#S#  
#S#  ## vim: expandtab sw=4 ft=perl6

#X Used for development.
#class STD {
  regex unimplemented_rule { <fail> }
  _inline_p5('
  my %unimp_rules_seen;
  sub AUTOLOAD {
    my $r = $AUTOLOAD;
    print STDERR "FAKING unimplemented rule: $r\n"
      if !$unimp_rules_seen{$r}++;
    return __PACKAGE__->unimplemented_rule("");
  }
');
#}
1; #XXX needed for independent compilation
