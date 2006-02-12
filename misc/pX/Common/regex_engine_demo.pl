# This is a regexp engine, written in p5.  It the very beginnings of a
# proof of concept.  A feasibility and performance reality check.
# Hopefully it will develop into a p6 rules engine, and then a p6
# parser, for p5.

# It's a backtracking, recursive-decent engine, generated from the
# regexp ast nodes created by Regexp::Parser.

# We're using the normal Regexp::Parser nodes, rather than it's p6
# ones, because the latter currently lack tests, so I am uncertain of
# their status.  The nodes are basically the same, so it's not a big
# deal.

# Currently "implemented", and perhaps even working, are literals "a",
# alternation "|", concatentation "ab", and some anchors "\A\z^$".

# This is day, almost hour, one.

#XXX - feel free to flesh things out.
# Most of it is simple (concat is anomalous).

#XXX:
# - write an accompanying .t file to dump tests into.
# - play with Regexp::Parser's Perl6::Rules to establish it's state.
#   Summarize that here.
# - Create a subrule node for it, and teach it to parse it.

#XXX - Set up a full p5 re_test suite.  Either using our own
#t/rules/perl5_N.t tests, or the re_test file (originally from the p5
#distribution) and accompanying driver from the parrot distribution.
#Our own might be best, so we can use same for mechanism for the p6
#tests. The suite could also be a nice mechanism to benchmark with.

use Regexp::Parser;
use Data::Dumper;
use strict;
{
  package Hacks;
  my $noop;
  $noop = sub{return 1 if $_[0] eq $noop; goto $_[0]($noop);};
  sub noop {
    $noop;
  }
  sub concat {
    my($o,$aref)=@_;
    die "bug $aref" if ref($aref) ne 'ARRAY';
    my @a = @$aref;
    return $o->noop if @a == 0;
    return $a[0]->emit if @a == 1;
    my @fs = map { $_->emit } @a;
    my $code1 = ""; my $code2 = "";
    my $code0 = "my \$f0 = \$fs[0]; ";
    for my $i (reverse(1..$#a)) {
      $code0 .= "my \$f$i = \$fs[$i]; ";
      $code1 .= "sub{\@_=";
      $code2 .= ";goto \&\$f$i}";
    }
    my $code = $code0."\n sub{my \$cn = \$_[0]; \@_=".$code1."\$cn".$code2.";goto \&\$f0}\n";
    #print $code;
    eval($code) || die "$@";
  }   
}
{
  package Regexp::Parser::__object__;
  @Regexp::Parser::__object__::ISA=qw(Hacks);
}  
{
  # \A ^ \B \b \G \Z \z $
  package Regexp::Parser::anchor;
  sub emit {
    my($o)=@_;
    my $noop = $o->noop;
    my $re = $o->raw();
    return sub{return undef if !($X::pos == 0);
               my $c = $_[0]; @_=$noop; goto &$c
    } if $re eq '\A';
    return sub{return undef if !($X::pos == 0 || substr($X::str,$X::pos-1,1) eq "\n");
               my $c = $_[0]; @_=$noop; goto &$c
    } if $re eq '^';
    # \B
    # \b
    # \G
    return sub{return undef if !($X::pos == (length($X::str)-1) || substr($X::str,$X::pos,1) eq "\n");
               my $c = $_[0]; @_=$noop; goto &$c
    } if $re eq '$';
    # \Z
    return sub{return undef if !($X::pos == (length($X::str)-1));
               my $c = $_[0]; @_=$noop; goto &$c
    } if $re eq '\z';
    die "didn't implement $re"
  }
}
{
  # . \C
  package Regexp::Parser::reg_any;
}
{
  # \w \W
  package Regexp::Parser::alnum;
}
{
  # \s \S
  package Regexp::Parser::space;
}
{
  # \d \D
  package Regexp::Parser::digit;
}
{
  package Regexp::Parser::anyof;
}
{
  package Regexp::Parser::anyof_char;
}
{
  package Regexp::Parser::anyof_range;
}
{
  package Regexp::Parser::anyof_class;
}
{
  package Regexp::Parser::anyof_close;
}
{
  package Regexp::Parser::prop;
}
{
  package Regexp::Parser::clump;
}
{
  # |
  package Regexp::Parser::branch;
  sub emit {
    my($o)=@_;
    my($f0,$f1) = map { $o->concat($_) } @{$o->data};
    sub{
      my($str,$pos); my $v;
      { local($X::str,$X::pos)=($X::str,$X::pos);
        $v = $f0->($_[0]);
        ($str,$pos)=($X::str,$X::pos) if defined $v;
      }
      if(defined $v) {
        ($X::str,$X::pos)=($str,$pos);
        return $v;
      }
      goto &$f1};
  }
}
{
  package Regexp::Parser::exact;
  sub emit{
    my($o)=@_;
    my $noop = $o->noop;
    my $s = join("",$o->data);
    my $len = length($s);
    sub{ my $c = $_[0]; return undef if !(substr($X::str,$X::pos,$len) eq $s); $X::pos += $len; @_=$noop; goto &$c};
  }
}
{
  package Regexp::Parser::quant;
}
{
  # ( non-capturing
  package Regexp::Parser::group;
}
{
  # ( capturing
  package Regexp::Parser::open;
}
{
  # ) closing
  package Regexp::Parser::close;
}
{
  # ) for non-captures
  package Regexp::Parser::tail;
}
{
  # \1 (backrefs)
  package Regexp::Parser::ref;
}
{
  package Regexp::Parser::assertion;
}
{
  # (?=) (?<=)
  package Regexp::Parser::ifmatch;
}
{
  # (?!) (?<!)
  package Regexp::Parser::unlessm;
}
{
  # (?>)
  package Regexp::Parser::suspend;
}
{
  # (?(n)t|f)
  package Regexp::Parser::ifthen;
}
{
  # the N in (?(N)t|f) when N is a number
  package Regexp::Parser::groupp;
}
{
  # (?{ ... })
  package Regexp::Parser::eval;
}
{
  # (??{ ... })
  package Regexp::Parser::logical;
}
{
  package Regexp::Parser::flags;
}
{
  package Regexp::Parser::minmod;
}

my $noop = Hacks->noop;
sub compile {
  my($re)=@_;
  my $parser = Regexp::Parser->new($re);
  my $n = $parser->root;
  #print Dumper $n;
  my $r = Hacks->concat($n);
  return $r;
}
sub match {
  my($r,$s)=@_;
  local $X::str = $s;
  local $X::pos = 0;
  my $m = $r->($noop);
  return $m;
}
sub match_re {
  my($re,$s)=@_;
  my $r = compile($re);
  my $m = match($r,$s);
  return $m;
}

print Dumper match_re('\z','abc');
print Dumper match_re('\z\z\z\z','abc');
print Dumper match_re('\z|\A','abc');
print Dumper match_re('\A|\z','abc');
print Dumper match_re('ab','abc');
print Dumper match_re('x','abc');
print Dumper match_re('x|a','abc');
