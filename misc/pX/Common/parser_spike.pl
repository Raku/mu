require 'regexp_engine_demo.pl';
use Data::Dumper;
use strict;

sub mk_dynamic_alt {
  my($aref)=@_;
  sub{
    my($c)=@_;
    my @a = @$aref;
    while(@a) {
      my $f = shift(@a);
      my $kludge_retval; ($f,$kludge_retval) = @$f if ref($f) eq 'ARRAY';
      my $c_down = $c;
      my($str,$pos,$cap); my $v;
      { local($X::str,$X::pos,$X::cap)=($X::str,$X::pos,$X::cap);
        $v = $f->($c_down);
        ($str,$pos,$cap)=($X::str,$X::pos,$X::cap) if defined $v;
      }
      if(defined $v) {
        ($X::str,$X::pos,$X::cap)=($str,$pos,$cap);
        return $kludge_retval if defined $kludge_retval;
        return $v;
      }
    }
    return undef;
  };
}

{
  package Op;
  my $increment = 1;  
  my %dict;
  sub new {
    my($cls,$name,$prec)=@_;
    $name = "$name";
    $prec = "$prec";
    my $nprec = 1;
    if($prec) {
      $increment = $increment / 2;
      if($prec =~ /is looser\((.*?)\)/) {
        my $op = $dict{$1} or die "Operator '$1' is not defined in $name $prec";
        $nprec = $op->{'nprec'} - $increment;
      }
      elsif($prec =~ /is tighter\((.*?)\)/) {
        my $op = $dict{$1} or die "Operator '$1' is not defined in $name $prec";
        $nprec = $op->{'nprec'} + $increment;
      }
      elsif($prec =~ /is equiv\((.*?)\)/) {
        my $op = $dict{$1} or die "Operator '$1' is not defined in $name $prec";
        $nprec = $op->{'nprec'};
      }
      else { die "Did not understand '$name' '$prec'"; }
    }
    my $o = { name => $name, nprec=>$nprec };
    $dict{$name} = $o;
    bless $o, $cls;
  }
  sub op_cmp {
    my($o,$o1)=@_;
    $o->{'nprec'} <=> $o1->{'nprec'};
  }
  sub make_node_with_args {
    my($o,@args)=@_;
    my $from = $args[0]->from;
    my $to = $args[-1]->to;
    my $str = substr($X::str,$from,$to-$from);
    my $m = MatchX->new->set(1,$str,\@args,{},$from,$to);
    $$m->{'RULE'} = $o->{'name'}; #EEEP
    $m;
  }
}


local %Z::oop;
sub mk_operator_precedence_parser {
  my($ws,$operators,$tokens)=@_;
  my $noop = Hacks->noop;
  my $debug = 0;
  sub{
    my $c = $_[0];
    my $id = rand()."";
    my $mat = $X::current_match;
    my $cap = $X::cap; local $X::cap = [];
    local $Z::oop{$id} = {ops=>[undef],var=>[],op_ahead=>undef};
    my($step);
    $step = sub{
      print STDERR Data::Dumper::Dumper $Z::oop{$id} if $debug;
      my $reduce_helper = sub {
        print STDERR "reduce\n" if $debug;
        my($var,$ops)=@_;
        my @args = splice(@$var,-2,2);
        my $oper = pop(@$ops);
        my $nont = $oper->make_node_with_args(@args);
        push(@$var,$nont);
      };
      my $op = $Z::oop{$id}{'op_ahead'};
      if(defined $op) {
        print STDERR "op $op\n" if $debug;
        my $ops = [@{$Z::oop{$id}{'ops'}}]; local $Z::oop{$id}{'ops'} = $ops;
        my $cmp = (!defined($ops->[-1])) ? -1 : $ops->[-1]->op_cmp($op);
        if($cmp <= 0) {
          print STDERR "op shift\n" if $debug;
          push(@$ops,$op);
          local $Z::oop{$id}{'op_ahead'} = undef;
          return &$step;
        } else {
            print STDERR "op reduce\n" if $debug;
          my $var = [@{$Z::oop{$id}{'var'}}]; local $Z::oop{$id}{'var'} = $var;
          &$reduce_helper($var,$ops);
          return &$step;
        }
      }
      $ws->($noop);
      my $tok = $tokens->($noop);
      if($tok) {
        $tok = pop(@$X::cap);
        my $var = [@{$Z::oop{$id}{'var'}}]; local $Z::oop{$id}{'var'} = $var;
        push(@$var,$tok);
        return &$step;
      }
      my $op1 = $operators->($noop);
      if($op1) {
        print STDERR "new op\n" if $debug;
        local $Z::oop{$id}{'op_ahead'} = $op1;
        #goto &$step;
        return &$step;
      }
      print STDERR "cleanup\n" if $debug;
      my $ops = $Z::oop{$id}{'ops'};
      my $var = $Z::oop{$id}{'var'};
      while(@$ops && @$var > 1) {
        &$reduce_helper($var,$ops);
      }
      print STDERR Data::Dumper::Dumper "cleaned",$Z::oop{$id} if $debug;
      die "bug" if @$var != 1;
      #@_=$noop; goto &$c;
      my $v = $c->($noop);
      if(defined $v) {
        @{$mat} = $var->[0]; #shudder
        return $v;
      }
      die "we're not set up yet for failing back into the oop";
    };
    #goto &$step; # $Z::oop{$id} contents dont make it to $step (v5.8.5)?!?
    return &$step;
  };
}

my @whitespaces;
my $ws = def_rule('ws',mk_dynamic_alt(\@whitespaces));

my @statements;
def_rule('statement',mk_dynamic_alt(\@statements));

my @tokens;
my $tok = def_rule('token',mk_dynamic_alt(\@tokens));
# tokens should be longest-match-first, but that's simple too, and the
# difference won't matter for the spike target.

sub def_thing {
  my($aref,$match_hack,$name,$re)=@_;
  #$match_hack - Match creation is one thing which is very cruft in this spike.
  # 0- dont create one, 1- wrap with push-on-array
  my $r = compile($re) || die;
  $r = mk_Match_appender($r,$name) if $match_hack;
  push(@$aref,$r);
  $r;
}
sub def_token      { def_thing(\@tokens,1,@_) }
sub def_whitespace { def_thing(\@whitespaces,1,@_) }
sub def_statement  { def_thing(\@statements,0,@_) }
sub def_stmt_prim  {
  my($name,$re)=@_;
  my $r = compile($re) || die;
  def_rule($name,$r);
  my $r2 = compile("<$name>") || die;
  my $aref = \@statements;
  push(@$aref,$r2);
  $r;
}

my @operators;
my $ops = mk_dynamic_alt(\@operators);

def_rule('expr',mk_operator_precedence_parser($ws,$ops,$tok));
sub def_operator {
  my($name,$re,$prec)=@_;
  $re =~ s/(\W)/\\$1/g;
  $re = "<ws>?".$re;
  my $op = Op->new($name,$prec);
  my $r = compile($re) || die;
  $r = mk_Match_appender($r,$name);
  push(@operators,[$r,$op]);
  $r;
}

# so Regexp::Parser doesn't have to struggle with the {?{...})
sub hlp_ws {  def_whitespace($X::cap->[-2],"(?x)".$::X::cap->[-1]) }
sub hlp_stmt { def_statement($X::cap->[-2],"<ws>?(?x)".$::X::cap->[-1]) }
sub hlp_token {    def_token($X::cap->[-2],"<ws>?(?x)".$::X::cap->[-1]) }
sub hlp_rule {      def_rule($X::cap->[-2],"<ws>?(?x)".$::X::cap->[-1]) }
sub hlp_oper { def_operator($X::cap->[-3],$X::cap->[-2],$X::cap->[-1]) }

my $x1 = def_stmt_prim('bs_ws_decl','\s*token\s+whitespace:<(\w+)>\s+{([^}]*)}(?{ &::hlp_ws;})');
my $x2 = def_stmt_prim('bs_token_decl','\s*token\s+(\w+)\s+{([^}]*)}(?{ &::hlp_token;})');
my $x3 = def_stmt_prim('bs_rule_decl','\s*rule\s+(\w+)\s+{([^}]*)}(?{ &::hlp_rule;})');
my $x4 = def_stmt_prim('bs_stmt_decl','\s*macro\s+statement_control:<(\w+)>\s+\([^\)]*\)\s+is\s+parsed\(rx:perl5/(.*?)/\)\s+{[^}]*}(?{ &::hlp_stmt;})');
my $x5 = def_stmt_prim('bs_infix_decl','\s*multi\s+(infix:<(.+?)>)\s+\([^\)]*\)\s+((?:is (?:looser|tighter|equiv).*?)?){[^}]*}(?{ &::hlp_oper;})');

my $Perl_prog = def_rule('prog','<statement>*');

my $source = `cat parser_spike_target.p6`;
my $m = match($Perl_prog,$source);

$Data::Dumper::Indent=1;
#print Dumper $m;
print $m->describe,"\n";

__END__
my $m = match($Perl_prog,<<'END'); print Dumper $m; print $m->describe;
token whitespace:<basic_whitespace> { \s+ }

rule identifier { \w+ }
token literal_int { \d+ }

rule simple_call { <identifier><ws><expr>; }
macro statement_control:<a_call> (Match $m --> Match) is parsed(rx:perl5/<simple_call>/) {$m}

multi infix:<*> ($a,$b) {...}
multi infix:<+> ($a,$b) is looser(infix:<*>) {...}

say 3;
END
