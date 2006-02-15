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
      my $c_down = $c;
      my($str,$pos,$cap); my $v;
      { local($X::str,$X::pos,$X::cap)=($X::str,$X::pos,$X::cap);
        $v = $f->($c_down);
        ($str,$pos,$cap)=($X::str,$X::pos,$X::cap) if defined $v;
      }
      if(defined $v) {
        ($X::str,$X::pos,$X::cap)=($str,$pos,$cap);
        return $v;
      }
    }
    return undef;
  };
}

my @whitespaces;
def_rule('ws',mk_dynamic_alt(\@whitespaces));

my @statements;
def_rule('statement',mk_dynamic_alt(\@statements));

my @tokens;
def_rule('token',mk_dynamic_alt(\@tokens));
# tokens should be longest-match-first, but that's simple too, and the
# difference won't matter for the spike target.

sub def_thing {
  my($aref,$name,$re)=@_;
  my $r = compile($re) || die;
  push(@$aref,$r);
  $r;
}
sub def_whitespace { def_thing(\@whitespaces,@_) }
sub def_statement  { def_thing(\@statements,@_) }
sub def_token      { def_thing(\@tokens,@_) }

# This <expr> is a crock.  Should be a operator precedence parser.
def_rule('expr',mk_dynamic_alt(\@tokens));
sub def_operator { def_thing(\@tokens,@_) }

# so Regexp::Parser doesn't have to struggle with the {?{...})
sub setRULE { '(?{$$X::current_match->{"RULE"}="'.$X::cap->[-2].'"})' }
sub hlp_ws {  def_whitespace($X::cap->[-2],"(?x)".$::X::cap->[-1].setRULE) }
sub hlp_stmt { def_statement($X::cap->[-2],"<ws>?(?x)".$::X::cap->[-1].setRULE) }
sub hlp_token {    def_token($X::cap->[-2],"<ws>?(?x)".$::X::cap->[-1].setRULE) }
sub hlp_rule {      def_rule($X::cap->[-2],"<ws>?(?x)".$::X::cap->[-1].setRULE) }
sub hlp_oper { def_operator($X::cap->[-2],"<ws>?(?x)"."\\Q".$::X::cap->[-1].setRULE) }

my $x1 = def_statement('bs_ws_decl','\s*token\s+whitespace:<(\w+)>\s+{([^}]*)}(?{ &::hlp_ws;})');
my $x2 = def_statement('bs_token_decl','\s*token\s+(\w+)\s+{([^}]*)}(?{ &::hlp_token;})');
my $x3 = def_statement('bs_rule_decl','\s*rule\s+(\w+)\s+{([^}]*)}(?{ &::hlp_rule;})');
my $x4 = def_statement('bs_stmt_decl','\s*macro\s+statement_control:<(\w+)>\s+\([^\)]*\)\s+is\s+parsed\(rx:perl5/(.*?)/\)\s+{[^}]*}(?{ &::hlp_stmt;})');
my $x5 = def_statement('bs_infix_decl','\s*multi\s+(infix:<(.+?)>)\s+\([^\)]*\)\s+(?:is looser.*?)?{[^}]*}(?{ &::hlp_oper;})');

my $Perl_prog = def_rule('prog','<statement>*');

my $source = `cat regexp_spike_target.p6`;
my $m = match($Perl_prog,$source);

$Data::Dumper::Indent=1;
print Dumper $m;
print $m->describe;

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
