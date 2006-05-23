package Pugs::Runtime::RuleInline;

# - fglock
#

# this is a program prototype
# see Pugs::Emitter::Rule::Perl5::Ratchet for the final implementation

use strict;
use warnings;
use Data::Dumper;
use PadWalker qw( peek_my );  # peek_our ); ???

sub alternation {
    return '( ' . join( ' || ', @_ ) . ' )';
}

sub concat {    
    return '( ' . join( ' && ', @_ ) . ' )';
}

sub constant { 
    my $const = shift;
    return "( ( \$s =~ m/^(\Q$const\E)(.*)/s ) ? do{ \$s = \$2; push \@match, \$1 } : 0 )";
}

sub capture { 
    return "do{ 
        push \@match, { 
            name => '$_[0]', 
            match => do{ my \@match; $_[1]; \\\@match },
        } }";
}

sub null {
    "1";
};

sub optional {
    alternation( @_, 1 );
}

sub wrap {
    return "sub { my \@match; my \$s = shift; $_[0]; return \$s, \\\@match; }";
}

{
my $r = capture( 'cap', constant('a') );
print "Perl5 source:\n", wrap( $r ), "\n";
my $x = eval wrap( $r );
print "Tail, Match:\n", Dumper( $x->("abc") );
}

{
my $r = 
    concat( 
        alternation( constant('a'), constant('b') ),
        capture( '0', constant('b') ),
    );
print "Perl5 source:\n", wrap( $r ), "\n";
my $x = eval wrap( $r );
print "Tail, Match:\n", Dumper( $x->("abc") );

use Benchmark;
use Pugs::Compiler::Rule;
use Pugs::Compiler::Token;
use Pugs::Compiler::Regex;
my $rpc_rule = Pugs::Compiler::Rule->compile('[a|b](b)');
my $rpc_token = Pugs::Compiler::Token->compile('[a|b](b)' );
my $rpc_regex = Pugs::Compiler::Regex->compile('[a|b](b)' );
print $rpc_token->perl5, "\n";

{
    print "P5 regex benchmark:\n";
    my $s = 'abcdefg';
    Benchmark::cmpthese(500, {
        substr         => sub{ substr($s,3,1) eq 'd' for 1..1000},
        regex          => sub{ $s =~ /^.{3}d/os      for 1..1000},
    });
}

{
    print "Benchmark:\n";
    Benchmark::cmpthese(500, {
        PCR_Rule_x1         => sub{ $rpc_rule->match('abc')  for 1..40},
        PCR_Token_x1         => sub{ $rpc_token->match('abc')  for 1..40},
        PCR_Regex_x1         => sub{ $rpc_regex->match('abc')  for 1..40},
        fast_x10       => sub{ $x->('abc')          for 1..400},
        P5regex_x100   => sub{ 'abc' =~ /[a|b](b)/o for 1..4000},
    });
}

}
