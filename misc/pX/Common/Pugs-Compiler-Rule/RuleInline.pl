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
my $rpcr = Pugs::Compiler::Rule->compile('[a|b](b)');
my $rpcr2 = Pugs::Compiler::Rule->compile('[a|b](b)', { ratchet => 1 } );
print $rpcr2->perl5;
print "Benchmark:\n";
Benchmark::cmpthese(500, {
    PCR_x1         => sub{ $rpcr->match('abc')  for 1..40},
    PCR_ratchet_x1 => sub{ $rpcr2->match('abc') for 1..40},
    fast_x10       => sub{ $x->('abc')          for 1..400},
    P5regex_x100   => sub{ 'abc' =~ /[a|b](b)/o for 1..4000},
});

}
