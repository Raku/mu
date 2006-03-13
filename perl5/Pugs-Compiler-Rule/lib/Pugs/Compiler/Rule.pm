use v5;
package Pugs::Compiler::Rule;

use Pugs::Runtime::Rule;
use Pugs::Runtime::Rule2;
use Pugs::Grammar::Rule;
use Pugs::Emitter::Rule::Perl5;
use Pugs::Runtime::Match;

# the compiler is syntax sugar for
# eval( emit::rule::perl5( parse::rule( $rule ) ) )

=pod

 *xxx = Pugs::Compiler::Rule->compile( '...' )->code;
 my $match = xxx( 'abc' );

 my $rule = Pugs::Compiler::Rule->compile( '...' );
 my $match = $rule->match( 'abc' );

=cut

use strict;
use warnings;

sub new { $_[0] }

sub compile {
    my ($class, $rule_source) = @_;
    my $self = { source => $rule_source };
    $self->{ast} = Pugs::Grammar::Rule::rule( 
        $self->{source} );
    $self->{perl5} = Pugs::Emitter::Rule::Perl5::emit( 
        $self->{ast}{capture} );

    local $@;
    $self->{code} = eval 
        $self->{perl5};
    die "Error in evaluation: $@\nSource:\n$self->{perl5}\n" if $@;

    bless $self, $class;
}

sub code { 
    my $rule = shift; 
    sub { $rule->match( @_ ); } 
}

sub match {
    my $match = $_[0]->{code}( $_[1] );
    return Match->new( $match ) if defined $match;
    return Match->new( { bool => 0 } );   # XXX - fix?
}

1;
