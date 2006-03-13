use v5;
package Pugs::Compiler::Rule;

use Pugs::Runtime::Rule;
use Pugs::Runtime::Rule2;
use Pugs::Grammar::Rule;
use Pugs::Emitter::Rule::Perl5;
use Pugs::Runtime::Match;

# the compiler is syntax sugar for
# eval( emit::rule::perl5( parse::rule( $rule ) ) )

use strict;
use warnings;

sub new { $_[0] }

sub compile {
    my ($class, $rule_source) = @_;
    my $self = { source => $rule_source };
    $self->{ast} = Pugs::Grammar::Rule::rule( 
        $self->{source} );
    $self->{perl5} = Pugs::Emitter::Rule::Perl5::emit( 
        $self->{ast} );

    local $@;
    $self->{code} = eval 
        $self->{perl5};
    die "Error in evaluation: $@\nSource:\n$self->{perl5}\n" if $@;

    bless $self, $class;
}

sub match {
    $_[0]->{code}( $_[1] );
}

1;
