package Foo;

use strict;
use warnings;
use feature 'say';
use STD;

sub new {
    my ($class,%options) = @_;
    return bless \%options, $class;
}

sub parse {
    my $self = shift;
    say "Parse " . $self->{text};    
    my $parser = STD->parse($self->{text});
}

1;
