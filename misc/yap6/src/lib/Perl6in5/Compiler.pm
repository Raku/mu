package Perl6in5::Compiler;

use Sub::Exporter;

sub new {
    my $proto = shift;
    my $class = ref $proto || $proto;
    my $self = {};
    bless $self, $class;
    return $self;
}

