package Perldoc::Base;
use Spiffy 0.22 -Base;
use Spiffy -XXX;
use Carp qw(croak);
our @EXPORT = qw(croak);

sub new() {
    my $class = shift;
    my %args = @_;
    my $self = bless {}, $class;
    for my $method (keys %args) {
        $self->$method($args{$method})
          if $self->can($method);
    }
    $self->init;
    return $self;
}

sub DESTROY {
    $self->cleanup;
}

sub init {}

sub cleanup {}
