package Kwid::Loader;
use strict;
use warnings;
use Kwid::Base;
use base 'Kwid::Base';

field 'output';
field 'complete' => 0;
field 'result';

sub init {
    my $self = shift;
    my $output = $self->output
      or die "Kwid::Loader->output not defined";
    no warnings;
    if (not ref $output) {
        open my $handle, '>', $output
          or die "Can't open $output for output:\n$!";
        $self->output($handle);
        *print = \&print_to_handle;
    }
    elsif (ref($output) eq 'SCALAR') {
        *print = \&print_to_string;
    }
    else {
        *print = \&print_to_handle;
    }
}

sub finish {
    my $self = shift;
    my $output = $self->output;
    if (ref($output) eq 'SCALAR') {
        $self->result($$output);
    }
    else {
        $self->result(1);
        close $output;
    }
    $self->output(undef);
}

sub print_to_string {
    my $self = shift;
    my $string = $self->output;
    $$string .= $_ for @_;
}

sub print_to_handle {
    my $self = shift;
    my $handle = $self->output;
    print $handle @_;
}

1;
