package Perldoc::Writer;
use Perldoc::Base -Base;

field 'output';
field 'result';
field 'write_func';

sub init {
    my $output = $self->output || do {my $x = ''; \$x};
    my $write_func;
    if (not ref $output) {
        open my $handle, '>', $output
          or die "Can't open $output for output:\n$!";
        $self->output($handle);
        $write_func = \&write_to_handle;
    }
    elsif (ref($output) eq 'SCALAR') {
        $write_func = \&write_to_string;
    }
    else {
        $write_func = \&write_to_handle;
    }
    $self->write_func($write_func);
    $self->output($output);
}

sub cleanup {
    my $output = $self->output
      or return;
    if (ref($output) eq 'SCALAR') {
        $self->result($$output);
    }
    else {
        $self->result(1);
        close $output;
    }
    $self->output(undef);
}

sub write {
    $self->write_func->(@_);
}

sub write_to_string {
    my $string = $self->output;
    $$string .= $_ for @_;
}

sub write_to_handle {
    my $handle = $self->output;
    print $handle @_;
}
