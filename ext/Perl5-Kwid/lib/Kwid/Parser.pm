package Kwid::Parser;
use strict;
use warnings;
use Kwid::Base;
use base 'Kwid::Base';

field 'input';
field 'loader';

our $buffer;

sub parse {
    local $buffer;
    my $self = shift;
    $self->init;
    while (my $para = $self->next_top_para) {
        $self->loader->start_para;
        $self->parse_para($para);
        $self->loader->end_para;
    }
    $self->finish;
    return $self->loader->result;
}

sub next_top_para {
    my $self = shift;
    while ($self->read) {
        next if $self->blank_line;
        next if $self->comment_line;
        return $self->get_normal_para;
    } continue { undef $buffer }
    return;
}

sub comment_line { $buffer =~ /^#\s/ }
sub blank_line { $buffer =~ /^\s*$/ }

sub get_normal_para {
    my $self = shift;
    my $para = '';
    while ($self->read) {
        last if $self->blank_line;
        next if $self->comment_line;
        $para .= $buffer;
    } continue { undef $buffer }
    $para =~ s/\n(?=.)/ /g;
    return $para;
}

sub parse_para {
    my $self = shift;
    $self->loader->content(shift);
}

sub init {
    my $self = shift;
    my $input = $self->input
      or die "Kwid::Parser->input not defined";
    no warnings;
    if (not ref $input) {
        open my $handle, '<', $input
          or die "Can't open $input for input:\n$!";
        $self->input($handle);
        *read = \&read_from_handle;
    }
    elsif (ref($input) eq 'SCALAR') {
        *read = \&read_from_string;
    }
    else {
        *read = \&read_from_handle;
    }
    $self->loader->start_stream;
}

sub finish {
    my $self = shift;
    $self->loader->end_stream;
    my $input = $self->input;
    if (ref($input) ne 'SCALAR') {
        close $input;
    }
    $self->input(undef);
}

sub read_from_string {
    my $self = shift;
    return 1 if defined $buffer;
    my $string = $self->input;
    if (length $$string) {
        $$string =~ s/(.*(\n|$))// or die;
        $buffer = $1;
        return 1;
    }
    return;
}

sub read_from_handle {
    my $self = shift;
    return 1 if defined $buffer;
    my $handle = $self->input;
    my $line = <$handle>;
    return unless defined $line;
    $buffer = $line;
    return 1;
}

1;
