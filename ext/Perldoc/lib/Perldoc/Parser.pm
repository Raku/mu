package Perldoc::Parser;
use strict;
use warnings;
use Perldoc::Base;
use base 'Perldoc::Base';

field 'input';
field 'loader';

our @buffer;
our $line_number;

sub parse {
    my $self = shift;
    no warnings 'once';

    local @buffer;
    local $line_number = 0;
    local *read = $self->read_sub;

    my $result = $self->do_parse;
}

sub do_parse {
    my $self = shift;
    $self->loader->begin({type => 'stream'});
    while (my $block = $self->next_block) {
        $self->loader->begin($block);
        $self->reparse($block);
        $self->loader->end($block);
    }
    $self->loader->end({type => 'stream'});
    return $self->finish;
}

sub reparse {
    my $self = shift;
    my $chunk = shift;
    my $type = $chunk->{type};
    my $class = "Perldoc::Parser::$type";
    my $parser = $class->new(
        input => \$chunk->{content},
        loader => $self->loader,
    );
    $parser->parse;
}

sub contains_blocks {
    qw( heading verbatim paragraph )
}

sub next_block {
    my $self = shift;
    $self->throwaway
      or return;
    for my $type ($self->contains_blocks) {
        my $method = "get_$type";
        my $block = $self->$method;
        next unless defined $block;
        $block = { content => $block }
          unless ref $block;
        $block->{type} ||= $type;
        return $block;
    }
    return;
}

sub throwaway {
    my $self = shift;
    while (my $line = $self->read) {
        next if
          $self->comment_line($line) or
          $self->blank_line($line);
        $self->unread($line);
        return 1;
    }
    return;
}

sub read_paragraph {
    my $self = shift;
    my $paragraph = '';
    while (my $line = $self->read) {
        last if $self->blank_line($line);
        $paragraph .= $line;
    }
    return $paragraph;
}

sub comment_line { (pop) =~ /^#\s/ }
sub blank_line { (pop) =~ /^\s*$/ }
sub line_matches {
    my $self = shift;
    my $regexp = shift;
    my $line = $self->read;
    $self->unread($line);
    $line =~ $regexp;
}

# Methods to parse out top level blocks
sub get_heading {
    my $self = shift;
    return unless $self->line_matches(qr/^={1,4} \S/);
    my $heading = $self->read_paragraph;
    $heading =~ s/\s*\n\s*(?=.)/ /g;
    chomp $heading;
    $heading =~ s/^(=+)\s+// or die;
    my $level = length($1);
    return +{
        content => $heading,
        level => $level,
    };
}

sub get_verbatim { 
    my $self = shift;
    my $verbatim = '';
    my $prev_blank = 0;
    while (my $line = $self->read) {
        if ($line =~ /^\S/) {
            if ($prev_blank) {
                $self->unread($line);
                last;
            }
            $self->unread($verbatim, $line);
            return;
        }
        next if $self->comment_line($line);
        $verbatim .= $line;
        $prev_blank = $self->blank_line($line);
    }
    return unless $verbatim;
    until ($verbatim =~ /^\S/) {
        $verbatim =~ s/^ //gm;
    }
    return $verbatim;
}

sub get_paragraph {
    my $self = shift;
    my $paragraph = $self->read 
      or return;
    while (my $line = $self->read) {
        next if $self->comment_line($line);
        last if $self->blank_line($line);
        $paragraph .= $line;
    }
    $paragraph =~ s/\s*\n(?=.)/ /g;
    return $paragraph;
}

# Methods to handle reading and buffering input
sub read_sub {
    my $self = shift;
    my $input = $self->input
      or die "Perldoc::Parser->input not defined";
    no warnings;
    if (not ref $input) {
        open my $handle, '<', $input
          or die "Can't open $input for input:\n$!";
        $self->input($handle);
        return \&read_from_handle;
    }
    elsif (ref($input) eq 'SCALAR') {
        return \&read_from_string;
    }
    else {
        return \&read_from_handle;
    }
}

sub finish {
    my $self = shift;
    my $input = $self->input;
    if (ref($input) ne 'SCALAR') {
        close $input;
    }
    $self->input(undef);
    return $self->loader->result;
}

sub unread {
    my $self = shift;
    for my $lines (@_) {
        my @lines = ($lines =~ /(.*\n)/g);
        unshift @buffer, @lines;
    }
}

sub readn {
    my $self = shift;
    my $number = shift;
    my $lines = '';
    while ($number) {
        my $line = $self->read or last;
        $lines .= $line;
    }
    return $lines
      if length($lines);
    return;
}

sub read_from_string {
    my $self = shift;
    if (@buffer) {
        $line_number++;
        return shift @buffer;
    }
    my $string = $self->input;
    if (length $$string) {
        $$string =~ s/(.*(\n|$))// or die;
        my $line = $1;
        $line_number++;
        return $line;
    }
    return;
}

sub read_from_handle {
    my $self = shift;
    if (@buffer) {
        $line_number++;
        return shift @buffer;
    }
    my $handle = $self->input;
    local $/ = "\n";
    my $line = <$handle>;
    if (defined $line) {
        $line_number++;
        return $line;
    }
    return;
}

package Perldoc::Parser::Unit;
our @ISA = qw(Perldoc::Parser);

sub do_parse {
    my $self = shift;
    $self->loader->content(${$self->input});
}

sub reparse {
    die;
}

package Perldoc::Parser::heading;
use base 'Perldoc::Parser::Unit';

package Perldoc::Parser::verbatim;
use base 'Perldoc::Parser::Unit';

package Perldoc::Parser::paragraph;
use base 'Perldoc::Parser::Unit';

1;
