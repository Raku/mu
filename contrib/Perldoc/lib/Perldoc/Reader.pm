package Perldoc::Reader;
use Perldoc::Base -Base;

field 'input';
field 'type';
field 'read_func';
field '_buffer';
field '_eof';

sub init {
    my $input = $self->input
      or die "Can't create a reader object without input";
    my ($read_func, $buffer, $eof);
    if (not ref $input) {
        if ($input = /\n/) {
            $read_func = \&read_from_string;
            $buffer = do {my $x = $input; \ $x};
            $eof = 1;
        }
        else {
            open my $handle, '<', $input
              or die "Can't open $input for input:\n$!";
            $self->input($handle);
            $buffer = do {my $x = ''; \$x};
            $read_func = \&read_from_handle;
            $eof = 0;
        }
    }
    elsif (ref($input) eq 'SCALAR') {
        $buffer = $input;
        $read_func = \&read_from_string;
        $eof = 1;
    }
    else {
        $buffer = do {my $x = ''; \$x};
        $read_func = \&read_from_handle;
        $eof = 0;
    }
    $self->read_func($read_func);
    $self->_normalize($buffer);
    $self->_buffer($buffer);
    $self->_eof($eof);
}

# Perldoc streams should have normalized line endings and end with a newline
sub _normalize {
    my $buffer = shift;
    return unless length $$buffer;
    $$buffer =~ s/\r\n/\n/g;
    $$buffer =~ s/\r/\n/g;
    $$buffer .= "\n"
      unless $$buffer =~ /\n\z/;
}

sub finish {
    my $input = $self->input;
    if (ref($input) ne 'SCALAR') {
        close $input;
    }
    $self->input(undef);
    return $self->loader->result;
}

# Return the buffer reference while asserting the at least one paragraph is
# present and that the buffer ends with a newline.
sub buffer {
    my $buffer = $self->_buffer;
    $self->read
      unless $$buffer =~ /\S.*?\n\s*\n/o or
             $self->_eof;
    return $buffer;
}

sub read {
    $self->normalize;
    # XXX Finish this   
}

sub unread {
    my $buffer = $self->_buffer;
    $$buffer = join '', @_, $$buffer;
}

sub eos {
    my $buffer = $self->_buffer;
    $self->_eof and not length $$buffer;
}

# sub read {
#     my $number = shift;
#     my $lines = '';
#     while ($number) {
#         my $line = $self->read or last;
#         $lines .= $line;
#     }
#     return $lines
#       if length($lines);
#     return;
# }
# 
# sub read_from_string {
#     if (@buffer) {
#         $line_number++;
#         return shift @buffer;
#     }
#     my $string = $self->input;
#     if (length $$string) {
#         $$string =~ s/(.*(\n|$))// or die;
#         my $line = $1;
#         $line_number++;
#         return $line;
#     }
#     return;
# }
# 
# sub read_from_handle {
#     if (@buffer) {
#         $line_number++;
#         return shift @buffer;
#     }
#     my $handle = $self->input;
#     local $/ = "\n";
#     my $line = <$handle>;
#     if (defined $line) {
#         $line_number++;
#         return $line;
#     }
#     return;
# }

