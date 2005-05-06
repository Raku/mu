
use v6;
module Pod::Event::Handler::POD-0.0.1;

multi sub pod2pod (Str $buffer is rw) returns Hash is export {
    my %events = (
        # Documents
        start_document => { $buffer ~= "=pod\n\n" },
        end_document   => { $buffer ~= "=cut"     },

        # Elements
        start_element => -> ($event_type, @args) { 
            given $event_type {
                when 'header' {
                    my $size = @args.shift;
                    $buffer ~= "=head$size ";          
                }
                when 'list' {
                    my $indent = @args.shift;
                    $buffer ~= "=over $indent\n\n";
                }
                when 'item' {
                    $buffer ~= "=item ";
                }
            }
        },
        end_element => -> ($event_type, @args) { 
            given $event_type {
                when 'list' {
                    $buffer ~= "=back\n\n"
                }  
                when rx:perl5/^header|item|paragraph|line_interpolation$/ {
                    $buffer ~= "\n" 
                }              
            }
        },          
  
        # Modifiers
        start_modifier => -> ($mod) { $buffer ~= $mod ~ "<" },
        end_modifier   => -> ($mod) { $buffer ~= ">" },      

        # Text handling
        verbatim => -> ($text) { 
            my @lines = split("\n", $text); 
            for (@lines) -> $line {
                if ($line eq '') {
                    $buffer ~= "\n";
                }
                else {
                    $buffer ~= " $line\n";                                            
                }

            }
        },
        string => -> ($str) { $buffer ~= $str }
    );
    return %events;
}

multi sub pod2pod (IO $fh) returns Hash is export {
    my %events = (
        # Documents
        start_document => { $fh.print("=pod\n\n") },
        end_document   => { $fh.print("=cut")     },

        # Elements
        start_element => -> ($event_type, @args) { 
            given $event_type {
                when 'header' {
                    my $size = @args.shift;
                    $fh.print("=head$size ");          
                }
                when 'list' {
                    my $indent = @args.shift;
                    $fh.print("=over $indent\n\n");
                }
                when 'item' {
                    $fh.print("=item ");
                }
            }
        },
        end_element => -> ($event_type, @args) { 
            given $event_type {
                when 'list' {
                    $fh.print("=back\n\n")
                }  
                when rx:perl5/^header|item|paragraph|line_interpolation$/ {
                    $fh.print("\n") 
                }              
            }
        },          
  
        # Modifiers
        start_modifier => -> ($mod) { $fh.print($mod ~ "<") },
        end_modifier   => -> ($mod) { $fh.print(">") },      

        # Text handling
        verbatim => -> ($text) { 
            my @lines = split("\n", $text); 
            for (@lines) -> $line {
                if ($line eq '') {
                    $fh.print("\n");
                }
                else {
                    $fh.print(" $line\n");                                            
                }

            }
        },
        string => -> ($str) { $fh.print($str) }
    );
    return %events;
}

=pod

=head1 NAME

Pod::Event::Handler::POD - A collection of POD event handlers for Pod::Event::Parser

=head1 SYNOPSIS

  use v6;
  use Pod::Event::Parser;
  use Pod::Event::Handler::POD;
  
  parse("path/to/file.pod", pod2pod($str_buffer));
  
  parse("path/to/file.pod", pod2pod($fh));  

=head1 DESCRIPTION

This is a collection of event handlers for use with L<Pod::Event::Parser> which will
perform POD roundtripping.

=head1 FUNCTIONS

=over 4

=item B<pod2pod (Str $buffer is rw)>
=item B<pod2pod (IO $fh)>

Returns a hash of event handlers for L<Pod::Event::Parser>. 

You can either pass in a string C<$buffer> which will get the POD appended to it, 
or you can pass in a filehandle C<$fh> which the POD will be printed too.

=back

=head1 SEE ALSO

=over 4

=item L<Pod::Event::Parser>

=back

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
