
use v6;
module Pod::Event::Handler::HTML-0.0.1;

multi sub pod2html (Str $buffer is rw) returns Hash is export {
    my %events = ();
    return %events;
}

multi sub pod2html (IO $fh) returns Hash is export {
    my %events = ();
    return %events;
}

=pod

=head1 NAME

Pod::Event::Handler::HTML - A collection of HTML event handlers for Pod::Event::Parser

=head1 SYNOPSIS

  use v6;
  use Pod::Event::Parser;
  use Pod::Event::Handler::HTML;
  
  parse("path/to/file.pod", pod2html($str_buffer));
  
  parse("path/to/file.pod", pod2html($fh));  

=head1 DESCRIPTION

This is a collection of event handlers for use with L<Pod::Event::Parser> which will
perform HTML formatting.

=head1 FUNCTIONS

=over 4

=item B<pod2html (Str $buffer is rw)>
=item B<pod2html (IO $fh)>

Returns a hash of event handlers for L<Pod::Event::Parser>. 

You can either pass in a string C<$buffer> which will get the HTML appended to it, 
or you can pass in a filehandle C<$fh> which the HTML will be printed too.

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
