module CGI::Server-0.0.1;
use v6;

sub server_name     returns Str is export { %*ENV<SERVER_NAME>     || 'localhost' }
sub server_software returns Str is export { %*ENV<SERVER_SOFTWARE> || 'cmdline'   }
sub server_port     returns Int is export { +(%*ENV<SERVER_PORT>)  || 80          }
sub server_protocol returns Str is export { %*ENV<SERVER_PROTOCOL> || 'HTTP/1.0'  }

=pod

=head1 NAME

CGI::Server - A CGI module to getting information about the server

=head1 SYNOPSIS

    #!/usr/bin/pugs
    use v6;
    require CGI-0.0.1;
    require CGI::Server-0.0.1;
    
    print header;
    
    if (server_port() != 80) {
        say "You are running " ~ server_software() ~ " on a non-standard port<BR>";
    }
    else {
        say "You are running " ~ server_software() ~ " on the standard port<BR>";
    }

=head1 DESCRIPTION

This module provides information about the server your CGI is running on. It also 
attempts to provide some useful defaults for debugging.

=head1 FUNCTIONS

=over 4

=item B<server_name returns Str>

=item B<server_software returns Str>

=item B<server_port returns Int>

=item B<server_protocol returns Str>

=back

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
