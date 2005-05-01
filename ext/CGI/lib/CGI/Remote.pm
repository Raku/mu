module CGI::Remote-0.0.1;
use v6;

sub remote_host     returns Str is export { %*ENV<REMOTE_HOST> || %*ENV<REMOTE_ADDR> || 'localhost' }
sub remote_address  returns Str is export { %*ENV<REMOTE_ADDR> || '127.0.0.1' }
sub remote_ident    returns Str is export { %*ENV<REMOTE_INDENT> }
sub remote_user     returns Str is export { %*ENV<REMOTE_USER> }
sub auth_type       returns Str is export { %*ENV<AUTH_TYPE> }

=pod

=head1 NAME

CGI::Remote - A CGI module to getting information about the remote user

=head1 SYNOPSIS

    #!/usr/bin/pugs
    use v6;
    require CGI-0.0.1;
    require CGI::Remote-0.0.1;
    
    print header;
    
    if (auth_type() eq 'Basic') {
        say "Hello " ~ remote_user() ~ "<BR>";
    }
    else {
        say "Unauthorized Access!!!!<BR>"
    }

=head1 DESCRIPTION

This module provides information about the remote user that is using your CGI. 
It also attempts to provide some useful defaults for debugging.

=head1 FUNCTIONS

=over 4

=item B<remote_host returns Str>

=item B<remote_address returns Str>

=item B<remote_ident returns Int>

=item B<remote_user returns Str>

=item B<auth_type returns Str>

=back

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
