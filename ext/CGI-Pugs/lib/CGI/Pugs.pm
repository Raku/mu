use v6;

module CGI::Pugs-0.0.1;

my %PARAMS;

my $REQUEST_METHOD = %*ENV<REQUEST_METHOD>;
my $CONTENT_LENGTH = %*ENV<CONTENT_LENGTH>;
my $CONTENT_TYPE   = %*ENV<CONTENT_TYPE>;
my $QUERY_STRING;

my $decode_url_data = -> (Str $data) {
    my @pairs = split('&', $data);
    for @pairs -> $pair {
        my ($key, $value) = split('=', $pair);
        %PARAMS{$key} = $value;
    }  
};

if (lc($REQUEST_METHOD) eq ('get' | 'head')) {
    $QUERY_STRING = %*ENV<QUERY_STRING>;
    $decode_url_data($QUERY_STRING) if $QUERY_STRING;
}
elsif (lc($REQUEST_METHOD) eq 'post') {
    if (!$CONTENT_TYPE || $CONTENT_TYPE eq 'application/x-www-form-urlencoded') {
        my $content; # = read($*IN, $CONTENT_LENGTH);
        $decode_url_data($content) if $content;
    }
}
else {
    die "Invalid Content Type";
}

sub header returns Str is export { "Content-type: text/html\n\n" }

multi sub param returns Array is export { keys(%PARAMS) }
multi sub param (Str $key) returns Str is export { %PARAMS{$key} }

=pod

=head1 NAME

CGI::Pugs - A basic CGI module for Pugs

=head1 SYNOPSIS

    #!/usr/bin/pugs
    use v6;
    require CGI::Pugs-0.0.1;
    
    say header;
    
    if (param()) {
        for param() -> $key {
            say param($key) ~ "<BR>";
        }
    }
    else {
        say "<FORM><INPUT TYPE='text' NAME='test'><INPUT TYPE='submit'></FORM>";
    }

=head1 DESCRIPTION

CGI for Pugs!

=head1 LIMITATIONS & CAVEATS

Right now, this is a proof of concept, so it does not do much 
at all. Pugs is also missing a few features to make things like
POST work. 

=head1 FUNCTIONS

=over 4

=item B<header returns Str>

=item B<param returns Array>

=item B<param (Str $key) returns Str>

=back

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
