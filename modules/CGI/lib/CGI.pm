use v6;

module CGI-0.0.1;

## set up all the globals

my %PARAMS;

my $REQUEST_METHOD;
my $CONTENT_LENGTH;
my $CONTENT_TYPE;
my $QUERY_STRING;

## functions

# information functions

multi sub param returns Array is export { keys(%PARAMS) }
multi sub param (Str $key) returns Str is export { %PARAMS{$key} }

sub request_method returns Str { $REQUEST_METHOD }
sub content_type   returns Str { $CONTENT_TYPE   }
sub content_length returns Str { $CONTENT_LENGTH }

# utility functions

sub header returns Str is export { "Content-type: text/html\n\n" }

sub url_decode (Str $to_decode) returns Str is export {
    my $decoded = $to_decode;
    $decoded ~~ s:perl5:g/\+/ /;
    ## NOTE:
    # this does not work yet, (code in regexp)
    # the code is actually taken from iblech's 
    # port of CGI::Lite, neither he nor I are
    # sure if it is correct
    # $decoded ~~ s:perl5:g/%(<[\da-fA-F]>**{2})/{chr :16($1)}/; 
    return $decoded;
}

sub url_encode (Str $to_encode) returns Str is export  {
    my $encoded = $to_encode;
    ## NOTE:
    # this does not work yet, (code in regexp)
    # the code is actually taken from iblech's 
    # port of CGI::Lite (    
    #$encoded ~~ s:perl5:g/(<-[-.\w ]>)/{ord($1).as('%%%02X')}/;
    $encoded ~~ s:perl5:g/ /\+/;
    return $encoded;
}

sub pack_params returns Str is export {
    my @packed_params;
    for param() -> $param_key {
        push(@packed_params, (url_encode($param_key) ~ '=' ~ url_encode(param($param_key))));
    }
    return join('&', @packed_params);
}

sub unpack_params (Str $data) returns Str is export {
    my @pairs = split('&', $data);
    for @pairs -> $pair {
        my ($key, $value) = split('=', $pair);
        %PARAMS{url_decode($key)} = url_decode($value);
    }  
}

## now initialize all the globals

try {
    $REQUEST_METHOD = %*ENV<REQUEST_METHOD>;
    $CONTENT_TYPE   = %*ENV<CONTENT_TYPE>;    
    $CONTENT_LENGTH = %*ENV<CONTENT_LENGTH>;   
        
    if (lc($REQUEST_METHOD) eq ('get' | 'head')) {
        $QUERY_STRING = %*ENV<QUERY_STRING>;
        unpack_params($QUERY_STRING) if $QUERY_STRING;
    }
    elsif (lc($REQUEST_METHOD) eq 'post') { 
        if (!$CONTENT_TYPE || $CONTENT_TYPE eq 'application/x-www-form-urlencoded') {
            my $content; # = read($*IN, $CONTENT_LENGTH);
            unpack_params($content) if $content;
        }
    }
    elsif (@ARGS) {
        my $input = join('', @ARGS);
        unpack_params($input);
    }
    else {
        die "Invalid Content Type" if $REQUEST_METHOD; # only die if we are running under CGI
    }
}
if ($!) {
    print header;
    say "There was an error getting the params:\n\t" ~ $!;
    exit();
}	

=pod

=head1 NAME

CGI - A basic CGI module for Pugs

=head1 SYNOPSIS

    #!/usr/bin/pugs
    use v6;
    require CGI-0.0.1;
    
    print header;
    
    if (param()) {
        for param() -> $key {
            say $key ~ " => " ~ param($key) ~ "<BR>";
        }
    }
    else {
        say "<FORM><INPUT TYPE='text' NAME='test'><INPUT TYPE='submit'></FORM>";
    }
    
    # you can also test it on the command line too
    % pugs -Ilib examples/test.p6 "greetings=hello world"    

=head1 DESCRIPTION

CGI for Pugs! 

=head1 LIMITATIONS & CAVEATS

Right now, this is a proof of concept, so it does not do much 
at all. Pugs is also missing a few features to make things like
POST work. 

=head1 FUNCTIONS

=head2 Informational

=over 4

=item B<param returns Array>

=item B<param (Str $key) returns Str>

=item B<request_method returns Str>

=item B<content_type returns Str>

=item B<content_length returns Str>

=back

=head2 Utility

=over 4

=item B<header returns Str>

=item B<url_decode (Str $to_decode) returns Str>

=item B<url_encode (Str $to_encode) returns Str>

=item B<pack_params returns Str>

=item B<unpack_params (Str $data) returns Str>

=back

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
