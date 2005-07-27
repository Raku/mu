#!/usr/bin/pugs
use v6;

use URI::Escape <uri_unescape>;
require HTTP::Headers;

class HTTP::Request::CGI-0.0.1 {
    is HTTP::Headers;
    
    has $.query_string;
    has %:params;
    
    submethod BUILD ($r: ) {
        $.method = %*ENV<REQUEST_METHOD>;
        $.uri = $HTTP::URI_CLASS.new(%*ENV<REQUEST_URI>);
        
        $:headers.header(Content-Length => %*ENV<CONTENT_LENGTH>) if %*ENV<CONTENT_LENGTH>.defined;
        $:headers.header(Content-Type => %*ENV<CONTENT_TYPE>) if %*ENV<CONTENT_TYPE>.defined;
        $:headers.header(Referer => %*ENV<HTTP_REFERER>) if %*ENV<HTTP_REFERER>.defined;
        
        $r.:load_params();
    }
    
    method params () {
        return %:params.keys();
    }
    
    multi method param (Str $name) {
        my @val = %:params{$name};
        
        return unless @val > 0;
        
        return (want.List) ?? @val :: @val[0];
    }
    
    multi method param (Str $name, Str *@vals is copy) is rw {
        %:params{$name} = \@vals;
    }
    
    multi method param ($r: ) {
        return $r.params();
    }
    
    method delete_param (Str $name) {
        return if !defined %:params{$name};
        %:params.delete($name);
    }
    
    method delete_params () {
        %:params.delete($_) for %:params.keys;
    }
    
    method keywords () {
        ...
    }
    
    method :load_params ($r: ) {
        if ($.method.lc() eq 'get'|'head') {
            $.query_string = %*ENV<QUERY_STRING> // %*ENV<REDIRECT_QUERY_STRING>;
            $r.:unpack_params($.query_string);
        } elsif ($.method.lc() eq 'post') {
            my $type = $r.header('Content-Type');
            
            if (!$type || $type eq 'application/x-www-form-urlencoded') {
                my $content;
                $r.:unpack_params($content);
            }
        } elsif (@*ARGS.elems > 0) {
            $r.:unpack_params([~] @*ARGS);
        } else {
            # XXX
        }
    }
    
    method :unpack_params (Str $data) {
        my @pairs = $data.split(/;|&/);
        
        for @pairs -> $pairs {
            my ($key, $value) = $pair.split('=');
            
            $key = uri_unescape($key);
            $value = uri_unescape($value);
            
            if (%:params.exists($key)) {
                @{%:params{$key}}.push($value);
            } else {
                %:params{$key} = [ $value ];
            }
        }
    
        return bool::true;
    }
}

=pod

=head1 NAME

HTTP::Request::CGI - Subclass of HTTP::Request for dealing with CGI-generated requests.

=head1 SYNOPSIS

require HTTP::Request::CGI;

my $r = HTTP::Request::CGI.new();

my $params = $r.params(); # or `$r.param()` (for backward compatibility)

my $foo = $r.param('foo');

$r.param('foo') = <an array of values>; # or `$r.param('foo', 'an', 'array', 'of', 'values');`

$r.delete_param('foo');

$r.delete_params();

=head1 DESCRIPTION

This module is meant to ease the creation of CGI scripts by providing convenient
 access to various environment variables, as well as the parameters of the
 request.

=head1 AUTHORS

"Aankhen"

=head1 LICENSE

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
