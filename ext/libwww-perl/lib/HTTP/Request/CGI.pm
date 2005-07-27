#!/usr/bin/pugs
use v6;

use URI::Escape <uri_unescape>;

require HTTP::Headers;
require HTTP::Query;

require URI;

class HTTP::Request::CGI-0.0.1[::URI_CLASS = URI] {
    is HTTP::Headers;
    
    has $.query_string;
    has $.query handles «param params keywords :delete_param<delete> :delete_params<clear>»;
    
    has @:keywords;
    has %:params;
    
    submethod BUILD ($r: ) {
        $.method = %*ENV<REQUEST_METHOD>;
        $.uri = ::URI_CLASS.new(%*ENV<REQUEST_URI>);
        
        $:headers.header(Content-Length => %*ENV<CONTENT_LENGTH>) if %*ENV<CONTENT_LENGTH>.defined;
        $:headers.header(Content-Type => %*ENV<CONTENT_TYPE>) if %*ENV<CONTENT_TYPE>.defined;
        $:headers.header(Referer => %*ENV<HTTP_REFERER>) if %*ENV<HTTP_REFERER>.defined;
        
        $.query = HTTP::Query.new();
        
        $r.:load_params();
    }
    
    method :load_params ($self: ) {
        if ($.method.lc() eq 'get'|'head') {
            $.query_string = %*ENV<QUERY_STRING> // %*ENV<REDIRECT_QUERY_STRING>;
            
            if ($.query_string ~~ /<[;&=]>/) {
                $.query.parse_params($.query_string);
            } else {
                $.query.parse_keywords($.query_string);
            }
        } elsif ($.method.lc() eq 'post') {
            my $type = $self.header('Content-Type');
            
            if (!$type || $type eq 'application/x-www-form-urlencoded') {
                my $content;
                $.query.:parse_params($content);
            }
        } elsif (@*ARGS.elems > 0) {
            $.query.:parse_params([~] @*ARGS);
        } else {
            # XXX
        }
    }
}

=pod

=head1 NAME

HTTP::Request::CGI - Subclass of HTTP::Request for dealing with CGI-generated requests.

=head1 SYNOPSIS

require HTTP::Request::CGI;

my $r = HTTP::Request::CGI.new();

=head1 DESCRIPTION

This class is meant to ease the creation of CGI scripts by providing convenient
 access to various environment variables, as well as the parameters of the
 request (through the HTTP::Query object that is part of each instance).

=head1 AUTHORS

"Aankhen"

=head1 LICENSE

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
