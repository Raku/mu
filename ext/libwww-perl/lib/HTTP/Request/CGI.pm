#!/usr/bin/pugs
use v6;

class HTTP::Request::CGI-0.0.1 {
    has $.query_string;
    
    submethod BUILD () {
        $.method = %*ENV<REQUEST_METHOD>;
        $.uri = $HTTP::URI_CLASS.new(%*ENV<REQUEST_URI>);
        
        $:headers.header(Content-Length => %*ENV<CONTENT_LENGTH>) if %*ENV<CONTENT_LENGTH>.defined;
        $:headers.header(Referer => %*ENV<HTTP_REFERER>) if %*ENV<HTTP_REFERER>.defined;
        $:headers.header(Content-Length => %*ENV<CONTENT_LENGTH>) if %*ENV<CONTENT_LENGTH>;
        $:headers.header(Content-Length => %*ENV<CONTENT_LENGTH>) if %*ENV<CONTENT_LENGTH>;
    }
    
    method params () {
        ...
    }
    
    multi method param (Str $name) {
        ...
    }
    
    multi method param (*%assign) {
        ...
    }
    
    multi method param (*%assign, Bool +$append) {
        ...
    }
    
    multi method param () {
        ...
    }
    
    method delete_param (Str $param) {
        ...
    }
    
    method delete_params () {
        ...
    }
    
    method keywords () {
        ...
    }
}

=pod

=head1 NAME

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 AUTHORS

=head1 COPYRIGHT
