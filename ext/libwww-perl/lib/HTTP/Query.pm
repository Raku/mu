#!/usr/bin/pugs
use v6;

use URI::Escape <uri_unescape>;

class HTTP::Query-0.0.1 {
    does Hash;
    
    has @:keywords;
    has %:params;
    
    method params () {
        return %:params.keys();
    }
    
    multi method param (Str $name) {
        my @val = %:params{$name};
        
        return unless @val > 0;
        
        return (want.List) ?? @val :: @val[0];
    }
    
    multi method param (Str $name, Str *@vals) is rw {
        return new Proxy:
            FETCH => { %:params{$name} };
            STORE => { return (%:params{$name} = @vals) };
    }
    
    multi method param ($self: ) {
        return $self.params();
    }
    
    method parse_params (Str $data) {
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
    
    method parse_keywords (Str $data) {
        $data = uri_unescape($data);
        $data .= trans('+' => ' ');
        
        @:keywords = $data.split(/\s+/);
    }
    
    method delete (Str $name) {
        return if !defined %:params{$name};
        %:params.delete($name);
    }
    
    method clear () {
        %:params.delete($_) for %:params.keys;
    }
    
    method keywords () {
        return @:keywords;
    }
    
    method FETCH ($name)        { return %:params{$name};          }
    method STORE ($name, $val)  { return (%:params{$name} = $val); }
}

=pod

=head1 NAME

HTTP::Query - Class designed to allow easy manipulation of query strings

=head1 SYNOPSIS

require HTTP::Query;

my $q = HTTP::Query.new();

$q.parse_params('foo=bar&baz=quux');

my $params = $q.params(); # or `$q.param()` (for backward compatibility)

my $foo = $q.param('foo');

$q.param('foo') = <an array of values>; # or `$q.param('foo', 'an', 'array', 'of', 'values');`

$q.delete('foo');

$q.clear();

=head1 DESCRIPTION

This class is meant to provide convenient access to the various parts of a query
 string, as passed to it by the user.

=head1 AUTHORS

"Aankhen"

=head1 LICENSE

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
