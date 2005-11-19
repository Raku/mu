#!/usr/bin/pugs
use v6;

use URI::Escape <uri_unescape>;

class HTTP::Query-0.0.1 {
    does Hash;
    
    has @!keywords;
    has @!params;
    has %!params;
    
    method params () {
        return @!params;
    }
    
    multi method param (Str $name) {
        my @val = %!params{$name};
        
        return unless @val > 0;
        
        return (want.List) ?? @val !! @val[0];
    }
    
    multi method param ($query: Str $name, Str *@vals) is rw {
        return new Proxy:
            FETCH => -> $name {
                return $query.param($name);
            },
            STORE => -> $name, *@vals {
                %!params{$name} = @vals;
                return $query.param($name);
            };
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
            
            if (%!params.exists($key)) {
                @{%!params{$key}}.push($value);
            } else {
                %!params{$key} = [ $value ];
            }
            
            if ($key != any(@!params)) {
                @!params.push($key);
            }
        }
    
        return bool::true;
    }
    
    method parse_keywords (Str $data) {
        $data = uri_unescape($data);
        $data .= trans('+' => ' ');
        
        @!keywords = $data.split(/\s+/);
    }
    
    method delete (Str $name) {
        return if !defined %!params{$name};
        %!params.delete($name);
    }
    
    method clear () {
        %!params.delete($_) for %!params.keys;
    }
    
    method keywords () {
        return @!keywords;
    }
    
    method FETCH ($name)        { return %!params{$name};          }
    method STORE ($name, $val)  { return (%!params{$name} = $val); }
}

=pod

=head1 NAME

HTTP::Query - Class designed to allow easy manipulation of query strings

=head1 SYNOPSIS

    require HTTP::Query;
    
    my $q = HTTP::Query.new();
    
    $q.parse_params('foo=bar&baz=quux');
    
    my $params = $q.params();
        # or `$q.param()` (for backward compatibility)
    
    my $foo = $q.param('foo');
    
    $q.param('foo') = <an array of values>;
        # or `$q.param('foo', 'an', 'array', 'of', 'values');`
    
    $q.delete('foo');
    
    $q.clear();

=head1 DESCRIPTION

An object of this class represents the query string portion of a requested URI.
It provides the following methods:

=over 8

=item C<$q.parse_params($data)>

Parses the given string, seperating it into 'name' and 'value' pairs.  This
method should be called before any other methods, as it initialises the list of
parameters.

=item C<$q.params()>

Returns the names of all the parameters as a list.  The order in which they were
present in the query string is preserved.

=item C<$q.param()>

This exists as an alias for C<params>.

=item C<$q.param($name)>

Returns the value of the parameter named C<$name>.  If called in List context,
it will return a list of values.  Otherwise, it will return the first value.

=item C<$q.param($name, $val1, $val2...)> or C<$q.param($name) = @vals>

Assigns C<@vals> to the parameter named C<$name>.

=item C<$q.delete($name)>

Deletes the parameter named C<$name>.

=item C<$q.clear()>

Deletes all the parameters.

=back

=head1 AUTHORS

"Aankhen"

=head1 LICENSE

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
