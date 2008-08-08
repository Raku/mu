package Pugs::Runtime::StrPos;
# Documentation in the __END__

#use 5.006; ???
use strict;
use warnings;
use utf8;
use Data::Dumper;

use overload (
    'bool'   => \&bool,
    '0+'     => \&graphs,
    '++'     => sub { 
        $_[0]->{codes} = ($_[0]->add_graphs( 1 ))->{codes}; 
    },
    '+='     => sub { 
        $_[0]->{codes} += $_[1]; 
    },
    '+'     => sub { 
        ($_[0]->add_graphs( $_[1] ))->{codes}; 
    }, 
    '='     => sub {
           my $self = shift;
           bless {%$self}, ref $self;
         }, 
    fallback => 1,
);

# new({ str => "...", codes => $n });
# $str must be utf-8
# $n can be undef, meaning "nowhere in this string" 
sub new {
    return bless $_[1], $_[0];
}

# codes is the default perl5 representation
# graphs is the default perl6 representation
sub from_str_graphs {
    
    # -- downgrade as appropriate
    #die "string has invalid internal encoding" 
    return from_str_codes( @_ )
        unless utf8::is_utf8( $_[1] );
        
    $_[1] =~ m/^\X{$_[2]}/g;
    return bless { str => $_[1], codes => pos($_[1]) }, $_[0];
}

sub from_str_codes {
    return bless { str => $_[1], codes => $_[2] }, $_[0];
}

sub from_str {
    return bless { str => $_[1], codes => pos( $_[1] ) }, $_[0];
}

sub clone {
    return bless { %{$_[0]} }, ref( $_[0] );
}

sub bool  { 
    defined $_[0]->{codes} 
}

sub codes  { 
    $_[0]->{codes} 
}

sub bytes { 
    return undef unless defined $_[0]->{codes};

    # -- downgrade as appropriate
    #die "string has invalid internal encoding" 
    return $_[0]->codes
        unless utf8::is_utf8( $_[0]->{str} );

    my $s = substr( $_[0]->{str}, 0, $_[0]->{codes} );
    {
        use bytes;
        return length( $s );
    }
    #my @bytes = unpack("C*", substr( $_[0]->{str}, 0, $_[0]->{codes} ) );
    #print "[",join(",", @bytes),"]\n";
    #return scalar @bytes;
}

sub graphs { 
    return undef unless defined $_[0]->{codes};

    # -- downgrade as appropriate
    #die "string has invalid internal encoding" 
    return $_[0]->codes
        unless utf8::is_utf8( $_[0]->{str} );
        
    #return scalar( substr( $_[0]->{str}, 0, $_[0]->{codes} ) =~ m/\X/g );
    my @g = substr( $_[0]->{str}, 0, $_[0]->{codes} ) =~ m/\X/g;
    return scalar @g;
}

sub langs {
    die "TODO: langs()";
}

sub add_codes {
    (ref $_[0])->from_str_codes( $_[0]->{str}, $_[0]->codes + $_[1] );
}

sub add_graphs {
    (ref $_[0])->from_str_graphs( $_[0]->{str}, $_[0]->graphs + $_[1] );
}

sub perl {
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Pad = '  ';
    return Dumper( $_[0] );
}

sub yaml {
    require YAML::Syck;
    # interoperability with other YAML/Syck bindings:
    $YAML::Syck::ImplicitTyping = 1;
    YAML::Syck::Dump( $_[0] );
}

# for Pugs interoperability
sub dump_hs {
    die "TODO";
}

1;

__END__

=head1 NAME 

Pugs::Runtime::StrPos - Represent a position inside a string

=head1 OPTIONAL MODULES

* YAML::Syck

- in order to support 'yaml()'

=head1 METHODS

* new({ str => "...", codes => $n });

create a new StrPos object.

'str' must be utf-8.

'codes' can be undef, meaning "nowhere in this string".

* from_str( $str )

create a new StrPos object, using the perl5-string internal 'pos'.

* from_str_graphs( $str, $graphs )
* from_str_codes( $str, $codes )

create a new StrPos object using grapheme or codepoint units.

* bytes()
* codes()
* graphs()

- return the position as an integer, counting in bytes, codepoints, or graphemes.

* langs()

- TODO - return the position as an integer, counting in language dependent chars. 

* bool()

- test whether a position is defined.

* add_codes($n)
* add_graphs($n)

- return a new StrPos with the new position

* clone

- return a new StrPos with the same position

=head1 OVERLOADS

* numeric

- return the count of graphemes, or undef.

* boolean 

- test whether a position is defined. The position 'zero' is true.

=head1 Dumper methods

* perl

- return the internal representation as Perl source code. 

* yaml

- return the internal representation as YAML. 
Requires the C<YAML::Syck> module.

* dump_hs

- for Pugs interoperability

=head1 SEE ALSO

C<v6> on CPAN

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

