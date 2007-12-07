package KindaPerl6::Perl5::Match;

# Documentation is at the __END__

=head1 NAME

KindaPerl6::Perl5::Match - Match object created by rules

=head1 VERSION

Version 0.01

=head1 SYNOPSIS

This packages provides a bunch of functions to mimic perl6 most are exported
into the callers package.

WARNING: This $scalar is heavely overloaded.
L<http://search.cpan.org/~nwclark/perl-5.8.8/lib/overload.pm>

=cut

use 5.006;
use strict;
use warnings;
no warnings 'recursion';

#use Class::InsideOut qw( public register id );
use Scalar::Util qw( refaddr );

my %_data;

=head1 OVERLOADS

$MATCH also is available as an array, hash, true/false, code, scalar, and
flat[en](able) though "" and "0+".

* $match->()

- return the capture object

* $match->[$n]

- return the positional matches

* $match->{$n}

- return the named matches

* $match ? 1 : 0

- return whether there was a match

=cut

use overload (
    '@{}'    => \&array,
    '%{}'    => \&hash,
    'bool'   => sub { $_data{ refaddr $_[0] }{bool} },
    '&{}'    => \&code,
    '${}'    => \&scalar,
    '""'     => \&flat,
    '0+'     => \&flat,
    fallback => 1,
);

=head1 INSIDE OUT CLASS

Internal to Match.pm is %_data which will store the information for the object.

 $_data{ 1234 } -> {
                        from => ?,
                        to => ?,
                        capture => ?,
                        bool => ?,
                        match => [],
                        named => {},
                        state => ?, # obsolete?
                    }
 $_data{ 2345 } -> { ... }

The "key" for the object is the memory address for the object as returned by
Scalar::Util::refaddr.

=head1 FUNCTIONS

=head2 new

 ::fail can be called from inside closures
 sub ::fail { $::_V6_SUCCEED = 0 }

 KindaPerl6::Perl5::Match->new(
     from => ?,
     to => ?,
     capture => ?,
     bool => ?,
     match => [],
     named => {},
 );

=cut

sub new {
    my $class = shift;
    my $obj = bless \$class, $class;

    #print "Match->new( @_ ) ",(refaddr $obj),"\n";

    # assign $_data{ memory address } = { arguments }
    $_data{ refaddr $obj } = {@_};

    # refaddr $obj, will be the internal "name" of this object.
    return $obj;
}

=head2 DESTROY

Destory deletes a pair in %_data, it does not delete the object.

=cut

sub DESTROY {
    delete $_data{ refaddr $_[0] };
}

=head2 data

Returns the contents of $_data{ refaddr $_[0] }

- return the internal representation as a data structure.

=cut

sub data {
    $_data{ refaddr $_[0] };
}

=head2 capture

Accessor method for "capture"

=cut

sub capture {
    @_ == 1 ? ( $_data{ refaddr $_[0] }{capture} ) : ( $_data{ refaddr $_[0] }{capture} = $_[1] );
}

#sub from  {    $_data{refaddr $_[0]}->{from}   }
#sub to    {    $_data{refaddr $_[0]}->{to}     }
#sub bool  {    $_data{refaddr $_[0]}->{bool}   }

=head2 from

Accessor method for "from"

- return the string position where the match started

=cut

sub from {
    @_ == 1 ? ( $_data{ refaddr $_[0] }{from} ) : ( $_data{ refaddr $_[0] }{from} = $_[1] );
}

=head2 to

Accessor method for "to"

- return the string position immediately after where the match finished

=cut

sub to {
    @_ == 1 ? ( $_data{ refaddr $_[0] }{to} ) : ( $_data{ refaddr $_[0] }{to} = $_[1] );
}

=head2 bool

Accessor method for "bool"

- return whether there was a match

=cut

sub bool {
    @_ == 1 ? ( $_data{ refaddr $_[0] }{bool} ) : ( $_data{ refaddr $_[0] }{bool} = $_[1] );
}

=head2 scalar

- return the capture object

If there is no capture, return the matched substring

=cut

sub scalar {
    return \( $_[0]->flat );
}

=head2 array

Return the positional matches in an array reference.

=cut

sub array {
    $_data{ refaddr $_[0] }->{match}
        || ( $_data{ refaddr $_[0] }->{match} = [] );
}

=head2 hash

return an hash reference for named (somethings) (named matched arguments)

(old documentation:

* hash

- return both the named and positional (numbered) matches

)

=cut

sub hash {
    $_data{ refaddr $_[0] }->{named}
        || ( $_data{ refaddr $_[0] }->{named} = {} )

        # XXX - doesn't work as lvalue
        #    my $array =
        #             $_data{refaddr $_[0]}->{match}
        #        || ( $_data{refaddr $_[0]}->{match} = [] );
        #    return {
        #        %{ $_data{refaddr $_[0]}->{named} || {} },
        #        (
        #        map { ( $_, $array->[$_] ) }
        #            0 .. $#$array
        #        ),
        #    }
}

=head1 "Hash" methods

* kv

* keys

* values

=cut

=head2 elems

Returns element count

=cut

sub elems {
    scalar $_[0]->keys;
}

=head2 kv

returns ($key1, $value1, $key2, $value2 ... $keyN, $valueN)

=cut

sub kv {
    map { ( $_, $_[0]->{$_} ) } $_[0]->keys;
}

=head2 keys

acts in place of CORE::keys

=cut

sub keys {
    CORE::keys %{ $_data{ refaddr $_[0] }->{named} }, 0 .. $#{ $_[0]->array };
}

=head2 values

acts in place of CORE::values

=cut

sub values {
    CORE::values %{ $_data{ refaddr $_[0] }->{named} }, @{ $_[0]->array };
}

=head1 String functions

=head2 chars

returns the length (CORE::length) of a string.

=cut

sub chars {
    CORE::length $_[0]->Str;
}

=head2 flat

returns a flattened, something...

=cut

sub flat {
    my $obj = $_data{ refaddr $_[0] };
    my $cap = $obj->{capture};

    #print ref $cap;
    return $cap if defined $cap;
    return '' unless $obj->{bool};
    return '' if $_[0]->from > length( $obj->{str} );

    return substr( $obj->{str}, $_[0]->from, $_[0]->to - $_[0]->from );
}

=head2 str

deprecated
TODO: check to see if this can be removed.

=cut

sub str {
    "" . $_[0]->flat;

    #TODO die "?->str called instead of ?->Str";
}

=head2 Str

- return the stringified capture object.
If there is no capture, return the matched substring

=cut

sub Str {
    "" . $_[0]->flat;
}

=head1 Dumper methods

=head2 perl

- return the internal representation as Perl source code.

=cut

sub perl {
    require Data::Dumper;
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Pad      = '  ';
    return __PACKAGE__ . "->new( " . Dumper( $_[0]->data ) . ")\n";
}

=head2 yaml

- return the internal representation as YAML.
Requires the C<YAML::Syck> module.

=cut

sub yaml {
    require YAML::Syck;

    # interoperability with other YAML/Syck bindings:
    $YAML::Syck::ImplicitTyping = 1;
    YAML::Syck::Dump( $_[0] );
}

1;

__END__

# tail() for backwards compatibility
# - doesn't work on failed matches
sub tail {
    return substr( ${$_data{refaddr $_[0]}->{str}}, $_[0]->to );
}

# state() is used for multiple matches and backtracking control
sub state {
    return $_data{refaddr $_[0]}->{state};
}

# return the capture
sub code {
    my $c = $_[0];
    return sub { $c->flat };
}

1;

__END__

=head1 SEE ALSO

C<v6> on CPAN

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
