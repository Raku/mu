package Pugs::Runtime::StrPos;
# Documentation in the __END__

use 5.006;
use strict;
use warnings;
use utf8;
use Data::Dumper;

use overload (
    'bool'   => \&bool,
    '0+'     => \&codepoints,
    fallback => 1,
);

# new({ str => "...", codepoints => $n });
# $str must be utf-8
# $n can be undef, meaning "nowhere in this string" 
sub new {
    return bless $_[1], $_[0];
}

sub from_str {
    return bless { str => $_[1], codepoints => pos( $_[1] ) }, $_[0];
}

sub bool  { 
    defined $_[0]->{codepoints} 
}

sub codepoints  { 
    $_[0]->{codepoints} 
}

sub bytes { 
    return undef unless defined $_[0]->{codepoints};
    die "string has invalid internal encoding" 
        unless utf8::is_utf8( $_[0]->{str} );
    my $s = substr( $_[0]->{str}, 0, $_[0]->{codepoints} );
    {
        use bytes;
        return length( $s );
    }
    #my @bytes = unpack("C*", substr( $_[0]->{str}, 0, $_[0]->{codepoints} ) );
    #print "[",join(",", @bytes),"]\n";
    #return scalar @bytes;
}

sub graphemes { die "TODO: graphemes()" }

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
    my $obj;
    if (ref($_[0]) eq 'SCALAR') {
        $obj = ${$_[0]};
    }
    else {
        #$obj = $_data{refaddr $_[0]};
    }

    if ($obj) {
        # Ok, this is a genuine Match object.
        return "PGE_Fail" unless ${$obj->{bool}};

        # Now we matched; dump the rest of data
        join(' ', 'PGE_Match', ${$obj->{from}}, ${$obj->{to}},
            ('['.join(', ', map { dump_hs($_) } @{$obj->{match}||[]} ).']'),
            ('['.join(', ', map {
                my $str = $_;
                if ( my $dump = dump_hs($obj->{named}{$_}) ) {
                    $str =~ s/([^ \!\#\$\%\&\x28-\x5B\x5D-\x7E])/'\\'.ord($1)/eg;
                    qq[("$str", $dump)];
                }
                else {
                    ();
                }
            } sort(CORE::keys(%{$obj->{named}||{}})) ).']'),
        )
    }
    elsif (ref($_[0]) eq 'ARRAY') {
        return "PGE_Array [" . join(', ', map { dump_hs($_) } @$obj) . "]"
    }
    elsif (!ref($_[0])) {
        my $str = shift;
        $str =~ s/([^ \!\#\$\%\&\x28-\x5B\x5D-\x7E])/'\\'.ord($1)/eg;
        return "PGE_String \"$str\"";
    }
    else {
        warn "Unrecognized blessed match object: $_[0]";
        return '';
    }
}

1;

__END__

=head1 NAME 

Pugs::Runtime::StrPos - Represent a position inside a string

=head1 METHODS

* new({ str => "...", codepoints => $n });

creates a new StrPos object.

'str' must be utf-8.

'codepoints' can be undef, meaning "nowhere in this string".

* from_str( $str )

encodes the string internal 'pos' into a StrPos object.

* codepoints()
* bytes()
* graphemes()

- return the position.

* bool()

- test whether the position is defined.

=head1 OVERLOADS

* numeric $pos

- return codepoints() as an integer, or undef.

* boolean $pos

- test whether codepoints() is defined.

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

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

