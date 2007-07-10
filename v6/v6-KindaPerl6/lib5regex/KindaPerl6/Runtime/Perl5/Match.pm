
# Match class for kp6-on-Perl5Regex 

# Documentation in the __END__


# XXX TODO - create {_dispatch} key for P6-MOP bridge

use strict;

package Match;

    use Data::Dumper;
    sub new {
        bless { 
            array  => [], 
            hash   => {}, 
            bool   => 0, 
            result => undef,
            from   => undef, 
            to     => undef, 
            match_str => undef,
            _dispatch => sub { 
                my ($self, $method, @param) = @_;
                $self->$method( @param );
            },
        }, $_[0];
    }
    sub clone {
        bless { 
            array  => [ @{$_[0]->{array}} ], 
            hash   => { %{$_[0]->{hash}} }, 
            bool   => $_[0]->{bool}, 
            result => $_[0]->{result},
            from   => $_[0]->{from}, 
            to     => $_[0]->{to}, 
            match_str => $_[0]->{match_str}, 
            _dispatch => $_[0]->{_dispatch}, 
        }, ref $_[0];
    }
    sub array  :lvalue { $_[0]->{array} }
    sub hash   :lvalue { $_[0]->{hash} }
    sub bool   :lvalue { $_[0]->{bool} }
    sub result :lvalue { $_[0]->{result} }
    sub from   :lvalue { $_[0]->{from} }
    sub to     :lvalue { $_[0]->{to} }
    sub match_str :lvalue { $_[0]->{match_str} }
    
    sub str {
          $_[0]->bool 
        ? substr( ${$_[0]->match_str}, $_[0]->from, $_[0]->to - $_[0]->from )
        : undef;
    }
    sub perl {
        Dumper( $_[0] );
    }

    our @Matches;
    my %actions = (
        create => sub {
            push @Matches, Match->new();
            $Matches[-1]->bool = 1;
            $Matches[-1]->from = $_[0];
            $Matches[-1]->match_str = $_[1];
        },
        to => sub {
            $Matches[-1]->to = $_[0];
        },
        result => sub {
            $Matches[-1]->result = $_[0];
        },
        positional_capture => sub {
            my $match = pop @Matches;
            ${ $Matches[-1]->array }[ $_[0] ] = $match;
        },
        positional_capture_to_array => sub {
            my $match = pop @Matches;
            push @{ 
                    ${ $Matches[-1]->array }[ $_[0] ] 
                }, $match;
        },
        named_capture => sub {
            my $match = pop @Matches;
            ${ $Matches[-1]->hash }{ $_[0] } = $match;
        },
        named_capture_to_array => sub {
            my $match = pop @Matches;
            push @{ 
                    ${ $Matches[-1]->hash }{ $_[0] } 
                }, $match;
        },
        discard_capture => sub {
            pop @Matches;
        },
    );
    sub from_global_data {
        unless ( defined $_[0] ) {
            # no match
            push @Matches, Match->new();
            return;
        }
        my ( $previous, $action, @data ) = @{+shift};
        if ( defined $previous ) {
            from_global_data( $previous );
        }
        local $@;
        eval {
            $actions{ $action }->( @data );
        };
        die "Error in action $action( @data ) - $@" if $@;
    }

1;
__END__

=head1 NAME 

KindaPerl6::Perl5::Match - Match object created by rules

=head1 METHODS

* array

- return the positional matches

* hash

- return both the named and positional (numbered) matches

* str

- return the stringified capture object. 
If there is no capture, return the matched substring

* scalar

- return the capture object
If there is no capture, return the matched substring

* bool

- return whether there was a match

* from

- return the string position where the match started

* to

- return the string position immediately after where the match finished

=head1 "Hash" methods

* elems

* kv

* keys

* values

=head1 "Str" methods

* chars

=head1 OVERLOADS

* $match->()

- return the capture object

* $match->[$n]

- return the positional matches

* $match->{$n}

- return the named matches

* $match ? 1 : 0

- return whether there was a match

=head1 Dumper methods

* data

- return the internal representation as a data structure.

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

Copyright 2006 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

