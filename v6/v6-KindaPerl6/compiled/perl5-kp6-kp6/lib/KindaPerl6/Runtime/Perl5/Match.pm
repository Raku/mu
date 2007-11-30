
# Match class for kp6-on-Perl5Regex 

# Documentation in the __END__

use strict;

package Match;

    use Data::Dumper;
    
    # This is the Perl 5 <--> Perl 6 bridge code
    my $perl6_dispatcher =  sub { 
                my ($self, $method, @param) = @_;
                
                if ( $method eq 'true' ) {
                    return ::DISPATCH( $::Bit, 'new', $_[0]->{bool} )
                }
                if ( $method eq 'LOOKUP' ) {
                    my $what = ::DISPATCH( $param[0], 'Str' )->{_value}; 
                    #print "** LOOKUP {$what} ** \n";
                    return $_[0]->{hash}{$what}; 
                }
                if ( $method eq 'does' ) {
                    my $what = ::DISPATCH( $param[0], 'Str' )->{_value};                    
                    return ::DISPATCH( $::Bit, 'new', 0 )
                        if $what eq 'Junction';
                    #print "MATCH DOES $what ???\n";
                    return ::DISPATCH( $::Bit, 'new', 1 );  # it probably does
                }
                if ( $method eq 'scalar' ) {
                    return ::DISPATCH( $self, 'result' );
                }
                if ( $method eq 'result' ) {
                    my $res = $self->result;
                    #if ( ! defined $res ) {
                    #    my $s = $self->Str;
                    #    return ::DISPATCH( $::Str, 'new', $s)
                    #        if defined $s;
                    #}
                    return ::DISPATCH( $::Undef, 'new', )
                        unless defined $res;
                    return $res;
                }
                if ( $method eq 'perl' ) {
                    return ::DISPATCH( $::Str, 'new', 
                          'Match.new( ' 
                        . join( ', ',
                                'from => ' . $self->from,
                                'to => '   . $self->to,
                                'true => ' . $self->true,
                                'str => '  . ${$self->match_str},
                                'result => ' . 
                                    do {
                                        my $s;
                                        eval {
                                            $s = ::DISPATCH( ::DISPATCH( $self, 'result' ), 'perl' )->{_value}
                                        };
                                        $@ ? $@ : $s;
                                    },
                              )
                        . ' )' );
                }
                
                $self->$method( @param );
        };
    
    sub new {
        bless { 
            array  => [], 
            hash   => {}, 
            bool   => 0, 
            result => undef,
            from   => undef, 
            to     => undef, 
            match_str => undef,
            _dispatch => $perl6_dispatcher,
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
    
    sub true   :lvalue { $_[0]->{bool} }
    sub result :lvalue { 
          $_[0]->true  
        ? $_[0]->{result} 
        : ( $_[0]->{result} = undef );
    }
    sub from   :lvalue { $_[0]->{from} }
    sub to     :lvalue { $_[0]->{to} }
    sub match_str :lvalue { $_[0]->{match_str} }

    sub Str {
          $_[0]->true 
        ? (
              defined $_[0]->{result}
            ? ::DISPATCH( $_[0]->{result}, 'Str', )->{_value}
            : substr( ${$_[0]->match_str}, $_[0]->from, $_[0]->to - $_[0]->from )
          )
        : undef;
    }
    sub perl {
        Dumper( $_[0] );
    }

    our @Matches;
    my %actions = (
        create => sub {
            push @Matches, Match->new();
            $Matches[-1]->true = 1;
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

* Str

- return the stringified capture object. 
If there is no capture, return the matched substring

* scalar

- return the capture object
If there is no capture, return the matched substring

* true

- return whether there was a match

* from

- return the string position where the match started

* to

- return the string position immediately after where the match finished

* perl

- return the internal representation as Perl source code. 

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2007 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

