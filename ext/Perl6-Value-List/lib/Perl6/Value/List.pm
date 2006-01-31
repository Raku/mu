use v6;

# Perl6::Value::List - implementation of Perl6 'List' class in Perl6

=for ChangeLog

2005-08-10
* New methods List.flatten(), is_lazy(), from_coro( $sub )
* Factored Perl6::Value::List out of the Array package

=cut

# TODO - sync with Perl5 version:
# * Separate from_num_range() and from_range() constructors. 
#   - from_num_range() is a numeric range. It accepts a 'step' value.
#   - from_range() is a generic range for strings, etc. It accepts a 'celems' closure.
#   Both constructors are just new() wrappers.
# * grep(), map() don't depend on coroutines
# * Removed pair() - this module does not have access to the Pair constructor
#
# TODO - is_contiguous() should test if $step == 1
# TODO - test flatten(), is_lazy(), error messages
# TODO - emit error message if attempting to flatten() an infinite list 
# TODO - does zip() has additional parameters?
# TODO - document unsupported operations: join, reduce, sort - call fail()
# TODO - check grep() syntax
# TODO - provide a better default stringify - see Span.pm
# TODO - fix elems() in from_range(), when start/end are Str - 'a'..'z'
#      - add tests
# TODO - rewrite ops using closures (instead of coro)
# TODO - reuse map() to write ops

# Things that will be solved by the compiler:
# - keys/kv/pairs/values with indexes (S29) --> array slice
# - lists of junctions --> junctions of lists
# - list concatenation --> array concatenation

# ---------

class Perl6::Value::List {
    does List;
    has Code $.cstart;
    has Code $.cend;
    has Code $.celems;
    has Code $.cis_infinite;
    has Code $.cis_contiguous;
    has Code $.cstringify;
    has Bool $.is_lazy;

    submethod BUILD () {
        $.cis_infinite   //= sub { &{$.celems}() == Inf },
        $.cis_contiguous //= sub { bool::false }, 
        $.cstringify     //= sub { &{$.cstart}() ~ '....' ~ &{$.cend}() }, 
        $.is_lazy        //= bool::true,
        $.celems         //= ( defined $.cstart || defined $.cend ) ?? 
                             sub { Inf } !! 
                             sub { 0 };
        $.cstart         //= sub {};
        $.cend           //= sub {};    
    }

    method start         () { &{$.cstart}() }  # == shift
    method end           () { &{$.cend}() }    # == pop

    method elems         () { &{$.celems}() }
    method is_infinite   ( $self: ) { &{$.cis_infinite}() }
    method is_contiguous ( $self: ) { &{$.cis_contiguous}() }
    method to_str        ( $self: ) { &{$.cstringify}() }
    # method clone         ( $self: ) { $self }  --- auto generated
    method to_ref        ( $self: ) { $self }
    method to_bit        ( $self: ) { $self.elems > 0 }
    method to_num        ( $self: ) { $self.elems }
    method to_list       ( $self: ) { $self }

    # method is_lazy       ( $self: ) { ... }  --- auto generated
    method flatten       ( $self: ) { 
        my $ret = $array;

        # TODO - add tests for this error message
        # fail "can't instantiate an infinite list"
        #     if $ret.is_infinite;

        my @list;
        while $ret.elems { *push @list, $ret.shift; }
        $self.from_single( @list ); 
    }

    method from_range ( $class: $start is copy, $end is copy, $step? ) {
        $class.new(
                    cstart =>  sub {
                                my $r = $start;
                                if ( defined $step ) { $start += $step } else { $start++ };
                                return $r;
                            },
                    cend =>    sub {
                                my $r = $end;
                                if ( defined $step ) {
                                    # XXX - this should use modulus, etc.
                                    $end -= $step
                                }
                                else {
                                    $end--
                                };
                                return $r;
                            },
                    celems =>  sub {
                                return $end - $start + 1 unless defined $step;
                                return int(( $end - $start + 1 ) / $step);
                            },
                    cis_infinite => sub { return $start == -Inf || $end == Inf },
                    cis_contiguous => sub { $step == -1 | 1 | undef },
        );
    }

    method from_single ( $class: @list is copy ) {
        $class.new( cstart => sub{ shift @list },
                    cend =>   sub{ pop @list },
                    celems => sub{ +@list },
                    is_lazy => bool::false );
    }

    method from_coro ( $class: $start ) {
        my $size = Inf;
        $class.new(
                    cstart =>  sub {
                                my $r = &{$start}();
                                $size = 0 unless defined $r;
                                return $r;
                            },
                    cend =>    sub {},
                    celems =>  sub { $size },
                    cis_infinite => sub { $size == Inf },
                    cis_contiguous => sub { bool::false },
        );
    }

    # --- list operations ---

    method reverse ( $array: ) { 
        my $ret = $array;
        Perl6::Value::List.new( 
                cstart =>         $ret.cend,
                cend =>           $ret.cstart,
                celems =>         $ret.celems,
                cis_infinite =>   $ret.cis_infinite,
                cis_contiguous => $ret.cis_contiguous,
                cstringify =>     $ret.cstringify,
        );
    }

    method grep ( $array: Code $code ) { 
        my $ret = $array; 
        Perl6::Value::List.new(
                cstart => coro {
                        my $x = $ret.shift // yield;
                        yield $x if &$code($x) 
                },
                cend => coro { 
                        my $x = $ret.pop // yield;
                        yield $x if &$code($x) 
                },
                # TODO - signal end of data using 'elems()'
        );
    }

    method map ( $array: Code $code ) { 
        my $ret = $array; 
        Perl6::Value::List.new(
                cstart => coro {
                        my @ret;
                        my $x = $ret.shift // yield;
                        unshift(@ret, &$code($x)); 
                        yield shift(@ret) while @ret
                },
                cend => coro {
                        my @ret; 
                        my $x = $ret.pop // yield;
                        push(@ret, &$code($x));
                        yield pop(@ret) while @ret  
                },
                # TODO - signal end of data using 'elems()'
        )
    }

    method uniq ( $array: ) { 
        my %seen = ();
        my $ret = $array; 
        Perl6::Value::List.new(
                cstart => coro {
                        my $x = $ret.shift // yield;
                        unless %seen{$x} { 
                            %seen{$x} = bool::true; 
                            yield $x 
                        }                       
                },
                cend => coro {
                        my $x = $ret.pop // yield;
                        unless %seen{$x} { 
                            %seen{$x} = bool::true; 
                            yield $x 
                        }  
                },
                # TODO - signal end of data using 'elems()'
        )
    }

    method kv ( $array: ) { 
        my $ret = $array; 
        my $count = 0;
        Perl6::Value::List.new(
                cstart => coro {
                        my $x = $ret.shift // yield;
                        yield $count++;
                        yield $x;
                },
                celems => sub { $ret.elems + $ret.elems },
        )
    }

    method pairs ( $array: ) { 
        my $ret = $array; 
        my $count = 0;
        Perl6::Value::List.new(
                cstart => coro {
                        my $x = $ret.shift // yield;
                        my $pair = $count => $x;
                        yield $pair;
                        $count++;
                },
                celems => sub { $ret.elems },
        )
    }

    method keys ( $array: ) { 
        my $ret = $array; 
        my $count = 0;
        Perl6::Value::List.new(
                cstart => coro {
                        my $x = $ret.shift // yield;
                        yield $count++; 
                },
                celems => sub { $ret.elems },
        )
    }

    method values ( $array: ) { 
        $array
    }

    method zip ( $array: Array @list ) { 
        # TODO: implement zip parameters
        # TODO: implement count = max( @lists.elems )
        my @lists = ( $array, @list );
        Perl6::Value::List.new(
                cstart => coro {
                        my @x;
                        my $count = 0;
                        # TODO - rewrite this checking 'elems()'
                        # XXX - the list would normally stop after the first 'undef'
                        for @lists -> $xx {
                            push(@x, [$xx.shift]);
                        }
                        if defined any(@x) {
                            for @lists -> $xx {
                                yield shift(@x);
                            }
                        }
                        else {
                            yield;
                        }
                }
        )
    }

}  # end class Perl6::Value::List

multi *shift ( Perl6::Value::List $l ) is export { $l.start if $l.elems }
multi *pop   ( Perl6::Value::List $l ) is export { $l.end   if $l.elems }  

=kwid

= NAME

Perl6::Value::List - implementation of Perl6 'List' class in Perl6

= SYNOPSIS

  my $list = Perl6::Value::List.from_range( start => 10, end => 20 );

  my $list = Perl6::Value::List.new( start => coro mylist2 { yield $_ for 1..3; yield; } );

= DESCRIPTION

A lazy list created from coroutines or subs.

= CONSTRUCTORS

- `new( ... )`

= METHODS

- `shift`

- `pop`

- `reverse`

= AUTHOR

Flavio S. Glock, <fglock@gmail.com>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
