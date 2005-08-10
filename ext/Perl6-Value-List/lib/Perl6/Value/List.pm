use v6;

# Perl6::Value::List - implementation of Perl6 'List' class in Perl6

# ChangeLog
#
# 2005-08-10
# * New methods Perl6::Value::List.flatten(), is_lazy()
# * New method Perl6::Value::List.from_coro( $sub )

    # TODO - is_lazy(), flatten()
    # TODO - finish sync with Perl5 version
    # TODO - emit error message if attempting to instantiate an infinite list
    # TODO - does zip() has additional parameters?
    # TODO - document unsupported operations: join, reduce, sort - call fail()
    # TODO - check grep() syntax
    # TODO - keys/kv/pairs/values with indexes - S29
    # TODO - provide a better default stringify - see Span.pm

# XXX - this is temporary
# this namespace is from 'S29'
# TODO - change these to *pop, *push, ...
    our &Perl6::Array::pop     := &pop;
    our &Perl6::Array::push    := &push;
    our &Perl6::Array::shift   := &shift;
    our &Perl6::Array::unshift := &unshift;
    our &Perl6::Array::reverse := &reverse;
    our &Perl6::Array::map     := &map;
    our &Perl6::Array::grep    := &grep;
# ---------

class Perl6::Value::List {
    has Code $.cstart;
    has Code $.cend;
    has Code $.celems;
    has Code $.cis_infinite;
    has Code $.cis_contiguous;
    has Code $.cstringify;
    has Bool $.is_lazy;

    submethod BUILD ($class: 
            Code ?$.cstart, 
            Code ?$.cend, 
            Code ?$.celems,
            Code ?$.cis_infinite   = sub { &{$.celems}() == Inf },
            Code ?$.cis_contiguous = sub { bool::false }, 
            Code ?$.cstringify     = sub { &{$.cstart}() ~ '....' ~ &{$.cend}() }, 
            Bool ?$.is_lazy        = bool::true,
    )
    {
        unless defined $.celems {
            $.celems =
                ( defined $.cstart || defined $.cend ) ?? sub { Inf } :: sub { 0 }
        }
        $.cstart = sub {} unless defined $.cstart;
        $.cend   = sub {} unless defined $.cend;    
    }

    method shift         ( $self: ) { &{$.cstart}() if &{$.celems}() }
    method pop           ( $self: ) { &{$.cend}()   if &{$.celems}() }  
    method elems         ( $self: ) { &{$.celems}() }
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

    method from_range ( $class: $start is copy, $end is copy, ?$step ) {
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
        $class.new( cstart => sub{ Perl6::Array::shift  @list },
                    cend =>   sub{ Perl6::Array::pop    @list },
                    celems => sub{ @list.elems  },
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
                        Perl6::Array::unshift @ret,&$code($x); 
                        yield Perl6::Array::shift @ret while @ret 
                },
                cend => coro {
                        my @ret; 
                        my $x = $ret.pop // yield;
                        Perl6::Array::push @ret, &$code($x); 
                        yield Perl6::Array::pop @ret while @ret  
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
                            Perl6::Array::push @x, [$xx.shift];
                        }
                        if defined any(@x) {
                            for @lists -> $xx {
                                yield Perl6::Array::shift @x;
                            }
                        }
                        else {
                            yield;
                        }
                }
        )
    }

}  # end class Perl6::Value::List

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
