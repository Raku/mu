use v6;

# Perl6::Value::List - implementation of Perl6 'List' class in Perl6

# XXX - this is temporary
# this namespace is from 'S29'
    our &Perl6::Array::pop     := &pop;
    our &Perl6::Array::push    := &push;
    our &Perl6::Array::shift   := &shift;
    our &Perl6::Array::unshift := &unshift;
    our &Perl6::Array::reverse := &reverse;
    our &Perl6::Array::map     := &map;
    our &Perl6::Array::grep    := &grep;
# ---------

    # TODO - finish sync with Perl5 version
    # TODO - emit error message if attempting to instantiate an infinite list
    # TODO - does zip() has additional parameters?
    # TODO - document unsupported operations: join, reduce, sort - call fail()
    # TODO - check grep() syntax
    # TODO - keys/kv/pairs/values with indexes - S29

class Perl6::Value::List {
    has Code $.cstart;
    has Code $.cend;
    has Code $.celems;
    has Code $.cis_infinite;

    submethod BUILD ($class: 
            Code ?$start, 
            Code ?$end, 
            Code ?$elems,
            Code ?$is_infinite ) 
    {
        if defined $elems {
            $.celems = $elems
        }
        else {
            $.celems =
                ( defined $start || defined $end ) ?? sub { Inf } :: sub { 0 }
        }
        $.cstart =       $start       // sub { };
        $.cend =         $end         // sub { };
        $.cis_infinite = $is_infinite // sub { &{$.celems}() == Inf };    
    }

    method shift       ( $self: ) { &{$.cstart}() if &{$.celems}() }
    method pop         ( $self: ) { &{$.cend}()   if &{$.celems}() }  
    method elems       ( $self: ) { &{$self.celems}() }
    method is_infinite ( $self: ) { &{$self.cis_infinite}() }

    method reverse     ( $array: ) { 
        my $ret = $array;
        Perl6::Value::List.new( 
                start =>       $ret.cend,
                end =>         $ret.cstart,
                elems =>       $ret.celems,
                is_infinite => $ret.cis_infinite,
        );
    }

    method from_range ( $class: $start is copy, $end is copy, ?$step ) {
        Perl6::Value::List.new(
                    start =>  sub {
                                my $r = $start;
                                if ( defined $step ) { $start += $step } else { $start++ };
                                return $r;
                            },
                    end =>    sub {
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
                    elems =>  sub {
                                return $end - $start + 1 unless defined $step;
                                return int(( $end - $start + 1 ) / $step);
                            },
                    is_infinite => sub { return $start == -Inf || $end == Inf },
        );
    }

    method grep ( $array: Code $code ) { 
        my $ret = $array; 
        Perl6::Value::List.new(
                start => coro {
                        my $x = $ret.shift // yield;
                        yield $x if &$code($x) 
                },
                end => coro { 
                        my $x = $ret.pop // yield;
                        yield $x if &$code($x) 
                },
                # TODO - signal end of data using 'elems()'
        );
    }

    method map ( $array: Code $code ) { 
        my $ret = $array; 
        Perl6::Value::List.new(
                start => coro {
                        my @ret;
                        my $x = $ret.shift // yield;
                        Perl6::Array::unshift @ret,&$code($x); 
                        yield Perl6::Array::shift @ret while @ret 
                },
                end => coro {
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
                start => coro {
                        my $x = $ret.shift // yield;
                        unless %seen{$x} { 
                            %seen{$x} = bool::true; 
                            yield $x 
                        }                       
                },
                end => coro {
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
                start => coro {
                        my $x = $ret.shift // yield;
                        yield $count++;
                        yield $x;
                },
                elems => sub { $ret.elems + $ret.elems },
        )
    }

    method pairs ( $array: ) { 
        my $ret = $array; 
        my $count = 0;
        Perl6::Value::List.new(
                start => coro {
                        my $x = $ret.shift // yield;
                        my $pair = $count => $x;
                        yield $pair;
                        $count++;
                },
                elems => sub { $ret.elems },
        )
    }

    method keys ( $array: ) { 
        my $ret = $array; 
        my $count = 0;
        Perl6::Value::List.new(
                start => coro {
                        my $x = $ret.shift // yield;
                        yield $count++; 
                },
                elems => sub { $ret.elems },
        )
    }

    method values ( $array: ) { 
        $array
    }

    method zip ( $array: *@list ) { 
        # TODO: implement zip parameters
        # TODO: implement count = max( @lists.elems )
        my @lists = ( $array, @list );
        Perl6::Value::List.new(
                start => coro {
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
