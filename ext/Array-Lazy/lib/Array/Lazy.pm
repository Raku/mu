use v6;

=for ChangeLog

2005-08-07
* Lazy Range supports 'a'..Inf
* New lazy array methods: splat(), elems(), uniq(), end(), 
  next(), previous(), kv(), keys(), values(), zip()
* Code rewritten to use coroutines instead of states
* New class Lazy::CoroList - takes a coroutine (or two, 
  for shift and pop support) and creates a Lazy List, that
  can be used to create a Lazy Array.
* Removed internal class Lazy::Generator

2005-08-06
* new lazy methods reverse(), grep(), map()
* Array::Lazy supports multi-dimensional Lazy Array
* new class 'Lazy::List'
* lazy List / Array of junctions is tested
* removed 'Iter' class; renamed 'Iter::Range' to 'Lazy::Range'

=cut

use Iter::Range;  # 'Lazy::List', 'Lazy::Range', 'Perl6::Array'

# This class should not inherit from Lazy::List, because this would
# cause multi-dimensional arrays to be flattened.

class Array::Lazy-0.01
{

has Array @.items;
   
    # TODO - preprocessing, indexing, etc - lazily
    # TODO - error messages on whatever parameters are illegal
    # TODO - zip(), slice()
    # TODO - change to normal Array if all elements are known
    # TODO - pushing a lazy list into a normal array should turn it into a lazy array
    # TODO - fix namespaces, move to Prelude.pm

    # operators from 'List.hs':
    # op0Zip, op1Pick, op1Sum,
    # op1Min, op1Max, op1Uniq,
    # op2FoldL, op2Fold, op2Grep, op2Map, op2Join,
    # sortByM,
    # op1HyperPrefix, op1HyperPostfix, op2Hyper,
    
    # error message from PIR.pm
    #   if $off > $size {
    #      warn "splice() offset past end of array\n";
    #      $off = $size;
    #   }

method new ( Array::Lazy $Class: *@items ) { 
    # say "new: ", @items;
    return $Class.new( items => @items ); 
}

method _shift_n ($array: Int $length ) returns List {
    my @ret;
    my @tmp = $array.items;
    if $length == Inf {
        $array.items = ();
        return @tmp;
    }    
    while @tmp {
        last if @ret.elems >= $length;
        if ! @tmp[0].isa('Lazy::List') {
            Perl6::Array::push @ret, Perl6::Array::shift @tmp;
            next;
        }
        my $i = @tmp[0].shift;
        if defined $i {
            Perl6::Array::push @ret, $i;
            last if @ret.elems >= $length;
        }
        else {
            Perl6::Array::shift @tmp;
        }
    };
    $array.items = @tmp;    
    return @ret;
}

method _pop_n ($array: Int $length ) returns List {
    my @ret;
    my @tmp = $array.items;
    if $length == Inf {
        $array.items = ();
        return @tmp;
    }    
    while @tmp {
        last if @ret.elems >= $length;
        if ! @tmp[-1].isa('Lazy::List') {
            Perl6::Array::unshift @ret, Perl6::Array::pop @tmp;
            next;
        }
        my $i = @tmp[-1].pop;
        if defined $i {
            Perl6::Array::unshift @ret, $i;
            last if @ret.elems >= $length;
        }
        else {
            Perl6::Array::pop @tmp;
        }
    };
    $array.items = @tmp;    
    return @ret;
}

method elems ( Array::Lazy $array: ) {
    
    # XXX - does elems() force non-lazy context?
    
    #my @tmp = $array.items;
    #say @tmp[0].ref;
    
    # XXX - this returns the wrong result (it doesn't recognize inheritance)
    # return Inf if any( @tmp ).isa( 'Lazy::List' );
    
    for $array.items { 
        return Inf if $_.isa('Lazy::List');
    }
    
    return $.items.elems;
}  

method splice ( 
    Array::Lazy $array: 
    ?$offset = 0, 
    ?$length = Inf, 
    *@list 
)
    returns Array::Lazy 
{ 
    my ( @head, @body, @tail );
    # say "items: ", $array.items, " splice: $offset, $length, ", @list;
    if $offset >= 0 {
        @head = $array._shift_n( $offset );
        if $length >= 0 {
            @body = $array._shift_n( $length ); 
            @tail = $array._shift_n( Inf ); 
        }
        else {
            @tail = $array._pop_n( -$length ); 
            @body = $array._shift_n( Inf ); 
        }
    }
    else {
        @tail = $array._pop_n( -$offset );
        @head = $array._shift_n( Inf );
        if $length >= 0 {
            # make $#body = $length
            while @tail {
                last if @body.elems >= $length;
                Perl6::Array::push @body, Perl6::Array::shift @tail;
            }
        }
        else {
            # make $#tail = -$length
            while @tail {
                last if @tail.elems <= -$length;
                Perl6::Array::push @body, Perl6::Array::shift @tail;
            }
        }
    };
    # say 'head: ',@head, ' body: ',@body, ' tail: ',@tail, ' list: ',@list;
    $array.items = ( @head, @list, @tail );
    return Array::Lazy.new( @body );
}

method shift ( Array::Lazy $array: ) {
    $array._shift_n( 1 )[0]
}

method pop ( Array::Lazy $array: ) {
    $array._pop_n( 1 )[0] 
}

method unshift ( Array::Lazy $array: *@item ) {
    Perl6::Array::unshift $array.items, @item;
    return $array; # XXX ?
}

method push ( Array::Lazy $array: *@item ) {
    Perl6::Array::push $array.items, @item;
    return $array; # XXX ?
}

method end  ( Array::Lazy $array: ) {
    my @x = $array.pop;
    $array.push( @x[0] ) if @x;
    return @x[0];
}

# XXX - if you use this, you get: "*** cannot next() outside a loop"
method next     ( Array::Lazy $array: ) { $array.shift } 
method previous ( Array::Lazy $array: ) { $array.pop }    # just in case 

method reverse ( Array::Lazy $array: ) { 
    my $rev = Perl6::Array::reverse $array.items;
    $rev = $rev.Perl6::Array::map:{
            $_.isa('Lazy::List') ?? $_.reverse :: $_
        };
    return Array::Lazy.new( @{$rev} );
}

method grep ( Array::Lazy $array: Code $code ) { 
    my $ret = $array; 
    return Array::Lazy.new(
        Lazy::CoroList.new(
            start => coro {
                    my $x = $ret.shift // yield;
                    yield $x if &$code($x) 
            },
            end => coro { 
                    my $x = $ret.pop // yield;
                    yield $x if &$code($x) 
            },
        )
    );
}

method map ( Array::Lazy $array: Code $code ) { 
    my $ret = $array; 
    return Array::Lazy.new( 
        Lazy::CoroList.new(
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
        )
    )
}

method splat ( Array::Lazy $array: ) { 
    my $ret = $array; 
    return Lazy::CoroList.new(
            start => coro {
                yield $ret.shift 
            },
            end => coro {
                yield $ret.pop 
            },
        )
}

method uniq ( Array::Lazy $array: ) { 
    my %seen = ();
    my $ret = $array; 
    return Array::Lazy.new( 
        Lazy::CoroList.new(
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
        )
    )
}

method kv ( Array::Lazy $array: ) { 
    my $ret = $array; 
    my $count = 0;
    return Lazy::CoroList.new(
            start => coro {
                    my $x = $ret.shift // yield;
                    yield $count++;
                    yield $x;
            }
    )
}

method pairs ( Array::Lazy $array: ) { 
    my $ret = $array; 
    my $count = 0;
    return Lazy::CoroList.new(
            start => coro {
                    my $x = $ret.shift // yield;
                    my $pair = $count => $x;
                    yield $pair;
                    $count++;
            }
    )
}

method keys ( Array::Lazy $array: ) { 
    my $ret = $array; 
    my $count = 0;
    return Lazy::CoroList.new(
            start => coro {
                    my $x = $ret.shift // yield;
                    yield $count++; 
            }
    )
}

method values ( Array::Lazy $array: ) { 
    my $ret = $array; 
    my $count = 0;
    return Lazy::CoroList.new(
            start => coro {
                    my $x = $ret.shift // yield;
                    yield $x;
            }
    )
}

method zip ( Array::Lazy $array: *@list ) { 
    # TODO: implement zip parameters
    my @lists = ( $array, @list );
    return Lazy::CoroList.new(
            start => coro {
                    my @x;
                    my $count = 0;
                    # TODO - this never ends
                    for @lists -> $xx {
                        @x = $xx.shift;
                        yield @x[0];
                    }
            }
    )
}

# XXX
# sub prefix:<~> () is export {

} # end class Lazy::Array

=kwid

= NAME

Array::Lazy - A Lazy Array

= SYNOPSIS

...


= DESCRIPTION

...

Example:

  1,2,3,-10..20,50,$obj,$span,1..Inf,10

= CONSTRUCTORS

- `new( @list )`

@list may contain "Lazy" lists.

= METHODS

- `splice( ... )`

...

- `push( @list )`

@list may contain "Lazy" lists.

- `pop`

- `unshift( @list )`

@list may contain "Lazy" lists.

- `shift`

- `reverse`

- `splat`

Implements the "splat" operation (*@x).
Returns a lazy list containing the lazy array elements.

  # insert a reference to array2 into array1
  $lazy_array.push( $lazy_array_2 );  
  
  # insert each of the array2 elements into array1
  $lazy_array.push( $lazy_array_2.list );  

- `uniq`

- `elems`

- `grep`

- `map`

- `kv`, `keys`, `values`

- `zip`

= AUTHOR

Flavio S. Glock, <fglock@gmail.com>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
