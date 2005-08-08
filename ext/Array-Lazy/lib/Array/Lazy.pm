use v6;

=for ChangeLog

2005-08-08
* New lazy array methods: FETCH(), STORE(), fetch_slice().
  slice accepts a lazy list or lazy array as argument!
* elems() actually works
* more or less fixed Lazy::List::reverse() inheritance
* Lazy::CoroList is gone
* Lazy list methods (grep, map, ...) moved from Array::Lazy to 
  Lazy::List. These methods can be accessed from an array
  by doing a 'splat()' first. 
  Array::Lazy::reverse() is still supported.

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
    # TODO - change Lazy Array to normal Array if all elements are known;
    # TODO - pushing a lazy list into a normal array should turn the array into a lazy array
    # TODO - fix namespaces, move to Prelude.pm
    
    # TODO - store_slice() - @a[5,7]=('x','y')

    # TODO - what happens when $a.FETCH(100000000)
    #      - this is not a 'sparse' array!

    # TODO - add support for sparse arrays

    # implement this error message (from PIR.pm)
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
    
    # XXX - doesn't compile
    # [+] .elems<< $.items;

    my $count = 0;
    for $.items {
        # XXX - inheritance bug workaround
        $count += $_.isa( 'Lazy::Range' ) ?? $_.Lazy::Range::elems :: 
                  $_.isa( 'Lazy::List' )  ?? $_.Lazy::List::elems  :: 
                  1;
    }
    $count;
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
    # say 'insert: ', $_, ' ', $_.ref for @list;
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
    #say 'head: ',@head, ' body: ',@body, ' tail: ',@tail, ' list: ',@list;
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

method FETCH ( Array::Lazy $array: Int $pos ) {
    # XXX - this is very inefficient
    # see also: slice()
    my $ret = $array.splice( $pos, 1 );
    $array.splice( $pos, 0, [$ret.items] ); 
    return $ret.items;
}

method STORE ( Array::Lazy $array: Int $pos, $item ) {
    # TODO - $pos could be a lazy list of pairs!
    $array.splice( $pos, 1, [$item] );
    return $array;  # ?
}

method fetch_slice ( Array::Lazy $array: Array::Lazy|Lazy::List $pos ) {
    my $arr = $array;
    my $where = $pos;
    $where = $where.splat if $where.isa( 'Array::Lazy' ); 
    Array::Lazy.new( $where.Lazy::List::map:{ $arr.FETCH($_) } );
}

method store_slice ( Array::Lazy $array: $pos, $values ) {
    # $pos must be one of:
    # - a Lazy::List with known size
    # - a Lazy::Range
    # - a Array::Lazy containing things with known size, or Lazy::Range
    # - a standard Int, Array or List

    # ---- start the index preprocessing ----

    my @ranges;
    my ( @start, @end, @step, @from );
    my $delta = 0;   # , $neg_delta );
    if $pos.isa( 'Array::Lazy' ) { 
        @ranges = $pos.items 
    }
    else  { 
        @ranges = $pos 
    };
    for @ranges {
        fail 'unable to store elements using index "' ~ $_ ~ '"'
            if   $_.isa( 'Lazy::List' ) &&
               ! $_.isa( 'Lazy::Range' );

        # --- this is less restrictive, but it is much harder to implement
        #   if   $_.isa( 'Lazy::List' ) &&
        #        $_.Lazy::Array::elems == Inf &&
        #      ! $_.isa( 'Lazy::Range' )
        # ---

        if ( $_.isa( 'Lazy::Range' ) ) {
            push @start, $_.start;
            push @end,   $_.end;
            push @step,  $_.step // 1;
            push @from, $delta;
            $delta += $_.elems;
            last if $_.elems == Inf;
        }
        else {
            push @start, $_;
            push @end,   $_;
            push @step,  1;
            push @from, $delta;
            $delta++;
        }
    }

    # ---- end of index preprocessing ----

    my sub index_exists {
        for 0..@end {
            # TODO - add support for 'step'
        }
        return bool::false;
    }

    # TODO - a sparse array would be much more efficient 

    ...

    my $me = $self.clone;
    my $tmp = Array::Lazy.new( $me.Lazy::List::map:{ $arr.FETCH($_) } );

    # $pos = 10..Inf         # easy 
    # $pos = 1..10, 20..Inf  # easy
    # $pos = $lazy_list      # very difficult

    # this would be much easier if $pos was a Hash::Lazy

    # I think it is reasonable to assume that $pos is numerically ordered.

    # does the index exist in $pos?
    # if it does, do the mapping
    # otherwise, keep the original value

    # my $arr =   $array.clone;
    # my $where = $pos.clone;
    # my $what =  $values.clone

    # $where = $where.splat if $where.isa( 'Array::Lazy' ); 
    # Array::Lazy.new( $where.Lazy::List::map:{ $arr.FETCH($_) } );

}

# XXX - if you use this, you get: "*** cannot next() outside a loop"
method next     ( Array::Lazy $array: ) { $array.shift } 
method previous ( Array::Lazy $array: ) { $array.pop }    # just in case 

method reverse ( Array::Lazy $array: ) { 
    my $rev = Perl6::Array::reverse $array.items;
    $rev = $rev.Perl6::Array::map:{
            $_.isa('Lazy::List') ?? $_.Lazy::List::reverse :: $_
        };
    return Array::Lazy.new( @{$rev} );
}

method splat ( Array::Lazy $array: ) { 
    my $ret = $array; 
    return Lazy::List.new(
            start => coro {
                yield $ret.shift 
            },
            end => coro {
                yield $ret.pop 
            },
        )
}

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

- `FETCH` / `STORE`
 
- `fetch_slice`

= AUTHOR

Flavio S. Glock, <fglock@gmail.com>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
