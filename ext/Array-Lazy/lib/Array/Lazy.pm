use v6;

=for ChangeLog

2005-08-10
* New List methods: is_contiguous(), to_str(), clone(), to_ref(), to_bit(), to_num()
* New List hooks: is_contiguous( $code ), stringify( $code )
* Started syncing with Perl5 version
* Removed class Lazy::Range
* Lowercase methods fetch(), store()
* Removed redundant methods next(), previous()

2005-08-09
* Renamed Lazy::List to Perl6::Value::List
* Renamed Array::Lazy to Perl6::Container::Array
* Removed fetch_slice() because it is non-standard.

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

use Iter::Range;  # 'Perl6::Value::List', 'Perl6::Array'

# This class should not inherit from Perl6::Value::List, because this would
# cause multi-dimensional arrays to be flattened.

class Perl6::Container::Array-0.01
{

has Array @.items;
   
    # TODO - separate modules "List" and "Array"
    # TODO - sync Perl5/Perl6 versions, create Perl5 version of Array
    # TODO - move files to the right directories, perl module name

    # TODO - exists(), delete() - S29
    # TODO - keys/kv/pairs/values with indexes - S29

    # TODO - preprocessing, indexing, etc - lazily
    # TODO - change Lazy Array to normal Array if all elements are known;
    # TODO - pushing a lazy list into a normal array should turn the array into a lazy array
    # TODO - fix namespaces, move to Prelude.pm
    
    # TODO - fetch_slice() / store_slice() - @a[5,7]=('x','y')

    # TODO - what happens when $a.FETCH(100000000)
    #      - this is not a 'sparse' array!

    # TODO - add support for sparse arrays

    # implement this error message (from PIR.pm)
    #   if $off > $size {
    #      warn "splice() offset past end of array\n";
    #      $off = $size;
    #   }

method new ( Perl6::Container::Array $Class: *@items ) { 
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
        if ! @tmp[0].isa('Perl6::Value::List') {
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
        if ! @tmp[-1].isa('Perl6::Value::List') {
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

method elems ( Perl6::Container::Array $array: ) {
    
    # XXX - doesn't compile
    # [+] .elems<< $.items;

    my $count = 0;
    for $.items {
        # XXX - inheritance bug workaround
        $count += $_.isa( 'Lazy::Range' ) ?? $_.Lazy::Range::elems :: 
                  $_.isa( 'Perl6::Value::List' )  ?? $_.Perl6::Value::List::elems  :: 
                  1;
    }
    $count;
}  

method splice ( 
    Perl6::Container::Array $array: 
    ?$offset = 0, 
    ?$length = Inf, 
    *@list 
)
    returns Perl6::Container::Array 
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
    return Perl6::Container::Array.new( @body );
}

method shift ( Perl6::Container::Array $array: ) {
    $array._shift_n( 1 )[0]
}

method pop ( Perl6::Container::Array $array: ) {
    $array._pop_n( 1 )[0] 
}

method unshift ( Perl6::Container::Array $array: *@item ) {
    Perl6::Array::unshift $array.items, @item;
    return $array; 
}

method push ( Perl6::Container::Array $array: *@item ) {
    Perl6::Array::push $array.items, @item;
    return $array; 
}

method end  ( Perl6::Container::Array $array: ) {
    my @x = $array.pop;
    $array.push( @x[0] ) if @x;
    return @x[0];
}

method fetch ( Perl6::Container::Array $array: Int $pos ) {
    # XXX - this is very inefficient
    # see also: slice()
    my $ret = $array.splice( $pos, 1 );
    $array.splice( $pos, 0, [$ret.items] ); 
    return $ret.items;
}

method store ( Perl6::Container::Array $array: Int $pos, $item ) {
    # TODO - $pos could be a lazy list of pairs!
    $array.splice( $pos, 1, [$item] );
    return $array; 
}

# XXX - if you use this, you get: "cannot next() outside a loop"
# method next     ( Perl6::Container::Array $array: ) { $array.shift } 
# method previous ( Perl6::Container::Array $array: ) { $array.pop }    # just in case 

method reverse ( Perl6::Container::Array $array: ) { 
    my $rev = Perl6::Array::reverse $array.items;
    $rev = $rev.Perl6::Array::map:{
            $_.isa('Perl6::Value::List') ?? $_.Perl6::Value::List::reverse :: $_
        };
    return Perl6::Container::Array.new( @{$rev} );
}

method splat ( Perl6::Container::Array $array: ) { 
    my $ret = $array; 
    # TODO - optimization - return the internal list object, if there is one
    return Perl6::Value::List.new(
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

Perl6::Container::Array - A Lazy Array

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
