use v6;

=for ChangeLog

2005-08-10
* New methods Array.flatten(), is_lazy()
  - Lazy Array changes to normal Array if all elements are known;
  - pushing a lazy list into a normal array turns the array into a lazy array
* New method: Array.is_infinite()
* Renamed methods: Array.new() -> Array.from_list(); Array.splat() -> Array.to_list
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

# TODO - test splice() with parameter 'Array @list'
# TODO - add lazy/strict tests

# TODO - 'dim'
# TODO - sync with Perl5 version of Array

# TODO - exists(), delete() - S29

# TODO - preprocessing, indexing, etc - lazily
    
# TODO - add support for sparse arrays
#      - what happens when $a.FETCH(100000000)

# implement this error message (from PIR.pm)
#   if $off > $size {
#      warn "splice() offset past end of array\n";
#      $off = $size;
#   }

# Things that will be solved by the compiler:
# - fetch slice / store slice - @a[5,7]=('x','y')
# - keys/kv/pairs/values with indexes (S29)

use Perl6::Value::List; 

# This class should not inherit from Perl6::Value::List, because this would
# cause multi-dimensional arrays to be flattened.

class Perl6::Container::Array-0.01
{
    has Array @.items;

method from_list ( $class: *@items ) { 
    $class.new( items => @items ); 
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
            &*push( @ret, Perl6::Array::shift @tmp );
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

method elems ( $array: ) {
    my $count = 0;
    for $.items {
        $count += $_.isa( 'Perl6::Value::List' )  ?? 
                  $_.Perl6::Value::List::elems  !! 
                  1;
    }
    $count;
}  

method is_infinite ( $array: ) {
    for $.items {
        return bool::true if $_.isa( 'Perl6::Value::List' ) && $_.is_infinite;
    }
    bool::false;
}  

method is_lazy ( $array: ) {
    for $.items {
        return bool::true if $_.isa( 'Perl6::Value::List' ) && $_.is_lazy;
    }
    bool::false;
}  

method flatten ( $self: ) { 
    # this needs optimization
    my $ret = $array;
    for $ret.items {
        $_ = $_.flatten() if $_.isa( 'Perl6::Value::List' ) && $_.is_lazy;
    }
    $ret;
}

method splice ( 
    Perl6::Container::Array $array: 
    $offset? = 0, 
    $length? = Inf, 
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
    return Perl6::Container::Array.from_list( @body );
}

method shift ( $array: ) {
    $array._shift_n( 1 )[0]
}

method pop ( $array: ) {
    $array._pop_n( 1 )[0] 
}

method unshift ( $array: *@item ) {
    Perl6::Array::unshift $array.items, @item;
    return $array; 
}

method push ( $array: *@item ) {
    Perl6::Array::push $array.items, @item;
    return $array; 
}

method end  ( $array: ) {
    my @x = $array.pop;
    $array.push( @x[0] ) if @x;
    return @x[0];
}

method fetch ( $array: Int $pos ) {
    # XXX - this is very inefficient
    # see also: slice()
    my $ret = $array.splice( $pos, 1 );
    $array.splice( $pos, 0, [$ret.items] ); 
    return $ret.items;
}

method store ( $array: Int $pos, $item ) {
    # TODO - $pos could be a lazy list of pairs!
    $array.splice( $pos, 1, [$item] );
    return $array; 
}

# XXX - if you use this, you get: "cannot next() outside a loop"
# method next     ( Perl6::Container::Array $array: ) { $array.shift } 
# method previous ( Perl6::Container::Array $array: ) { $array.pop }    # just in case 

method reverse ( $array: ) { 
    my $rev = Perl6::Array::reverse $array.items;
    $rev = $rev.Perl6::Array::map:{
            $_.isa('Perl6::Value::List') ?? $_.Perl6::Value::List::reverse !! $_
        };
    return Perl6::Container::Array.from_list( @{$rev} );
}

method to_list ( $array: ) { 
    my $ret = $array.clone; 
    # TODO - optimization - return the internal list object, if there is one
    return Perl6::Value::List.new(
            cstart => sub { $ret.shift },
            cend =>   sub { $ret.pop },
            celems => sub { $ret.elems },
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

- `from_list( @list )`

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

- `to_list`

Implements the "splat" operation (*@x).
Returns a lazy list containing the lazy array elements.

  # insert a reference to array2 into array1
  $lazy_array.push( $lazy_array_2 );  
  
  # insert each of the array2 elements into array1
  $lazy_array.push( $lazy_array_2.list );  

- `fetch` / `store`

= AUTHOR

Flavio S. Glock, <fglock@gmail.com>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
