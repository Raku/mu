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
    $class.new( items => [@items] ); 
}

method _shift_n (Int $length ) returns List {
    my @ret;
    my @tmp = @.items;
    if $length == Inf {
        @.items = ();
        return @tmp;
    }    
    while @tmp {
        last if @ret >= $length;
        unless @tmp[0].isa(Perl6::Value::List) {
            &*push( @ret: @tmp.shift );
            next;
        }
        my $i = @tmp[0].shift;
        if defined $i {
            &*push( @ret: $i);
            last if @ret >= $length;
        }
        elsif @tmp.end > 0 {
            &*shift( @tmp );
        }
        else {
            @tmp = ();
        }
    };
    @.items = @tmp;
    return @ret;
}

method _pop_n (Int $length ) returns List {
    my @ret;
    my @tmp = @.items;
    if $length == Inf {
        @.items = ();
        return @tmp;
    }    
    while @tmp {
        last if @ret >= $length;
        unless @tmp[-1].isa(Perl6::Value::List) {
            &*unshift(@ret: @tmp.pop);
            next;
        }
        my $i = @tmp[-1].pop;
        if defined $i {
            &*unshift(@ret: $i);
            last if @ret >= $length;
        }
        elsif @tmp.end > 0 {
            &*pop(@tmp);
        }
        else {
            @tmp = ();
        }
    };
    @.items = @tmp;    
    return @ret;
}

method elems () {
    [+] &*map({
        $_.isa( 'Perl6::Value::List' )
            ?? $_.Perl6::Value::List::elems
            !! 1;
    }, @.items);
}  

method is_infinite () {
    [||] @.items.map:{
        ($_.isa( 'Perl6::Value::List' ) && $_.is_infinite);
    }, Bool::False;
}

method is_lazy () {
    [||] @.items.map:{
        ($_.isa( 'Perl6::Value::List' ) && $_.is_lazy);
    }, Bool::False;
}

method flatten () { 
    # this needs optimization
    for @.items {
        $_ .= flatten if (.isa( 'Perl6::Value::List' ) && .is_lazy);
    }
}

method splice ( 
    $offset? = 0, 
    $length? = Inf, 
    *@list 
)
    returns Perl6::Container::Array 
{ 
    my ( @head, @body, @tail );
    # say "items: ", @.items, " splice: $offset, $length, ", @list;
    # say 'insert: ', $_, ' ', $_.ref for @list;
    if $offset >= 0 {
        @head = self._shift_n( $offset );
        if $length >= 0 {
            @body = self._shift_n( $length ); 
            @tail = self._shift_n( Inf ); 
        }
        else {
            @tail = self._pop_n( -$length ); 
            @body = self._shift_n( Inf ); 
        }
    }
    else {
        @tail = self._pop_n( -$offset );
        @head = self._shift_n( Inf );
        if $length >= 0 {
            # make $#body = $length
            while @tail {
                last if @body >= $length;
                &*push(@body, &*shift(@tail));
            }
        }
        else {
            # make $#tail = -$length
            while @tail {
                last if @tail <= -$length;
                &*push(@body, &*shift(@tail));
            }
        }
    };
    #say 'head: ',@head, ' body: ',@body, ' tail: ',@tail, ' list: ',@list;
    @.items = ( @head, @list, @tail );
    return Perl6::Container::Array.from_list( @body );
}

method shift () {
    self._shift_n( 1 )[0]
}

method pop () {
    self._pop_n( 1 )[0] 
}

method unshift ( *@item ) {
    &*unshift(@.items: @item);
    return self;
}

method push ( *@item ) {
    &*push(@.items: @item);
    return self;
}

method end () {
    my @x = self.pop;
    self.push( @x[0] ) if @x;
    return @x[0];
}

method fetch ( Int $pos ) {
    # XXX - this is very inefficient
    # see also: slice()
    my $ret = self.splice( $pos, 1 );
    self.splice( $pos, 0, [$ret.items] ); 
    return $ret.items;
}

method store ( Int $pos, $item ) {
    # TODO - $pos could be a lazy list of pairs!
    self.splice( $pos, 1, [$item] );
    return self;
}

# XXX - if you use this, you get: "cannot next() outside a loop"
# method next     () { $array.shift } 
# method previous () { $array.pop }    # just in case 

method reverse () { 
    return Perl6::Container::Array.from_list(
        &*reverse(@.items).map:{
            .isa(Perl6::Value::List) ?? .Perl6::Value::List::reverse !! $_
        }
    );
}

method to_list () { 
    my $ret = self.clone;
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
