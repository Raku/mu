use v6;

=for ChangeLog

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
    # TODO - parameters to splice should be optional
    # TODO - uniq(), splat(), zip(), slice()
    # TODO - change to normal Array if all elements are known
    # TODO - fix namespaces, move to Prelude.pm

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
            while @tail {
                last if @body.elems >= $length;
                Perl6::Array::push @body, Perl6::Array::shift @tail;
            }
        }
        else {
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

method unshift ( Array::Lazy $array: @item ) {
    Perl6::Array::unshift $array.items, @item;
    return $array; # XXX ?
}

method push ( Array::Lazy $array: @item ) {
    Perl6::Array::push $array.items, @item;
    return $array; # XXX ?
}

method reverse ( Array::Lazy $array: ) { 
    my $rev = Perl6::Array::reverse $array.items;
    $rev = $rev.Perl6::Array::map:{
            $_.isa('Lazy::List') ?? $_.reverse :: $_
        };
    return Array::Lazy.new( @{$rev} );
}

method grep ( Array::Lazy $array: Code $code ) { 
    return Array::Lazy.new( 
        Lazy::Generator.new(
            shifter => sub { 
                loop { my $x = $array.shift // return;
                       return $x if &$code($x) } 
            },
            popper =>  sub { 
                loop { my $x = $array.pop // return;
                       return $x if &$code($x) } 
            },
        )
    )
}

method map ( Array::Lazy $array: Code $code ) { 
    return Array::Lazy.new( 
        Lazy::Generator.new(
            shifter => sub { 
                loop { 
                    my $x = $array.shift // return;
                    my @ret = &$code($x); 
                    return @ret if @ret;
                } 
            },
            popper =>  sub { 
                loop { 
                    my $x = $array.pop // return;
                    my @ret = &$code($x); 
                    return @ret if @ret;
                }                                
            },
        )
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

The "elems()" method is not supported.

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

= AUTHOR

Flavio S. Glock, <fglock@gmail.com>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
