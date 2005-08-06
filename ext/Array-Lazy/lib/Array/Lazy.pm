use v6;

use Iter::Range;  # import my 'Iter' class

# this namespace is from 'S29'
  our &Perl6::Array::pop     := &pop;
  our &Perl6::Array::push    := &push;
  our &Perl6::Array::shift   := &shift;
  our &Perl6::Array::unshift := &unshift;

class Array::Lazy-0.01
    is Iter;

has Array @.items;
# has $.has_spans;  # optimization
    
    # TODO - preprocessing, indexing, etc - lazily
    # TODO - error messages on whatever parameters are illegal
    # TODO - optional parameters to splice
    # TODO - reverse

submethod BUILD ( *@.items ) {}

method new ( Array::Lazy $Class: *@items ) { 
    # say "new: ", @items;
    return $Class.new( items => @items ); 
}

method splice ( Array::Lazy $array: 
    $offset is copy, 
    $length, 
    *@list )
        returns Array::Lazy 
        is export
{ 
    my ( @head, @body, @tail );

    #say "items: ", $array.items;
    #say "splice: $offset, $length, ", @list;

    if -Inf == $offset { $offset = 0 }
    # if $offset==-Inf { $offset = 0 } -- syntax error ???

    if $offset >= 0
    {
        # get $offset items
        if $offset == Inf
        {
            @head = $array.items;
        }
        else
        {
            while $array.items
            {
                last if @head.elems >= $offset;
                if $array.items[0].isa('Iter') {
                    loop {
                        my $i = $array.items[0].shift;
                        unless defined $i {
                            Perl6::Array::shift @head;
                            last;
                        }
                        Perl6::Array::push @head, $i;
                        last if @head.elems >= $offset;
                    };
                }
                else
                {
                    my @tmp = $array.items;
                    Perl6::Array::push @head, Perl6::Array::shift @tmp;
                    $array.items = @tmp;    
                    # XXX "Can't modify constant item: VUndef"
                    #   push @head, shift @.items;
                };
            };
            if $length == Inf
            {
                @body = $array.items;
            }
            else
            {
                while $array.items
                {
                    last if @body.elems >= $length;
                    # say 'test Iter: ', @.items[0], ' -- ', @.items[0].isa('Iter');
                    if $array.items[0].isa('Iter') {
                        loop {
                            my $i = $array.items[0].shift;
                            unless defined $i {
                                Perl6::Array::shift @body;
                                last;
                            }
                            Perl6::Array::push @body, $i;
                            last if @body.elems >= $length;
                        };
                    }
                    else
                    {
                        my @tmp = $array.items;
                        Perl6::Array::push @body, Perl6::Array::shift @tmp;
                        $array.items = @tmp;    
                        # XXX error - push @body, shift @.items;
                        # say 'shifted to ', @.items;
                    };
                };
                @tail = $array.items;
            }
        };
    }
    else
    {
        # get offset items, backwards
        @head = $array.items;
        $offset = - $offset;
        while @head 
        {
            last if @tail.elems >= $offset;
            my $item = @head[-1];
            if @head[-1].isa('Iter') {
                loop {
                    my $i = @head[-1].pop;
                    unless defined $i {
                        Perl6::Array::pop @head;
                        last;
                    }
                    Perl6::Array::unshift @tail, $i;
                    last if @tail.elems >= $offset;
                };
            }
            else
            {
                Perl6::Array::unshift @tail, Perl6::Array::pop @head;
            };
        };
        while @tail {
            last if @body.elems >= $length;
            Perl6::Array::push @body, Perl6::Array::shift @tail;
        }
    };

    #say 'head: ',@head;
    #say 'body: ',@body;
    #say 'tail: ',@tail;

    $array.items = ( @head, @list, @tail );
    # say "items: ", $array.items;
    # say 'body: ',@body;
    my $result = Array::Lazy.new( @body );

    # say @.spans, " spliced ", @spliced;

    return $result;
}

method shift ( Array::Lazy $array: ) {
    return Perl6::Array::shift $array 
        unless $array.isa('Array::Lazy');
    my $tmp = $array.splice( 0, 1, () );
    return $tmp.items[0];
}

method pop ( Array::Lazy $array: ) {
    my $tmp = $array.splice( -1, 1, () );
    return $tmp.items[0];
}

method unshift ( Array::Lazy $array: @item ) {
    my $tmp = $array.splice( 0, 0, @item );
    return $array; # XXX ?
}

method push ( Array::Lazy $array: @item ) {
    Perl6::Array::push $array.items, @item;
    return $array; # XXX ?
}

# XXX
# sub prefix:<~> () is export {

method stringify {
    @.items.join(',')
}

=kwid

= NAME

Array::Lazy - An "Array" implemented with iterators

= SYNOPSIS

...


= DESCRIPTION

...

Example:

  1,2,3,-10..20,50,$obj,$span,1..Inf,10

The "elems()" method is not supported.

= CONSTRUCTORS

- `new( @list )`

@list may contain iterator "Iter" objects.

= METHODS

- `splice( ... )`

...

- `push( @list )`

@list may contain iterator "Iter" objects.

- `pop`

- `unshift( @list )`

@list may contain iterator "Iter" objects.

- `shift`

= AUTHOR

Flavio S. Glock, <fglock@gmail.com>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
