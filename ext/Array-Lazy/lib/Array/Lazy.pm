use v6;

class Array::Lazy-0.01
    does Iter;

    has Array @.items;
    # has $.has_spans;
    # TODO - preprocessing, indexing, etc - lazily
    # TODO - error messages on whatever is illegal
    # TODO - find out how to use 'splice' instead of '_splice'

submethod BUILD ( *@.items ) {}

method new ( Array::Lazy $Class: @items ) { 
    # say "new: ", @items;
    return $Class.new( items => @items ); 
}

method _splice ( $array: $offset is copy, $length, *@list ) returns Array::Lazy 
{ 
    my ( @head, @body, @tail );

    # say @.items, ".splice: $offset, $length, ", @list;

    if -Inf == $offset { $offset = 0 }
    # if $offset==-Inf { $offset = 0 } -- syntax error ???

    if $offset >= 0
    {
        # get $offset items
        if $offset == Inf
        {
            @head = @.items;
        }
        else
        {
            while @.items
            {
                last if @head.elems >= $offset;
                if @.items[0].isa('Iter') {
                    loop {
                        my $i = @.items[0].shift;
                        unless defined $i {
                            shift @head;
                            last;
                        }
                        push @head, $i;
                        last if @head.elems >= $offset;
                    };
                }
                else
                {
                    my @tmp = @.items;
                    push @head, shift @tmp;
                    @.items = @tmp;    
                    # XXX "Can't modify constant item: VUndef"
                    #   push @head, shift @.items;
                };
            };
            if $length == Inf
            {
                @body = @.items;
            }
            else
            {
                while @.items
                {
                    last if @body.elems >= $length;
                    if @.items[0].isa('Iter') {
                        loop {
                            my $i = @.items[0].shift;
                            unless defined $i {
                                shift @body;
                                last;
                            }
                            push @body, $i;
                            last if @body.elems >= $length;
                        };
                    }
                    else
                    {
                        push @body, shift @.items;
                    };
                };
                @tail = @.items;
            }
        };
    }
    else
    {
        # get offset items, backwards
        @head = @.items;
        $offset = - $offset;
        while @head 
        {
            last if @tail.elems >= $offset;
            my $item = @head[-1];
            if @head[-1].isa('Iter') {
                loop {
                    my $i = @head[-1].pop;
                    unless defined $i {
                        pop @head;
                        last;
                    }
                    unshift @tail, $i;
                    last if @tail.elems >= $offset;
                };
            }
            else
            {
                unshift @tail, pop @head;
            };
        };
        while @tail {
            last if @body.elems >= $length;
            push @body, shift @tail;
        }
    };

    # say 'head: ',@head;
    # say 'body: ',@body;
    # say 'tail: ',@tail;

    @.items = ( @head, @list, @tail );
    # say "items: ", @.items;
    # say 'body: ',@body;
    my $result = Array::Lazy.new( @body );

    # say @.spans, " spliced ", @spliced;

    return $result;
}

method _shift {
    ...
}

method _pop {
    ...
}

method stringify {
    return @.items.join(',');
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

= CONSTRUCTORS

- `new( @list )`

@list may contain iterator "Iter" objects.

= MUTATOR METHODS

- `_splice( ... )`

...

= AUTHOR

Flavio S. Glock, <fglock@gmail.com>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
