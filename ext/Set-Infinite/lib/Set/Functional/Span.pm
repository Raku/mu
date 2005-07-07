use v6;

class Set::Functional::Span-0.01;

has Object $:start;
has Object $:end;
has Bool   $:start_is_open;
has Bool   $:end_is_open;

=for TODO

    * union
    * intersection
    * contains
        - Set::Infinite uses: ( $a->union( $b ) == $a )
    * stringify
    * spaceship

    * complete POD

From "Set" API (maybe):

    * equal/not_equal
    * difference
    * symmetric_difference
    * proper_subset
    * proper_superset
    * subset
    * superset
    * includes/member/has
    * unicode

=cut

# constructor: a single constructor, without bounds checking

submethod BUILD ( Object $start, Object $end, Bool $start_is_open, Bool $end_is_open ) 
returns Set::Functional::Span 
{
    $:start =         $start;
    $:end =           $end;
    $:start_is_open = $start_is_open;
    $:end_is_open =   $end_is_open;
}

multi method size () returns Object {
    return $:end - $:start;
}
multi method size ( Object $density ) returns Object {
    if $:start_is_open || $:end_is_open 
    {
        return $:end - $:start - $density if $:start_is_open && $:end_is_open;
        return $:end - $:start;
    }
    return $:end - $:start + $density;
}

method start () returns Object {
    return $:start;
}
method end () returns Object {
    return $:end;
}
method start_is_open () returns Bool {
    return $:start_is_open;
}
method start_is_closed () returns Bool {
    return ! $:start_is_open;
}
method end_is_open () returns Bool {
    return $:end_is_open;
}
method end_is_closed () returns Bool {
    return ! $:end_is_open;
}

method intersects ( Set::Functional::Span $span ) returns Bool {
    my ($i_beg, $i_end, $open_beg, $open_end);
    my $cmp = $.start <=> $span.start;
    if ($cmp < 0) {
        $i_beg       = $span.start;
        $open_beg    = $span.start_is_open;
    }
    elsif ($cmp > 0) {
        $i_beg       = $.start;
        $open_beg    = $.start_is_open;
    }
    else {
        $i_beg       = $.start;
        $open_beg    = $.start_is_open || $span.start_is_open;
    }
    $cmp = $.end <=> $span.end;
    if ($cmp > 0) {
        $i_end       = $span.end;
        $open_end    = $span.end_is_open;
    }
    elsif ($cmp < 0) {
        $i_end       = $.end;
        $open_end    = $.end_is_open;
    }
    else {
        $i_end       = $.end;
        $open_end    = $.end_is_open || $span.end_is_open;
    }
    $cmp = $i_beg <=> $i_end;
    return $cmp <= 0  &&
           ( $cmp != 0  ||  ( ! $open_beg && ! $open_end ) );
}

method complement () returns List of Set::Functional::Span 
{
    if ($.end == Inf) {
        return if $.start == -Inf;
        return .new( start => -Inf,
                     end =>   $.start,
                     start_is_open => bool::true,
                     end_is_open => ! $.start_is_open );
    }
    if ($.start == -Inf) {
        return .new( start => $.end,
                     end =>   Inf,
                     start_is_open => ! $.end_is_open,
                     end_is_open => bool::true );
    }
    return (   .new( start => -Inf,
                     end =>   $.start,
                     start_is_open => bool::true,
                     end_is_open => ! $.start_is_open ),
               .new( start => $.end,
                     end =>   Inf,
                     start_is_open => ! $.end_is_open,
                     end_is_open => bool::true ) );
}

multi method union ( Set::Functional::Span $span ) returns List of Set::Functional::Span {
    ...

=for TODO
    my $cmp;
    $cmp = $tmp1->{b} <=> $tmp2->{a};
    if ( $cmp < 0 ||
             ( $cmp == 0 && $tmp1->{open_end} && $tmp2->{open_begin} ) ) {
        return ( $tmp1, $tmp2 );
    }
    $cmp = $tmp1->{a} <=> $tmp2->{b};
    if ( $cmp > 0 ||
         ( $cmp == 0 && $tmp2->{open_end} && $tmp1->{open_begin} ) ) {
        return ( $tmp2, $tmp1 );
    }
    #-- common code
    my $tmp;
    $cmp = $tmp1->{a} <=> $tmp2->{a};
    if ($cmp > 0) {
        $tmp->{a} = $tmp2->{a};
        $tmp->{open_begin} = $tmp2->{open_begin};
    }
    elsif ($cmp == 0) {
        $tmp->{a} = $tmp1->{a};
        $tmp->{open_begin} = $tmp1->{open_begin} ? $tmp2->{open_begin} : 0;
    }
    else {
        $tmp->{a} = $tmp1->{a};
        $tmp->{open_begin} = $tmp1->{open_begin};
    }

    $cmp = $tmp1->{b} <=> $tmp2->{b};
    if ($cmp < 0) {
        $tmp->{b} = $tmp2->{b};
        $tmp->{open_end} = $tmp2->{open_end};
    }
    elsif ($cmp == 0) {
        $tmp->{b} = $tmp1->{b};
        $tmp->{open_end} = $tmp1->{open_end} ? $tmp2->{open_end} : 0;
    }
    else {
        $tmp->{b} = $tmp1->{b};
        $tmp->{open_end} = $tmp1->{open_end};
    }
    return $tmp;
=cut

}
multi method union ( Set::Functional::Span $span, Object $density ) returns List of Set::Functional::Span {
    ...

=for TODO
    my $cmp;
        my $a1_open =  $tmp1->{open_begin} ? -$tolerance : $tolerance ;
        my $b1_open =  $tmp1->{open_end}   ? -$tolerance : $tolerance ;
        my $a2_open =  $tmp2->{open_begin} ? -$tolerance : $tolerance ;
        my $b2_open =  $tmp2->{open_end}   ? -$tolerance : $tolerance ;
        # open_end touching?
        if ((($tmp1->{b}+$tmp1->{b}) + $b1_open ) <
            (($tmp2->{a}+$tmp2->{a}) - $a2_open)) {
            # self disjuncts b
            return ( $tmp1, $tmp2 );
        }
        if ((($tmp1->{a}+$tmp1->{a}) - $a1_open ) >
            (($tmp2->{b}+$tmp2->{b}) + $b2_open)) {
            # self disjuncts b
            return ( $tmp2, $tmp1 );
        }
    #-- common code
    my $tmp;
    $cmp = $tmp1->{a} <=> $tmp2->{a};
    if ($cmp > 0) {
        $tmp->{a} = $tmp2->{a};
        $tmp->{open_begin} = $tmp2->{open_begin};
    }
    elsif ($cmp == 0) {
        $tmp->{a} = $tmp1->{a};
        $tmp->{open_begin} = $tmp1->{open_begin} ? $tmp2->{open_begin} : 0;
    }
    else {
        $tmp->{a} = $tmp1->{a};
        $tmp->{open_begin} = $tmp1->{open_begin};
    }

    $cmp = $tmp1->{b} <=> $tmp2->{b};
    if ($cmp < 0) {
        $tmp->{b} = $tmp2->{b};
        $tmp->{open_end} = $tmp2->{open_end};
    }
    elsif ($cmp == 0) {
        $tmp->{b} = $tmp1->{b};
        $tmp->{open_end} = $tmp1->{open_end} ? $tmp2->{open_end} : 0;
    }
    else {
        $tmp->{b} = $tmp1->{b};
        $tmp->{open_end} = $tmp1->{open_end};
    }
    return $tmp;
=cut

}


method intersection ( Set::Functional::Span $span ) returns List of Set::Functional::Span {
    ...

=for TODO
            if ( ( $tmp1a <= $tmp1b ) &&
                 ( ($tmp1a != $tmp1b) ||
                   (!$open_beg and !$open_end) ||
                   ($tmp1a == $inf)   ||               # XXX
                   ($tmp1a == $neg_inf)
                 )
               )
            {
                if ( $op eq 'intersection' )
                {
                    push @a, {
                        a => $tmp1a, b => $tmp1b,
                        open_begin => $open_beg, open_end => $open_end } ;
                }
                if ( $op eq 'intersects' )
                {
                    return 1;
                }
                if ( $op eq 'intersected_spans' )
                {
                    push @a, $tmp1;
                    $a0++;
                    next A;
                }
            }
=cut

}

method stringify () returns String {
    ...

=for TODO
    my $set = shift;
    my $self = $_[0];
    my $s;
    return "" unless defined $self;
    $self->{open_begin} = 1 if ($self->{a} == -$inf );
    $self->{open_end}   = 1 if ($self->{b} == $inf );
    my $tmp1 = $self->{a};
    $tmp1 = $tmp1->datetime if UNIVERSAL::can( $tmp1, 'datetime' );
    $tmp1 = "$tmp1";
    my $tmp2 = $self->{b};
    $tmp2 = $tmp2->datetime if UNIVERSAL::can( $tmp2, 'datetime' );
    $tmp2 = "$tmp2";
    return $tmp1 if $tmp1 eq $tmp2;
    $s = $self->{open_begin} ? $set->separators(2) : $set->separators(0);
    $s .= $tmp1 . $set->separators(4) . $tmp2;
    $s .= $self->{open_end} ? $set->separators(3) : $set->separators(1);
    return $s;
=cut

}

method spaceship ( Set::Functional::Span $span ) returns Int {
    ...

=for TODO
    my ($tmp1, $tmp2, $inverted) = @_;
    my $cmp;
    if ($inverted) {
        $cmp = $tmp2->{a} <=> $tmp1->{a};
        return $cmp if $cmp;
        $cmp = $tmp1->{open_begin} <=> $tmp2->{open_begin};
        return $cmp if $cmp;
        $cmp = $tmp2->{b} <=> $tmp1->{b};
        return $cmp if $cmp;
        return $tmp1->{open_end} <=> $tmp2->{open_end};
    }
    $cmp = $tmp1->{a} <=> $tmp2->{a};
    return $cmp if $cmp;
    $cmp = $tmp2->{open_begin} <=> $tmp1->{open_begin};
    return $cmp if $cmp;
    $cmp = $tmp1->{b} <=> $tmp2->{b};
    return $cmp if $cmp;
    return $tmp2->{open_end} <=> $tmp1->{open_end};
=cut

}

=kwid

= NAME

Set::Functional::Span - An object representing a single span

= SYNOPSIS

  use Set:::Functional::Span;

  # XXX

= DESCRIPTION

This class represents a single span.

= CONSTRUCTORS

- `new( start => $start, end => $end, start_is_open => bool::false, end_is_open => bool::false )`

= OBJECT METHODS

The following methods are available for Set::Span objects:

- `start()`

Returns the start.

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
