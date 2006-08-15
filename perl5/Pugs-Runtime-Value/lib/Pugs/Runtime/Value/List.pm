package Pugs::Runtime::Value::List;

# Pugs::Runtime::Value::List - implementation of Perl6 'List' class in Perl5

# ChangeLog
#
# 2005-09-05
# * removed uniq(), grep() - now implemented in p6 Prelude
# * map() uses Code->arity
# * lists have an internal buffer for unshift/push
#
# 2005-09-01
# * fixed stringification
#
# 2005-08-31
# * str() is non-destructive
#
# 2005-08-30
# * new hook 'clone'
# * some constructors implement deep cloning
# * functions that don't support deep cloning emit warnings on clone
#
# 2005-08-29
# * new methods shift_n() pop_n() - provide lazy access to sublists
# * new hook DESTROY
#
# 2005-08-27
# * new methods start() end()
#
# 2005-08-23
# * fixed stringification
#
# 2005-08-12
# * fixed map()->pop()
#
# 2005-08-11
# * Fixed string comparison to Inf portability (Windows x Linux)
# * Separate from_num_range() and from_range() constructors. 
#   - from_num_range() is a numeric range. It accepts a 'step' value.
#   - from_range() is a generic range for strings, etc. It accepts a 'celems' closure.
#   Both constructors are just new() wrappers.
# * grep(), map() don't depend on coroutines
# * Removed pair() - this module does not have access to the Pair constructor
#
# 2005-08-10
# * Removed method concat_list(), added "TODO" methods

# TODO - finish sum() - not all operations support sum() yet; 
#      - object numification is not supported yet

# TODO - finish support for unshift, push 

# TODO - List.is_lazy() could be defined with a closure; Perl6 version too
# TODO - map(), grep() could accept the optional 'celems' parameter - for kv() implementation
# TODO - is_contiguous() should test if $step == 1

use strict;
use Pugs::Runtime::Value;

use constant Inf => Pugs::Runtime::Value::Num::Inf;
our $VERSION = '0.02';

sub _default_stringify {
    my $self = (shift)->clone;
    my @start;
    my @end;
    my $samples = 3;
    $samples = 1000 unless $self->is_infinite;
    my $tmp = -&Inf;
    for ( 1 .. $samples ) {
        last unless $self->elems;
        $tmp = $self->shift;
        last if $tmp == Inf;
        push @start, $tmp;
        last if $tmp == -&Inf;
    }
    $tmp = Inf;
    for ( 1 .. $samples ) {
        last unless $self->elems;
        $tmp = $self->pop( $tmp );
        last if $tmp == -&Inf;
        unshift @end, $tmp;
        last if $tmp == Inf;
    }
    return '' unless @start;
    # if @start and @end intersect, don't print ".."
    if ( $self->elems == 0 ) {
        push @start, @end;
        return join( ', ', @start, @end );
    }
    return 
        join( ', ', 
        map { Pugs::Runtime::Value::stringify( $_ ) } @start ) .
        ' ... ' . 
        join( ', ', 
        map { Pugs::Runtime::Value::stringify( $_ ) } @end );
}

sub new {
    my $class = shift;
    my %param = @_;

    $param{cis_infinite}   = sub { $_[0]->{celems}() == Inf } 
        unless defined $param{cis_infinite};
    $param{cis_contiguous} = sub { 0 } 
        unless defined $param{cis_contiguous};
    $param{cstringify}     = \&_default_stringify 
        unless defined $param{cstringify}; 

    $param{is_lazy}        = 1 unless defined $param{is_lazy};
    unless ( defined $param{celems} ) {
        $param{celems} =
            ( defined $param{cstart} || defined $param{cend} ) ? sub { Inf } : sub { 0 }
    }
    $param{cstart}  = sub {} unless defined $param{cstart};
    $param{cend}    = sub {} unless defined $param{cend}; 
    
    $param{start}   = sub {} unless defined $param{start};
    $param{end}     = sub {} unless defined $param{end}; 

    $param{shift_n} = sub { $_[0]->shift } unless defined $param{shift_n};
    $param{pop_n}   = sub { $_[0]->pop }   unless defined $param{pop_n}; 

    $param{DESTROY} = sub {}   unless defined $param{DESTROY}; 
    $param{clone}   = sub { 
        my $self = shift;
        my $clone = bless { %$self }, ref $self;
        @{$clone->{pops}} =   @{$self->{pops}};
        @{$clone->{shifts}} = @{$self->{shifts}};
        return $clone;
    } unless defined $param{clone}; 

    $param{sum}     = sub { undef }   unless defined $param{sum}; 

    $param{shifts}  = [] unless defined $param{shifts};
    $param{pops}    = [] unless defined $param{pops};

    return bless \%param, $class;
}

sub DESTROY       { $_[0]->{DESTROY}( @_ ) }
sub clone         { $_[0]->{clone}( @_ ) }
sub elems         { $_[0]->{celems}( @_ ) }
sub is_infinite   { $_[0]->{cis_infinite}( @_ ) }
sub is_contiguous { $_[0]->{cis_contiguous}( @_ ) }
sub is_lazy       { $_[0]->{is_lazy} }
sub str           { $_[0]->{cstringify}( @_ ) }
sub int           { $_[0]->elems }
sub bit           { $_[0]->elems > 0 }
sub num           { $_[0]->elems }
sub perl          { '(' . $_[0]->{cstringify}( @_ ) . ')' }
sub shift_n       { $_[0]->{shift_n}( @_ ) }
sub pop_n         { $_[0]->{pop_n}( @_ ) }
sub sum           { $_[0]->{sum}( @_ ) }

sub flatten       { 
    my $ret = shift;
    my $class = ref($ret);

    # TODO - add tests for this error message
    # fail "can't instantiate an infinite list"
    #     if $ret->is_infinite;

    my @list;
    while ( $ret->elems ) { push @list, $ret->shift }
    $class->from_single( @list ); 
}

sub from_num_range {
    my $class = shift;
    my %param = @_;
    my $start = Pugs::Runtime::Value::numify( $param{start} );
    my $end =   Pugs::Runtime::Value::numify( $param{end} );
    my $step =  Pugs::Runtime::Value::numify( $param{step} || 1 );
    $class->new(
        clone => sub { 
            $class->from_num_range( start => $start, end => $end, step => $step ) 
        },
        sum => sub {
            $_[0]->elems * ( $start + $end ) / 2;
        },
        shift_n => sub {
            my $list = shift;
            my $length = shift;
            # warn "shift_n( $length ) from  ". $list->start . "..". $list->end;
            my $middle = $start + $length - 1;
            $middle = $end if $middle > $end;
            my $shifted = ref($list)->from_num_range(
                start => $start,
                end => $middle,
                step => $step,
            );
            $start = $middle + 1;
            return $shifted;
        },
        start =>   sub { $start },
        end =>     sub { $end },
        cstart =>  sub {
            my $r = $start;
            if ( defined $step ) { $start += $step } else { $start++ };
            return $r;
        },
        cend =>    sub {
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
        celems =>  sub {
            # warn "ELEMS $end - $start = ".($end - $start + 1)."\n";
            return Inf if $start == -&Inf || $end == Inf;
            return $end - $start + 1 unless defined $step;
            return CORE::int(( $end - $start + 1 ) / $step);
        },
        cis_infinite =>   sub { return $start == -&Inf || $end == Inf },
        cis_contiguous => sub { $step == -1 || $step ==  1 },
    );
}

sub from_range {
    my $class = shift;
    my %param = @_;
    my $start = $param{start};
    my $end =   $param{end};
    my $count = $param{celems};
    $count = sub { 
            no warnings 'numeric';
            if ( ref($end) ) {
                return Inf if $end->unboxed == Inf;
                return $start->unboxed le $end->unboxed ? Inf : 0 
            }
            else
            {
                return Inf if $end == Inf;
                return $start le $end ? Inf : 0 
            }
        } 
        unless defined $count;
    $class->new(
        clone => sub { 
            $class->from_range( start => $start, end => $end, celems => $count ) 
        },
        sum => sub {
            warn "string sum not supported";
        },
        start =>   sub { $start },
        end =>     sub { $end },
        cstart =>  sub { 
            my $tmp = $start; 
            if ( ref( $start ) ) {
                $start = $start->increment; 
            }
            else {
                $start++;
            }
            $tmp; 
        },
        cend =>    sub { 
            my $tmp = $end; 
            if ( ref( $end ) ) {
                $end = $end->decrement; 
            }
            else {
                $end--;
            }
            $tmp; 
        },
        celems =>  $count,
        cis_contiguous => sub { 1 },
    );
}

sub from_x {
    # implements ('a' x 100) style lists
    # this can be used to create sparse arrays like:  [ 1, undef x 10000, 2 ]
    my $class = shift;
    my %param = @_;
    my $item =  $param{item};
    my $count = $param{count};
    $count = 0 
        unless defined $count;
    $class->new(
        clone => sub { 
            $class->from_x( item => $item, count => $count ) 
        },
        sum => sub {
            Pugs::Runtime::Value::numify( $_[0] ) * $item;
        },
        shift_n => sub {
            my $list = shift;
            my $length = shift;
            # warn "shift_n( $length ) from  ". $list->start . "..". $list->end;
            $length = $count if $length > $count;
            my $shifted = ref($list)->from_x(
                item => $item,
                count => $length,
            );
            $count = $count - $length;
            return $shifted;
        },
        start =>   sub { $item },
        end =>     sub { $item },
        cstart =>  sub { $count--; return if $count < 0; $item },
        cend =>    sub { $count--; return if $count < 0; $item },
        celems =>  sub { $count },
    );
}

sub from_single {
    my $class = shift;

    my @list;
    for( @_ ) {
        if ( UNIVERSAL::isa($_, 'Pugs::Runtime::Value::List') ) {
            my @li; push @li, $_->shift while $_->elems; 
            push @list, @li;
        }
        else {
            push @list, $_
        }
    }

    $class->new(
        clone =>   sub { $class->from_single( @list ) },
        start =>   sub { $list[0]  },
        end =>     sub { $list[-1] },
        cstart =>  sub { shift  @list },
        cend =>    sub { pop    @list },
        celems =>  sub { scalar @list },
        is_lazy => 0,
     );
}

sub from_coro {
    my $class = shift;
    my $start = shift;
    my $size =  &Inf;
    $class->new(
        clone => sub { 
            warn "from_coro->clone() not implemented";
            $class->from_coro( $start ) 
        },
        cstart =>  sub {
            my $r = $start->();
            # print "coro\n";
            $size = 0 unless defined $r;
            return $r;
        },
        cend =>           sub {},
        celems =>         sub { $size },
        cis_infinite =>   sub { $size == Inf },
        cis_contiguous => sub { 0 },
    );
}

# --- list operations ---

sub reverse { 
    my $ret = shift;
    Pugs::Runtime::Value::List->new( 
        clone => sub { 
            $ret->clone->reverse 
        },
        sum =>            $ret->{sum},
        start =>          $ret->{end},
        end =>            $ret->{start},
        cstart =>         $ret->{cend},
        cend =>           $ret->{cstart},
        celems =>         $ret->{celems},
        cis_infinite =>   $ret->{cis_infinite},
        cis_contiguous => $ret->{cis_contiguous},
        cstringify =>     $ret->{cstringify},
    );
}

sub map { 
    my $array = shift;
    my $code = shift;
    my $ret = $array->clone; 
    my $arity = Pugs::Runtime::Value::numify( $code->arity );
    Pugs::Runtime::Value::List->new(
        clone => sub { 
            my $self = shift;
            # this doesn't work if $code is a closure
            # it may break grep(), uniq(), kv()...
            # It will need $code->clone in order to work...
            warn "List::map->clone() not implemented";
            warn "map->clone has pending shifts/pops" if @{$self->{pops}} || @{$self->{shifts}};
            $ret->clone->map( $code ) 
        },
        cstart => sub {
            my $self = shift;
            # print "entering map, elems = ", $ret->elems, "\n";
            while( $ret->elems && @{$self->{shifts}} < 2 ) {
                my @x;
                push @x, $ret->shift for 1 .. $arity;
                my $res = $code->do( @x );
                my @res;
                if ( Pugs::Runtime::Value::p6v_isa( $res, 'Array' ) ) {
                    @res = $res->items 
                }
                else {
                    @res = ($res)
                }
                push @{$self->{shifts}}, @res;
            }
            # print " left [", @shifts, @pops, "] ", scalar @shifts, "+", scalar @pops, "\n";
            return shift @{$self->{shifts}} if @{$self->{shifts}};
            return shift @{$self->{pops}}   if @{$self->{pops}};
            return
        },
        cend => sub { 
            my $self = shift;
            while( $ret->elems && @{$self->{pops}} < 2 ) {
                my @x;
                unshift @x, $ret->pop for 1 .. $arity;
                my $res = $code->do( @x );
                my @res;
                if ( Pugs::Runtime::Value::p6v_isa( $res, 'Array' ) ) {
                    @res = $res->items 
                }
                else {
                    @res = ($res)
                }
                unshift @{$self->{pops}}, @res;
            }
            return pop @{$self->{pops}}   if @{$self->{pops}};
            return pop @{$self->{shifts}} if @{$self->{shifts}};
            return
        },
        celems => sub { 
            my $self = shift;
            $ret->elems ? Inf : scalar @{$self->{shifts}} + scalar @{$self->{pops}}
        },
    );
}

sub zip { 
    my $array = shift;
    my @lists = @_;
    my $ret = $array->clone; 
    Pugs::Runtime::Value::List->new(
        sum => sub { 
            my $sum = $ret->sum;
            $sum += $_->sum for @lists;
            return $sum;
        },
        clone => sub {
            my $self = shift;
            my ( $l, @ls ) = map { $_->clone } ( $ret, @lists );
            warn "zip->clone has pending shifts/pops" if @{$self->{pops}} || @{$self->{shifts}};
            return $l->zip( @ls );
        },
        cstart => sub {
            my $self = shift;
            return shift @{$self->{shifts}} if @{$self->{shifts}};
            my $any = 0;
            for ( $ret, @lists ) { $any++ if $_->elems }
            push @{$self->{shifts}}, ( $ret->shift, 
                        map { my $x = $_->shift } #; defined $x ? $x : 'x' } 
                            @lists ) if $any;
            return shift @{$self->{shifts}} if @{$self->{shifts}};
            return shift @{$self->{pops}}   if @{$self->{pops}};
            return
        },
        cend => sub { 
            my $self = shift;
            return pop @{$self->{pops}} if @{$self->{pops}};
            my $any = 0;
            for ( $ret, @lists ) { $any++ if $_->elems }
            unshift @{$self->{pops}}, ( $ret->pop, 
                        map { my $x = $_->pop } #; defined $x ? $x : 'x' } 
                            @lists ) if $any;
            return pop @{$self->{pops}}   if @{$self->{pops}};
            return pop @{$self->{shifts}} if @{$self->{shifts}};
            return
        },
        celems => sub { 
            my $self = shift;
            my $any = 0;
            for ( $ret, @lists ) { $any++ if $_->elems }
            $any ? Inf : scalar @{$self->{shifts}} + scalar @{$self->{pops}} 
        },
    );
}

sub shift { $_[0]->{celems}( @_ ) ? $_[0]->{cstart}( @_ ) : undef }
sub pop   { $_[0]->{celems}( @_ ) ? $_[0]->{cend}( @_ )   : undef }  
sub start { $_[0]->{celems}( @_ ) ? $_[0]->{start}( @_ )  : undef }
sub end   { $_[0]->{celems}( @_ ) ? $_[0]->{end}( @_ )    : undef }  

1;
__END__

# removed uniq, grep - these are just examples

sub _MySub::arity { 1 };
sub _MySub::do    { (shift)->(@_) };

sub uniq { 
    # TODO - use p6 hash
    my $array = shift;
    my %seen = ();
    return $array->map( 
        bless sub {
            my $str = $_[0];
            $str = '**UnDeF**' unless defined $str;
            return if $seen{$str};
            $seen{$str}++;
            $_[0];
        }, '_MySub' ); 
}

sub grep { 
    my $array = shift;
    my $code = shift;
    return $array->map( 
        bless sub { 
            return $_[0] if $code->($_[0]);
            return
        }, '_MySub' ); 
}

sub kv { 
    my $array = shift;
    my $count = 0;
    return $array->map( 
        sub {
            return ( $count++, $_[0] )
        } ); 
}

sub keys { 
    my $array = shift;
    my $count = 0;
    return $array->map( 
        sub {
            $count++
        } ); 
}

sub values { 
    @_
}


=head1 NAME

Pugs::Runtime::Value::List - Perl extension for Perl6 "List" class

=head1 SYNOPSIS

  use Pugs::Runtime::Value::List;
  
  my $list = Pugs::Runtime::Value::List.from_range( start => 10, end => 20 );
  
  my $list = Pugs::Runtime::Value::List.new( ... );

=head1 DESCRIPTION

This module implements a "List" object.

new() without parameters is an empty list.

=head1 SEE ALSO

Pugs

=head1 AUTHOR

Flavio S. Glock, E<lt>fglock@gmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.


=cut
