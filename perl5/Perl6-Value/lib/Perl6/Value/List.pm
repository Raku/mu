package Perl6::Value::List;

# Perl6::Value::List - implementation of Perl6 'List' class in Perl5

# ChangeLog
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

# TODO - stringification of unboxed types doesn't call native-stringification
#        meaning Inf and NaN stringify wrongly
# TODO - List.is_lazy() could be defined with a closure; Perl6 version too
# TODO - map(), grep() could accept the optional 'celems' parameter - for kv() implementation
# TODO - is_contiguous() should test if $step == 1

use strict;
use Perl6::Value;

use constant Inf => Perl6::Value::Num::Inf;
our $VERSION = '0.01';

sub _default_stringify {
    my $self = shift;
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
        map { UNIVERSAL::can($_,'str') ? $_->str : $_ } @start ) .
        ' ... ' . 
        join( ', ', 
        map { UNIVERSAL::can($_,'str') ? $_->str : $_ } @end );
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
    $param{cstart} = sub {} unless defined $param{cstart};
    $param{cend}   = sub {} unless defined $param{cend}; 
    
    $param{start} = sub {} unless defined $param{start};
    $param{end}   = sub {} unless defined $param{end}; 

    return bless \%param, $class;
}

sub clone         { bless { %{ $_[0] } }, ref $_[0] }
sub elems         { $_[0]->{celems}() }
sub is_infinite   { $_[0]->{cis_infinite}( $_[0] ) }
sub is_contiguous { $_[0]->{cis_contiguous}() }
sub is_lazy       { $_[0]->{is_lazy} }
sub str           { $_[0]->{cstringify}( $_[0] ) }
sub int           { $_[0]->elems }
sub bit           { $_[0]->elems > 0 }
sub num           { $_[0]->elems }
sub perl          { '(' . $_[0]->{cstringify}( $_[0] ) . ')' }

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
    my $start = $param{start};
    my $end =   $param{end};
    my $step =  $param{step} || 1;
    $class->new(
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
                            return $end - $start + 1 unless defined $step;
                            return CORE::int(( $end - $start + 1 ) / $step);
                        },
                cis_infinite => sub { return $start == -&Inf || $end == Inf },
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
            return Inf if $end == Inf;
            $start le $end ? Inf : 0 
        } 
        unless defined $count;
    $class->new(
                start =>   sub { $start },
                end =>     sub { $end },
                cstart =>  sub { $start++ },
                cend =>    sub { $end-- },
                celems =>  $count,
                cis_contiguous => sub { 1 },
    );
}

sub from_x {
    # implements ('a' x 100) style lists
    # this can be used to create sparse arrays like:  [ 1, undef x 10000, 2 ]
    my $class = shift;
    my %param = @_;
    my $item = $param{item};
    my $count = $param{count};
    $count = 0 
        unless defined $count;
    $class->new(
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
    for(@_) {
        if ( UNIVERSAL::isa($_, 'Perl6::Value::List') ) {
            my @li; push @li, $_->shift while $_->elems; 
            push @list, @li;
        }
        else {
            push @list, $_
        }
    }

    $class->new(
                 start =>   sub { $list[0] },
                 end =>     sub { $list[-1] },
                 cstart =>  sub{ shift  @list },
                 cend =>    sub{ pop    @list },
                 celems =>  sub{ scalar @list },
                 is_lazy => 0,
             );
}

sub from_coro {
    my $class = shift;
    my $start = shift;
    my $size = Inf;
    $class->new(
                cstart =>  sub {
                            my $r = $start->();
                            # print "coro\n";
                            $size = 0 unless defined $r;
                            return $r;
                        },
                cend =>    sub {},
                celems =>  sub { $size },
                cis_infinite => sub { $size == Inf },
                cis_contiguous => sub { 0 },
    );
}

# --- list operations ---

sub reverse { 
    my $ret = shift;
    Perl6::Value::List->new( 
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

sub grep { 
    my $array = shift;
    my $code = shift;
    return $array->map( 
        sub { 
            return $_[0] if $code->($_[0]);
            return
        } ); 
}

sub map { 
    my $array = shift;
    my $code = shift;
    my $ret = $array->clone; 
    my @shifts;
    my @pops;
    Perl6::Value::List->new(
            cstart => sub {
                    # print "entering map, elems = ", $ret->elems, "\n";
                    while( $ret->elems ) {
                        # TODO - invert the order a bit
                        my $x = $ret->shift; 
                        # print "map $x\n";
                        # print " got x ", $x," elems ", $ret->elems ,"\n";
                        push @shifts, $code->($x);

                        unless ( @shifts > 1 ) {
                            # keep some data in the buffer - helps to find EOF in time
                            my $x = $ret->shift; 
                            push @shifts, $code->($x);
                        }

                        # print " mapped to [", @shifts, "] ", scalar @shifts, "\n";
                        return shift @shifts if @shifts;
                        # print " skipped ";
                    }
                    # print " left [", @shifts, @pops, "] ", scalar @shifts, "+", scalar @pops, "\n";
                    return shift @shifts if @shifts;
                    return shift @pops if @pops;
                    return
            },
            cend => sub { 
                    while( $ret->elems ) {
                        my $x = $ret->pop; 
                        # print "x ", $_," elems ", $ret->elems ,"\n";
                        unshift @pops, $code->($x);

                        unless ( @pops > 1 ) {
                            # keep some data in the buffer - helps to find EOF in time
                            my $x = $ret->pop; 
                            unshift @pops, $code->($x);
                        }

                        return pop @pops if @pops;
                    }
                    return pop @pops if @pops;
                    return pop @shifts if @shifts;
                    return
            },
            celems => sub { 
                    $ret->elems ? Inf : $#shifts + $#pops + 2 
            },
    );
}

sub uniq { 
    my $array = shift;
    my %seen = ();
    return $array->map( 
        sub {
            my $str = $_[0];
            $str = '**UnDeF**' unless defined $str;
            return if $seen{$str};
            $seen{$str}++;
            $_[0];
        } ); 
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

sub zip { 
    my $array = shift;
    my @lists = @_;
    my $ret = $array->clone; 
    my @shifts;
    my @pops;
    Perl6::Value::List->new(
            cstart => sub {
                return shift @shifts if @shifts;
                my $any = 0;
                for ( $ret, @lists ) { $any++ if $_->elems }
                push @shifts, ( $ret->shift, 
                            map { my $x = $_->shift } #; defined $x ? $x : 'x' } 
                                @lists ) if $any;
                return shift @shifts if @shifts;
                return shift @pops if @pops;
                return
            },
            cend => sub { 
                return pop @pops if @pops;
                my $any = 0;
                for ( $ret, @lists ) { $any++ if $_->elems }
                unshift @pops, ( $ret->pop, 
                            map { my $x = $_->pop } #; defined $x ? $x : 'x' } 
                                @lists ) if $any;
                return pop @pops if @pops;
                return pop @shifts if @shifts;
                return
            },
            celems => sub { 
                my $any = 0;
                for ( $ret, @lists ) { $any++ if $_->elems }
                $any ? Inf : $#shifts + $#pops + 2 
            },
    );
}

sub shift { $_[0]->{celems}() ? $_[0]->{cstart}() : undef }
sub pop   { $_[0]->{celems}() ? $_[0]->{cend}()   : undef }  

sub start { $_[0]->{celems}() ? $_[0]->{start}() : undef }
sub end   { $_[0]->{celems}() ? $_[0]->{end}()   : undef }  

1;
__END__

=head1 NAME

Perl6::Value::List - Perl extension for Perl6 "List" class

=head1 SYNOPSIS

  use Perl6::Value::List;
  
  my $list = Perl6::Value::List.from_range( start => 10, end => 20 );
  
  my $list = Perl6::Value::List.new( ... );

=head1 DESCRIPTION

This module implements a "List" object.

new() without parameters is an empty list.

=head1 SEE ALSO

Pugs

=head1 AUTHOR

Flavio S. Glock, E<lt>fglock@Egmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.


=cut
