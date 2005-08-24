
# This is a Perl5 file

# ChangeLog
#
# 2005-08-12
# * store(List) actually means store([List])
# * fixed new(), elems(), pop(), shift(), fetch()
# * fixed "defined $i" to "elems == 0" in push/pop
#
# 2005-08-11
# * fixed some syntax errors
#
# 2005-08-10
# * Ported from Perl6 version

# TODO - store(List) actually means store([List]), Perl6 version
# TODO - fix "defined $i" to "elems == 0" in push/pop, Perl6 version
# TODO - splice() should accept a 'List' object, Perl6 version too
#
# TODO - Tests:
# TODO - test splice offset == 0, 1, 2, -1, -2, -Inf, Inf
# TODO - test splice length == 0, 1, 2, Inf, negative
# TODO - test splice list == (), (1), (1,2), Iterators, ...
# TODO - splice an empty array
# TODO - test multi-dimensional array
# TODO - test optional splice parameters

# Notes:
# * Cell is implemented in the Perl6::Container::Scalar package

use strict;

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Value;
use Perl6::Container::Scalar;

my $class_description = '-0.0.1-cpan:FGLOCK';

class 'Array'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ [ '$:cell' => { 
                        access => 'rw', 
                        build => sub { 
                            # warn " ---- new @_ ---- ";
                            my $cell = Perl6::Cell->new;
                            my $h = Perl6::Container::Array->new( items => [ @_ ] );
                            $cell->{v} = $h;
                            $cell->{type} = 'Array';
                            return $cell;
                        } } ] ],
        DESTROY => sub {
            $_[0]->{'instance_data'}{'$:cell'} = undef;  
        },
        methods => { 

            # %a := %b 
            'bind' =>     sub {
                my ( $self, $thing ) = @_;
                die "argument to Array bind() must be a Array"
                    unless $thing->cell->{type} eq 'Array';
                _('$:cell', $thing->cell);
                return $self;
            },
            'cell' =>     sub { _('$:cell') },  # cell() is used by bind()
            'id' =>       sub { _('$:cell')->{id} },  

            'tieable' =>  sub { _('$:cell')->{tieable} != 0 },
            'tie' =>      sub { shift; _('$:cell')->tie(@_) },
            'untie' =>    sub { _('$:cell')->untie },

             # See perl5/Perl6-MetaModel/t/14_AUTOLOAD.t  
            'isa' => sub { ::next_METHOD() },

            # 'elems' =>    sub { _('$:cell')->{tied} ? 
            #                    _('$:cell')->{tied}->elems :
            #                    Perl6::Container::Array::elems( _('$:cell')->{v} )
            # },

            'unboxed' => sub { 
                _('$:cell')->{tied} ? _('$:cell')->{tied} : _('$:cell')->{v}
            },
            'AUTOLOAD' => sub {
                my ($self, @param) = @_;
                my $method = ::AUTOLOAD($self);

                # TODO - add support for tied array
                # TODO - check if scalar := array works properly
                my $tmp = $self->unboxed;
                # warn ref($tmp), ' ', $method, " @param == " . $tmp->$method( @param );
                
                @param = 
                    map {
                        UNIVERSAL::isa( $_, 'List' ) ? $_->unboxed : $_ 
                    } @param;

                if ( $method eq 'splice' || $method eq 'reverse' ) {
                    my $ret = Array->new();
                    $ret->push( $tmp->$method( @param )->items );
                    return $ret;
                }
                
                if ( $method eq 'push'   || $method eq 'unshift' ) {
                    $tmp->$method( @param );
                    return $self;
                }
                
                return $tmp->$method( @param );
            },
            
            str => sub {
                # warn "-- str --";
                my $self = _('$:cell')->{tied} ? _('$:cell')->{tied} : _('$:cell')->{v};
                # warn "-- cell $tmp --";
                
                my @start;
                my @end;
                my $samples = 2;
                $samples = 100 unless $self->is_infinite; 
                my $tmp = -&Inf;
                for ( 0 .. $samples ) {
                    last if $_ >= $self->elems;
                    $tmp = $self->fetch( $_ );
                    last if $tmp == &Inf;
                    push @start, $tmp;
                    last if $tmp == -&Inf;
                }
                $tmp = &Inf;
                for ( map { $_ - $samples - 1 } reverse 0 .. $samples ) {
                    last unless $self->elems + $_ > scalar @start;
                    $tmp = $self->fetch( $_ );
                    # warn "$_ - $tmp";
                    last if $tmp == -&Inf;
                    unshift @end, $tmp;
                    last if $tmp == &Inf;
                }
                return '' unless @start;
                # if @start and @end intersect, don't print ".."
                if ( $self->elems == ( scalar @start + scalar @end ) ) {
                    return join( ', ', @start, @end );
                }
                return 
                    join( ', ', 
                    map { UNIVERSAL::can($_,'str') ? $_->str : $_ } @start ) .
                    ' ... ' . 
                    join( ', ', 
                    map { UNIVERSAL::can($_,'str') ? $_->str : $_ } @end );
                
            },
            perl => sub { '[' . ::SELF->str . ']' },
        },
    }
};

# ----- unboxed functions

package Perl6::Container::Array;

use strict;
use Perl6::Value;
use Perl6::Value::List;

use constant Inf => Perl6::Value::Num::Inf;

sub new {
    my $class = shift;
    my %param = @_;
    my @items = @{$param{items}};
    # warn "-- new -- @items --";
    return bless { items => \@items }, $class;
}

sub clone         { bless { %{ $_[0] } }, ref $_[0] }

sub items {
    my $self = shift;
    # my @x = %$self;  warn "-- items -- @x --";
    return @{$self->{items}};
}

sub from_list {
    my $class = shift;
    $class->new( items => [@_] );
}

sub _shift_n { 
    my $array = shift;
    my $length = shift;
    my @ret;
    my @tmp = @{$array->{items}};
    if ( $length == Inf ) {
        @{$array->{items}} = ();
        return @tmp;
    }
    while ( @tmp ) {
        # warn "ret ".scalar(@ret)." length $length";
        last if @ret >= $length;
        if ( ! UNIVERSAL::isa( $tmp[0], 'Perl6::Value::List') ) {
            push @ret, shift @tmp;
            next;
        }
        if ( $tmp[0]->elems > 0 ) {
            my $i = $tmp[0]->shift;
            push @ret, $i;
            last if @ret >= $length;
        }
        else {
            shift @tmp;
        }
    };
    @{$array->{items}} = @tmp;
    # warn "ret @ret ; array @tmp ";
    return @ret;
}

sub _pop_n {
    my $array = shift;
    my $length = shift;
    my @ret;
    my @tmp = @{$array->{items}};
    if ( $length == Inf ) {
        @{$array->{items}} = ();
        return @tmp;
    }
    while ( @tmp ) {
        # warn "ret ".scalar(@ret)." length $length";
        last if @ret >= $length;
        if ( ! UNIVERSAL::isa( $tmp[-1], 'Perl6::Value::List') ) {
            unshift @ret, pop @tmp;
            next;
        }
        if ( $tmp[-1]->elems > 0 ) {
            my $i = $tmp[-1]->pop;
            unshift @ret, $i;
            last if @ret >= $length;
        }
        else {
            pop @tmp;
        }
    };
    @{$array->{items}} = @tmp;
    # warn "ret @ret ; array @tmp ";
    return @ret;
}

sub elems {
    my $array = shift;
    my $count = 0;
    for ( @{$array->{items}} ) {
        $count += UNIVERSAL::isa( $_, 'Perl6::Value::List') ?
                  $_->elems  :
                  1;
    }
    $count;
}

sub is_infinite {
    my $array = shift;
    for ( @{$array->{items}} ) {
        return 1 if UNIVERSAL::isa( $_, 'Perl6::Value::List') && $_->is_infinite;
    }
    0;
}

sub is_lazy {
    my $array = shift;
    for ( @{$array->{items}} ) {
        return 1 if UNIVERSAL::isa( $_, 'Perl6::Value::List') && $_->is_lazy;
    }
    0;
}

sub flatten {
    # this needs optimization
    my $array = shift;
    my $ret = $array->clone;
    for ( @{$ret->{items}} ) {
        $_ = $_->flatten() if UNIVERSAL::isa( $_, 'Perl6::Value::List') && $_->is_lazy;
    }
    $ret;
}

sub splice { 
    my $array = shift;
    my $offset = shift; $offset = 0   unless defined $offset;
    my $length = shift; $length = Inf unless defined $length;
    my @list = @_;
    my ( @head, @body, @tail );
    # print "items: ", $array->items, " splice: $offset, $length, ", @list, "\n";
    # print 'insert: ', $_, ' ', $_->ref for @list, "\n";
    if ( $offset >= 0 ) {
        @head = $array->_shift_n( $offset );
        if ( $length >= 0 ) {
            @body = $array->_shift_n( $length );
            @tail = $array->_shift_n( Inf );
        }
        else {
            @tail = $array->_pop_n( -$length );
            @body = $array->_shift_n( Inf );
        }
    }
    else {
        @tail = $array->_pop_n( -$offset );
        @head = $array->_shift_n( Inf );
        if ( $length >= 0 ) {
            # make $#body = $length
            while ( @tail ) {
                last if @body >= $length;
                push @body, shift @tail;
            }
        }
        else {
            # make $#tail = -$length
            while ( @tail ) {
                last if @tail->elems <= -$length;
                push @body, shift @tail;
            }
        }
    };
    # print 'head: ',@head, ' body: ',@body, ' tail: ',@tail, ' list: ',@list, "\n";
    @{$array->{items}} = ( @head, @list, @tail );
    return Perl6::Container::Array->from_list( @body );
}

sub end  {
    my $array = shift;
    return unless $array->elems;
    my $x = $array->pop;
    $array->push( $x );
    return $x;
}

sub fetch {
    # XXX - this is very inefficient
    # see also: slice()
    my $array = shift;
    my $pos = shift;
    
    #use Data::Dumper;
    #warn "-- array -- ". Dumper( $array );
    my $ret = $array->splice( $pos, 1 );
    #warn "-- $pos -- ".@{$ret->{items}}." -- ".@{$array->{items}}." ";
    if ( $pos == -1 ) {
        $array->push( @{$ret->{items}} );
    }
    elsif ( $pos < 0 ) {
        $array->splice( $pos+1, 0, @{$ret->{items}} );
    }
    else {
        $array->splice( $pos, 0, @{$ret->{items}} );
    }
    return shift @{$ret->{items}};
}

sub store {
    # TODO - $pos could be a lazy list of pairs!
    my $array = shift;
    my $pos = shift;
    my $item  = shift;
    if ( UNIVERSAL::isa( $item, 'Perl6::Value::List') ) {
        my $class = ref($array);
        $item = $class->new( items => [$item] );
    }
    $array->splice( $pos, 1, $item );
    return $array;
}

sub reverse {
    my $array = shift;
    my @rev = reverse @{$array->{items}};
    @rev = map {
            $_->isa('Perl6::Value::List') ? $_->reverse : $_
        } @rev;
    return Perl6::Container::Array->from_list( @rev );
}

sub to_list {
    my $array = shift;
    my $ret = $array->clone;
    # TODO - optimization - return the internal list object, if there is one
    return Perl6::Value::List->new(
            cstart => sub { $ret->shift },
            cend =>   sub { $ret->pop },
            celems => sub { $ret->elems },
            is_lazy => $ret->is_lazy,
        )
}

sub unshift {
    my $array = shift;
    unshift @{$array->{items}}, @_;
    return $array;
}

sub push {
    my $array = shift;
    push @{$array->{items}}, @_;
    return $array;
}

sub pop {
    my $array = shift;
    ( $array->_pop_n( 1 ) )[0]
}

sub shift {
    my $array = shift;
    ( $array->_shift_n( 1 ) )[0]
}

1;
__END__

=head1 NAME

Perl6::Container::Array - Perl extension for Perl6 "Array" class

=head1 SYNOPSIS

  use Perl6::Container::Array;

  ...

=head1 DESCRIPTION

...


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
