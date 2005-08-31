
# This is a Perl5 file

# ChangeLog
#
# 2005-08-29
# * Lazy lists are deep cloned when Array is cloned
#
# 2005-08-29
# * Full support for lazy splicing and sparse array
# * Added support for store() past the end of the array
# * Simplified fetch()
#
# 2005-08-27
# * Fixed fetch/store single elements from a lazy slice
# * Fixed fetch/store of whole lazy slice
#   supports syntax: @a = (0..Inf); @a[1,10,100..10000] = @a[500..30000]
#   (needs optimization)
# * New method tied()
# * Fixed binding of fetched result
# * Fixed stringification of unboxed values
# * New parameter 'max' in perl() and str() methods - controls how many elements
#   of the lazy array are stringified.
# * Array is stringified using parenthesis.
#
# 2005-08-26
# * New internal class 'Perl6::Slice'
#   supports syntax: @a = (2,3,4,5); @a[1,2] = @a[0,3]
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

# TODO - TEST FAIL - PIL-Run> @a[10,100,1000,10000,100000]=(1..9999999) 
#        returns undef (the list goes into array element zero)
#        might be a signature problem
# TODO - TEST FAIL - array-boxed.t - "shared" lazy lists are not shared
#
# TODO - should lazy Arrays warn when you are doing something stupid? like 
#           '@a=lazy_read_from_file; @b=@a; shift @b;' 
#           - @a no longer points to the first line of file
#      - option 1 - warn the user
#      - option 2 - use a shared buffer
#      - option 3 - implement lazy-list-deep-clone
#
# TODO - optimize Eager array to O(1)
#
# TODO - ($a,undef,$b) = @a
#      - (@a[1..10],$a,undef,$b) = @a
#
# TODO - @a[1] == Scalar
# TODO - test - @a[1] := $x
# TODO - exists
# TODO - defined

# TODO - @a[1..2]=(1,2) 
#        &postcircumfix:<[ ]> has to be "is rw"
#        &infix:<,> has to be is rw, too (think ($a, undef, $b) = (1,2,3))

# TODO - store(List) actually means store([List]), Perl6 version
# TODO - fix "defined $i" to "elems == 0" in push/pop, Perl6 version
# TODO - splice() should accept a 'List' object, Perl6 version too
#
# TODO - Tests:
# TODO - fetch/store should not destroy binding
# TODO - test splice offset == 0, 1, 2, -1, -2, -Inf, Inf
# TODO - test splice length == 0, 1, 2, Inf, negative
# TODO - test splice list == (), (1), (1,2), Iterators, ...
# TODO - test splice an empty array
# TODO - test multi-dimensional array

# Notes:
# * Cell is implemented in the Perl6::Container::Scalar package

use strict;
use Carp;

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Value;
use Perl6::Container::Scalar;

use constant Inf => Perl6::Value::Num::Inf;

my $class_description = '-0.0.1-cpan:FGLOCK';

sub Perl6::Value::stringify {
    my $s = shift;
    $s = $s->fetch if ref($s) && $s->isa('Scalar');
    $s = $s->str(max=>3) if ref($s) && $s->can('str');
    $s = $s->unboxed if ref($s) && $s->can('unboxed');
    return 'undef' unless defined $s;

    no warnings 'numeric';
    $s = Perl6::Value::Num::to_str( $s ) if $s+0 eq $s;
    
    return $s;
}

# ------ Perl6::Slice -----

sub Perl6::Slice::new { 
    my $class = shift;
    my %param = @_;  
    bless { %param }, $class;
} 
sub Perl6::Slice::fetch {
    my $self = shift;
    my $i = shift;
    my $pos = $self->{slice}->fetch( $i )->fetch;
    $pos = $pos->unboxed if ref( $pos );
    return unless defined $pos;
    return if $pos >= $self->{array}->elems()->unboxed || $pos < 0;
    $self->{array}->fetch( $pos, @_ );
}
sub Perl6::Slice::store {
    my $self = shift;
    my $i = shift;
    my $pos = $self->{slice}->fetch( $i )->fetch;
    $pos = $pos->unboxed if ref( $pos );
    return unless defined $pos;
    return if $pos >= $self->{array}->elems()->unboxed || $pos < 0;
    $self->{array}->store( $pos, @_ );
}
sub Perl6::Slice::is_infinite {
    my $self = shift; 
    $self->{slice}->is_infinite()->unboxed;
}
sub Perl6::Slice::elems {
    my $self = shift; 
    $self->{slice}->elems()->unboxed;
}
sub Perl6::Slice::unbind {
    # creates a new Array - not bound to the original array/slice
    my $self = shift; 
    my $ary = $self->{array};
    my @idx = $self->{slice}->items;
    my $result = Array->new;
    my $pos = 0;
    for my $i ( @idx ) {
        # warn "unbind() loop...";
        if ( UNIVERSAL::isa( $i, 'Perl6::Value::List' ) ) {
            die "Not implemented: instantiate lazy slice using a non-contiguous list"
                unless $i->is_contiguous;
            my $start = $i->start;
            my $end =   $i->end;
            die "Slice start/end is not defined"
                unless defined $end && defined $start;
            die "Not implemented: instantiate lazy slice using a reversed list"
                unless $end >= $start;
            #warn "unbind: \n";
            #warn "    Index: ". $i->str . "\n";
            # warn "Slicing from $start to $end";
            my $slice = $ary->splice( $start, ( $end - $start + 1 ) );
            my $elems = $slice->elems->unboxed;
            # warn "splice 2 - elems = $elems - slice isa $slice";
            $ary->splice( $start, 0, $slice );
            # warn "splice done";
            # items should be cloned before storing
            my @items = $slice->unboxed->items;
            @items = map {
                        UNIVERSAL::isa( $_, 'Perl6::Value::List' ) ? $_->clone : $_
                    } @items;
            $result->push( @items );
            if ( $elems < ( $end - $start + 1 ) ) {
                my $diff = $end - $start + 1 - $elems;
                # warn "Missing $diff elements";
                $result->push( Perl6::Value::List->from_x( item => undef, count => $diff ) );
            }
            $pos = $pos + $end - $start + 1;
            # warn "pos = $pos";
        }
        else {
            # non-lazy slicing
            my $p = $self->{slice}->fetch( $pos )->fetch;
            $p = $p->unboxed if ref( $p );
            #warn "P $pos = $p";
            next unless defined $p;
            next if $p >= $ary->elems()->unboxed || $p < 0;
            my $tmp = $ary->fetch( $p )->fetch;
            $result->store( $pos, $tmp );
            #warn "store $tmp to $pos";
            $pos++;            
        }
    }
    return $result;
}
sub Perl6::Slice::write_thru {
    # writes back to the bound Array using the slice index
    my $self = shift; 
    my $other = shift;
    my $ary = $self->{array};
    my @idx = $self->{slice}->items;
    my $pos = 0;
    for my $i ( @idx ) {
        #warn "write loop...";
        if ( UNIVERSAL::isa( $i, 'Perl6::Value::List' ) ) {
            # warn "List -- ". $i->is_contiguous;
            die "Not implemented: instantiate lazy slice using a non-contiguous list"
                unless $i->is_contiguous;
            my $start = $i->start;
            my $end =   $i->end;
            die "Slice start/end is not defined"
                unless defined $end && defined $start;
            die "Not implemented: instantiate lazy slice using a reversed list"
                unless $end >= $start;
            #warn "write_thru: Slicing from position $pos to ( $start .. $end )\n";
            #warn "    Index: ". $i->str . "\n";
            # items should be cloned before storing
            my $slice = $other->splice( $pos, ( $end - $start + 1 ) );
            my $elems = $slice->elems->unboxed;
            # $other->splice( $pos, 0, $slice );  # does this make any difference?
            
            my @items = $slice->unboxed->items;
            # @items = map {
            #            UNIVERSAL::isa( $_, 'Perl6::Value::List' ) ? $_->clone : $_
            #        } @items;
            
            if ( $elems < ( $end - $start + 1 ) ) {
                my $diff = $end - $start + 1 - $elems;
                #warn "Missing $diff elements";
                push @items, Perl6::Value::List->from_x( item => undef, count => $diff )
                    if $diff > 0;
            }
            #warn "Storing to $start";
            $ary->splice( $start, ( $end - $start + 1 ), @items );
            $pos = $pos + $end - $start + 1;
            #warn "pos = $pos";
        }
        else {
            # non-lazy slicing
            my $p = $self->{slice}->fetch( $pos )->fetch;
            $p = $p->unboxed if ref( $p );
            #warn "P $pos = $p";
            next unless defined $p;
            next if $p >= $self->{array}->elems()->unboxed || $p < 0;
            my $tmp = $other->fetch( $pos )->fetch;
            $ary->store( $p, $tmp );
            #warn "pos $pos - store $tmp to $p";
            $pos++;            
        }
    }
    return;
}

# ------ end Perl6::Slice -----

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

            # @a := @b 
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
            'tied' =>     sub { _('$:cell')->{tied} },

             # See perl5/Perl6-MetaModel/t/14_AUTOLOAD.t  
            'isa' => sub { ::next_METHOD() },
            'unboxed' => sub { 
                _('$:cell')->{tied} ? _('$:cell')->{tied} : _('$:cell')->{v}
            },
            'slice' => sub {
                # Returns an array whose fetch/store are bound to this array
                my ( $self, $list ) = @_;
                # given $list = (4,5,6)
                # given an index $i = 1
                #   get $list[$i] == 5
                #   ignore request if index == undef
                #   store/fetch from array[$list[$i]] == array[5]
                my $ret = Array->new();
                $ret->cell->{tieable} = 1;
                my $proxy = Perl6::Slice->new( 
                    array => $self, 
                    slice => $list,   
                );
                $ret->tie( $proxy );
                return $ret;
            },
            'AUTOLOAD' => sub {
                my ($self, @param) = @_;
                my $method = ::AUTOLOAD($self);
                my $tmp = $self->unboxed;
                # warn ref($tmp), ' ', $method, " @param == " . $tmp->$method( @param );
                
                @param = 
                    map {
                        UNIVERSAL::isa( $_, 'List' ) ? $_->unboxed : $_ 
                    } @param;

                if ( $method eq 'clone' || $method eq 'splice' || $method eq 'reverse' ) {
                    my @p;
                    #warn "-- $method";
                    for ( @param ) {
                        # my @items = ($_);
                        $_ = $_->unboxed if UNIVERSAL::isa( $_, 'Array' );
                        if ( UNIVERSAL::isa( $_, 'Perl6::Container::Array' ) ) {
                            push @p, $_->items;
                            #warn "    SPLICE $_ - @p"; 
                            next;
                        }
                        #warn "    SPLICE $_ "; 
                        push @p, $_;
                    }
                    my $ret = Array->new();
                    $ret->push( $tmp->$method( @p )->items );
                    return $ret;
                }
                
                if ( $method eq 'push'   || $method eq 'unshift' || $method eq 'store' ) {
                    #warn "STORING THINGS $method @param";
                    if ( $method eq 'store' && @param == 1 ) {
                        # whole Array store
                        # warn "WHOLE ARRAY STORE";
                        # XXX - what if the array is tied?
                        #  @a = (2,3,4,5); @a[1,2] = @a[0,3]
                        my $other = $param[0];
                        # if ( $self->cell->{tied} ||
                        #      $other->cell->{tied} ) 
                        
                        # if ( $other->is_infinite->unboxed ) {
                        #    die "Infinite slices and tied arrays are not yet fully supported";
                        # }

                        if ( UNIVERSAL::isa( $other, 'Array' ) ) {
                            if ( UNIVERSAL::isa( $other->tied, 'Perl6::Slice' ) ) {
                                # unbind the slice from the original arrays
                                $other = $other->tied->unbind;
                            }
                        }
                        else {
                            my $tmp = Array->new();
                            $tmp->push( $other );
                            $other = $tmp;
                        }

                        my @items = $other->unboxed->items;  
                        if ( UNIVERSAL::isa( $self->tied, 'Perl6::Slice' ) ) {
                            $self->tied->write_thru( $other );
                            return $self;
                        }
                        # warn "got @items - current = ". _('$:cell')->{v};

                        # unbind cells
                        @items = map {
                                ( ref($_) eq 'Scalar' ) ? $_->fetch : $_
                            } @items;

                        my $ret = Perl6::Container::Array->from_list( @items );
                        _('$:cell')->{v} = $ret;
                        return $self;
                    }

                    $tmp->$method( @param );
                    return $self;
                }

                if ( $method eq 'fetch' ) {
                    # warn "FETCHING THINGS @param";
                    if ( @param == 0 ) {
                        # whole Array fetch
                        return $self;
                    }
                    my $elem = $tmp->$method( @param );
                    my $scalar;
                    if ( UNIVERSAL::isa( $elem, 'Scalar' ) ) {
                        # warn "FETCHED CELL IS A SCALAR: $elem";
                        $scalar = $elem;
                    }
                    else
                    {
                        # warn "FETCHED CELL IS NOT YET A SCALAR: $elem";
                        $scalar = Scalar->new();
                        $scalar->store( $elem );
                        # replace Value with Scalar
                        #warn "STORE = $_[1], $scalar";
                        # XXX - this will break if it were a multi-dim fetch
                        $self->store( $_[1], $scalar );
                    }
                    my $ret = Scalar->new();
                    $ret->bind( $scalar );
                    return $ret;
                }

                if ( $method eq 'pop'   || $method eq 'shift' ) {
                    my $elem = $tmp->$method( @param );
                    unless ( UNIVERSAL::isa( $elem, 'Scalar' ) ) {
                        # XXX - I think only fetch() need to return Scalar 
                        my $scalar = Scalar->new();
                        $scalar->store( $elem );
                        return $scalar;
                    }
                    return $elem;
                }

                if ( $method eq 'elems' || $method eq 'int' || $method eq 'num' ) {
                    return Int->new( '$.unboxed' => $tmp->elems( @param ) )
                }
                if ( $method eq 'exists' ) {
                    # TODO - recursive to other dimensions
                    return Bit->new( '$.unboxed' => ($tmp->elems > $param[0] ) )
                }
                if ( $method eq 'is_infinite' ) {
                    return Bit->new( '$.unboxed' => $tmp->$method( @param ) )
                }
                
                return $tmp->$method( @param );
            },
            
            str => sub {
                my $array = shift;
                my %param = @_;
                my $samples = $param{'max'};
                my $self = _('$:cell')->{tied} ? _('$:cell')->{tied} : _('$:cell')->{v};
                $samples-- if defined $samples;
                $samples = 100 unless defined $samples || $self->is_infinite; 
                $samples = 2   unless defined $samples;
                my @start;
                my @end;
                my $tmp;
                for ( 0 .. $samples ) {
                    no warnings 'numeric';
                    last if $_ >= $self->elems;
                    $tmp = $self->fetch( $_ );
                    last if $tmp == &Inf;
                    push @start, $tmp;
                    last if $tmp == -&Inf;
                }
                for ( map { - $_ - 1 } 0 .. $samples ) {
                    no warnings 'numeric';
                    last unless $self->elems + $_ > scalar @start;
                    $tmp = $self->fetch( $_ );
                    last if $tmp == -&Inf;
                    unshift @end, $tmp;
                    last if $tmp == &Inf;
                }
                my $str = '';
                if ( @start > 0 ) {
                    if ( $self->elems == ( scalar @start + scalar @end ) ) {
                        $str =  join( ', ', map { Perl6::Value::stringify($_) } @start, @end );
                    }
                    else {
                        $str =  join( ', ', map { Perl6::Value::stringify($_) } @start ) .
                                ' ... ' . 
                                join( ', ', map { Perl6::Value::stringify($_) } @end );
                    }
                }
                return Str->new( '$.unboxed' => '(' . $str . ')' );                
            },
            perl => sub { my $self = shift; $self->str( @_ ) },
        },
    }
};

# ----- unboxed functions

package Perl6::Container::Array;

use strict;
use Perl6::Value;
use Perl6::Value::List;
use Carp;

use constant Inf => Perl6::Value::Num::Inf;

sub new {
    my $class = shift;
    my %param = @_;
    my @items = @{$param{items}};
    # warn "-- new -- @items --";
    return bless { items => \@items }, $class;
}

sub clone { 
    # TODO - clone Scalars
    my $self = bless { %{ $_[0] } }, ref $_[0];
    @{$self->{items}} = map {
            UNIVERSAL::isa( $_, 'Perl6::Value::List' ) ? $_->clone : $_
        } @{$self->{items}};
    return $self;
}

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
        my $len = $array->elems;
        @{$array->{items}} = ();
        return ( $len, @tmp );
    }
    my $ret_length = 0;
    while ( @tmp ) {
        # warn "ret $ret_length == ".scalar(@ret)." length $length";
        last if $ret_length >= $length;
        if ( ! UNIVERSAL::isa( $tmp[0], 'Perl6::Value::List') ) {
            push @ret, shift @tmp;
            $ret_length++;
            # warn "push elem @ret[-1]";
            next;
        }
        if ( $tmp[0]->elems > 0 ) {
            # my $i = $tmp[0]->shift;
            my $li = $tmp[0];
            my $diff = $length - $ret_length;
            my $i = $li->shift_n( $diff );
            push @ret, $i;
            if ( UNIVERSAL::isa( $i, 'Perl6::Value::List') ) {
                $ret_length += $i->elems;
            }
            else {
                $ret_length++;
            }
            # warn "push list ". $i->start . ".." . $i->end . " now length=$ret_length";
            last if $ret_length >= $length;
        }
        else {
            shift @tmp;
        }
    };
    @{$array->{items}} = @tmp;
    # warn "ret @ret ; array @tmp ";
    return ( $ret_length, @ret );
}

sub _pop_n {
    my $array = shift;
    my $length = shift;
    my @ret;
    my @tmp = @{$array->{items}};
    if ( $length == Inf ) {
        my $len = $array->elems;
        @{$array->{items}} = ();
        return ( $len, @tmp );
    }
    my $ret_length = 0;
    while ( @tmp ) {
        # warn "ret ".scalar(@ret)." length $length";
        last if $ret_length >= $length;
        if ( ! UNIVERSAL::isa( $tmp[-1], 'Perl6::Value::List') ) {
            unshift @ret, pop @tmp;
            $ret_length++;
            next;
        }
        if ( $tmp[-1]->elems > 0 ) {
            # my $i = $tmp[-1]->pop;
            # unshift @ret, $i;
            my $li = $tmp[-1];
            my $diff = $length - $ret_length;
            my $i = $li->pop_n( $diff );
            unshift @ret, $i;
            if ( UNIVERSAL::isa( $i, 'Perl6::Value::List') ) {
                $ret_length += $i->elems;
            }
            else {
                $ret_length++;
            }
            # warn "pop list ". $i->start . ".." . $i->end . " now length=$ret_length";
            last if $ret_length >= $length;
        }
        else {
            pop @tmp;
        }
    };
    @{$array->{items}} = @tmp;
    # warn "ret @ret ; array @tmp ";
    return ( $ret_length, @ret );
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
    my $class = ref($array);
    my @list = @_;
    my ( @head, @body, @tail );
    my ( $len_head, $len_body, $len_tail );
    # print "items: ", $array->items, " splice: $offset, $length, ", @list, "\n";
    # print 'insert: ', $_, ' ', $_->ref for @list, "\n";
    # print " offset $offset length $length \n";
    if ( $offset >= 0 ) {
        ( $len_head, @head ) = $array->_shift_n( $offset );
        if ( $length >= 0 ) {
            #  head=shift offset -> body=shift length -> tail=remaining
            ( $len_body, @body ) = $array->_shift_n( $length );
            ( $len_tail, @tail ) = $array->_shift_n( Inf );
        }
        else {
            #  tail=pop length -> head=shift offset -> body=remaining 
            ( $len_tail, @tail ) = $array->_pop_n( -$length );
            ( $len_body, @body ) = $array->_shift_n( Inf );
        }
    }
    else {
        ( $len_tail, @tail ) = $array->_pop_n( -$offset );
        ( $len_head, @head ) = $array->_shift_n( Inf );
        if ( $length >= 0 ) {
            # negative offset, positive length
            #  tail=pop length -> head=remaining -> body=shift tail until body == length
            # make $#body = $length
            my $tail = $class->from_list( @tail );
            ( $len_body, @body ) = $tail->_shift_n( $length );
            @tail = $tail->items;
        }
        else {
            # negative offset, negative length
            #  tail=pop length -> head=remaining -> body=shift tail until tail == length
            # make $#tail = -$length
            my $body = $class->from_list( @tail );
            ( $len_tail, @tail ) = $body->_pop_n( -$length );
            @body = $body->items;
        }
    };
    # print "off: $offset len: $length head: @head body: @body tail: @tail list: @list\n";
    @{$array->{items}} = ( @head, @list, @tail );
    return $class->from_list( @body );
}

sub end  {
    my $array = shift;
    return unless $array->elems;
    my $x = $array->pop;
    $array->push( $x );
    return $x;
}

sub fetch {
    # XXX - this is inefficient because it needs 2 splices
    # see also: splice()
    my $array = shift;
    my $pos = shift;
    
    #use Data::Dumper;
    #warn "-- array -- ". Dumper( $array );
    return unless defined $pos;   # XXX - undefined == zero?
    return if $pos >= $array->elems;

    my $ret = $array->splice( $pos, 1 );
    ($ret) = @{$ret->{items}};
    $ret = $ret->shift if UNIVERSAL::isa( $ret, 'Perl6::Value::List' );
    if ( $pos < 0 ) {
        if ( $pos == -1 ) {
            $array->push( $ret );
        }
        else {
            $array->splice( $pos+1, 0, $ret );
        }
    }
    else {
        $array->splice( $pos, 0, $ret );
    }
    # warn "FETCH $pos returns $ret";
    return $ret;
}

sub store {
    my $array = shift;
    my $pos = shift;
    my $item  = shift;
    if ( UNIVERSAL::isa( $item, 'Perl6::Value::List') ) {
        my $class = ref($array);
        $item = $class->new( items => [$item] );
    }
    if ( $pos <= $array->elems ) {
        # XXX - TODO - if the cell is bound, the binding must be kept
        $array->splice( $pos, 1, $item );
        return $array;
    }
    # store after the end 
    my $fill = Perl6::Value::List->from_x( item => undef, count => ( $pos - $array->elems ) );
    push @{$array->{items}}, $fill, $item;
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
    my ( $length, $ret ) = $array->_pop_n( 1 );
    # warn "POP $length -- ". $ret->elems if UNIVERSAL::isa( $ret, 'Perl6::Value::List' );
    $ret = $ret->shift if UNIVERSAL::isa( $ret, 'Perl6::Value::List' );
    return $ret;
}

sub shift {
    my $array = shift;
    my ( $length, $ret ) = $array->_shift_n( 1 );
    # warn "SHIFT $length -- ". $ret->elems if UNIVERSAL::isa( $ret, 'Perl6::Value::List' );
    $ret = $ret->shift if UNIVERSAL::isa( $ret, 'Perl6::Value::List' );
    return $ret;
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
