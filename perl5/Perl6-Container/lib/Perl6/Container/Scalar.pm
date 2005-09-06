
# This is a Perl5 file

# ChangeLog
#
# 2005-09-06
# - tied()
#
# 2005-08-25
# - Arrays stored in a Scalar can be accessed using indexed store/fetch
#
# 2005-08-23
# - fixed Perl6::Cell::fetch
#
# 2005-08-19
# - Added roles (traits) - tieable, readonly
# - Removed methods: .set_tieable(), .access()
#   These functions will be provided by traits. See t/trait.t.
#   Methods .tieable, .tie, .untie were kept, in order to keep 
#   bound scalars tieable.
#   ._cell was promoted to public method, in order to keep .bind
#   working after a trait is applied.
#
# 2005-08-18
# - New methods: .tie($object), .untie, .tieable
# - Method .id() returns the Cell id.
#
# 2005-08-17
# - New methods: .access('ro'); .access('rw'); .bind( $scalar )
# - reimplemented Scalar - auto-deref; new methods .fetch, .store, .unboxed
#
# 2005-08-15
# - created the 'Scalar' container

# TODO - lvalue substr

# TODO - tied() method
# TODO - test tied Scalar
# TODO - test 'readonly' and 'tieable' using traits - Scalar, Hash and Array
# TODO - test Scalar auto-deref with a lazy List
# TODO - verify .ref() and .undefine() implementations
# TODO - .ref() should be inherited from Object
# TODO - .meta should give access to .name, etc
# TODO - 'is readonly'
# TODO - store(Scalar) => store(Scalar->value)
# TODO - store(Hash) / fetch
# TODO - Constant class

use strict;

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Value;

my $class_description = '-0.0.1-cpan:FGLOCK';

# Cell
#
# Cell is implemented as a native class
#
# cell keys:  
#    v  (value)   (default: undef)
#    ro           (default: r/w; 1=read-only cell)
#    tieable      (default: non-tieable; 1=tieable)
#    tied         (default: undef; can be set to an object)
#    type         (default: undef; can be set to 'Hash' or 'Array')
#    id

$Perl6::Cell::_id = rand;
sub Perl6::Cell::new { bless { 'id' => ++$Perl6::Cell::_id }, 'Perl6::Cell' } 
sub Perl6::Cell::store {
    # warn "Perl6::Cell::store tied=$_[0]{tied} type=$_[0]{type} ref=".ref($_[0]{v})." param=@_";
    die 'read only cell' if $_[0]{ro} && defined $_[0]{v};
    # return $_[0]{tied}->store($_[1]) if $_[0]{tied};
    if ( $_[0]{tied} ) {
        my $cell = shift;
        return $cell->{tied}->store( @_ );
    }
    if ( @_ > 2 ) {
           # && (
           # UNIVERSAL::isa( $_[0]{v}, 'Array' ) || 
           # UNIVERSAL::isa( $_[0]{v}, 'Hash'  )
           # ))  {
        my $cell = shift;
        # warn "storing @_ to ".ref($cell->{v});
        # warn "Cell is not yet defined, and you are trying to access position $_[0] with @_" unless defined $cell->{v};
        return $cell->{v}->store( @_ );
    }
    $_[0]{v} = $_[1]
}
sub Perl6::Cell::fetch {
    # warn "Perl6::Cell::fetch tied=$_[0]{tied} type=$_[0]{type} ref=".ref($_[0]{v})." param=@_";
    # return $_[0]{tied}->fetch if $_[0]{tied};
    if ( $_[0]{tied} ) {
        my $cell = shift;
        return $cell->{tied}->fetch( @_ );
    }
    if ( @_ > 1 ) {
           # && (
           # UNIVERSAL::isa( $_[0]{v}, 'Array' ) || 
           # UNIVERSAL::isa( $_[0]{v}, 'Hash'  )
           # ))  {
        my $cell = shift;
        #warn "fetching @_ from ".ref($cell->{v});
        return $cell->{v}->fetch( @_ );
    }
    $_[0]{v}
}
sub Perl6::Cell::tie {
    die 'untieable cell' if ! $_[0]{tieable};
    # TODO - use extra parameters
    $_[0]{tied} = $_[1];
}
sub Perl6::Cell::untie {
    $_[0]{tied} = undef;
}

# --- end Cell

# tieable, readonly
#
# These are Perl6 roles
# See: S06 - Implementation types
# See: t/trait.t
#

role tieable => {
    methods => {
        'tieable' => sub { 
            _('$:cell')->{tieable} = 1; 
            1;
        }, 
        'tie' =>     sub { 
            shift; 
            _('$:cell')->{tieable} = 1; 
            _('$:cell')->tie(@_); 
        },
        'untie' =>   sub { 
            _('$:cell')->untie;
        },
    }
};

role readonly => {
    methods => {
        'store' => sub { 
            my ( $self, $value ) = @_; 
            _('$:cell')->{ro} = 1;
            _('$:cell')->store($value ) 
        },
    }
};

# Scalar 
#
# Scalar is implemented as a Perl6 class
#

class 'Scalar'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ [ '$:cell' => { 
                        access => 'rw', 
                        build => sub { Perl6::Cell->new } } ] ],
        DESTROY => sub {
            # XXX - didn't undefine the value 
            # _('$.value' => undef) },
            $_[0]->{'instance_data'}{'$:cell'} = undef;  
        },
        methods => { 
            'fetch' => sub { shift; _('$:cell')->fetch( @_ ) },
            'store' => sub { 
                # TODO - copy cell 'type'
                shift; 
                _('$:cell')->store( @_);
            },
            'defined' => sub {
                Bit->new( '$.unboxed' => defined _('$:cell')->fetch ? 1 : 0 )
            },
            'undefine' => sub {
                # XXX - didn't undefine the value 
                # _('$.value' => undef) },
                my $self = shift;
                _('$:cell')->store( undef );
                # what happens if $x was bound to %h ???
                _('$:cell')->{type} = undef;
                return $self;
            },

            # There is no 'set_access' - use 'is Readonly' instead
            # See: t/trait.t
            # 'access' => sub {
            #    die "access must be 'ro' or 'rw'"
            #        if $_[1] ne 'ro' && $_[1] ne 'rw';
            #    _('$:cell')->{ro} = $_[1] eq 'ro';
            #    return ::SELF;
            # },

            'bind' =>    sub {
                my ( $self, $scalar ) = @_;
                die "argument to bind() must be a Scalar"
                    unless $scalar->isa( 'Scalar' );
                _('$:cell', $scalar->cell);
                return $self;
            },
            'cell' =>    sub { _('$:cell') },  # _cell() is used by bind()
            'id' =>      sub { _('$:cell')->{id} },  

            # There is no 'set_tieable' - use 'is Tieable' instead
            # See: t/trait.t
            # 'set_tieable' => sub { _('$:cell')->{tieable} = 1 },

            'tieable' => sub { _('$:cell')->{tieable} != 0 },
            'tie' =>     sub { shift; _('$:cell')->tie(@_) },
            'untie' =>   sub { _('$:cell')->untie },
            'tied' =>    sub { _('$:cell')->{tied} },

             # See perl5/Perl6-MetaModel/t/14_AUTOLOAD.t  
            'isa' =>     sub { ::next_METHOD() },
            'does' =>    sub { ::next_METHOD() },
            'AUTOLOAD' => sub {
                my ($self, @param) = @_;
                my $method = ::AUTOLOAD($self);
                my $tmp = _('$:cell')->fetch;

                if ( defined $tmp ) {
                    if ( $method eq 'increment' || $method eq 'decrement' ) {
                        _('$:cell')->store( $tmp->$method( @param ) );
                        return ::SELF; 
                    }
                    # warn "calling Scalar -> $method on $tmp ";
                    return Str->new( '$.unboxed' => $tmp ) unless ref( $tmp );   # unboxed
                    return $tmp->$method( @param );
                }
                else {
                    # empty cell
                    return Bit->new( '$.unboxed' => 0 ) if $method eq 'bit';  # XXX ?
                    return ::CLASS if $method eq 'ref';
                    return Str->new( '$.unboxed' => '\\undef' ) if $method eq 'perl';
                    if ( $method eq 'increment' ) {
                        _('$:cell')->store( Int->new( '$.unboxed' => 1 ) );
                        return ::SELF 
                    }
                    if ( $method eq 'decrement' ) {
                        _('$:cell')->store( Int->new( '$.unboxed' => -1 ) );
                        return ::SELF 
                    }
                    return Str->new( '$.unboxed' => 'undef' ) if $method eq 'str';
                    return if $method eq 'unboxed';
                    die "Trying to call method \"$method\" in an undefined Scalar";
                }
            },
        },
    }
};

__END__


=head1 NAME

Perl6::Container::Scalar - Perl extension for Perl6 "Scalar" class

=head1 SYNOPSIS

  use Perl6::Container::Scalar;

  ...

=head1 DESCRIPTION

...


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
