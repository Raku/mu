
# This is a Perl5 file

# ChangeLog
#
# 2005-08-17
# - New methods: .access('ro'); .access('rw'); .bind( $scalar )
# - reimplemented Scalar - auto-deref; new methods .fetch, .store, .unboxed
#
# 2005-08-15
# - created the 'Scalar' container

# TODO - test Scalar auto-deref with a lazy List
# TODO - verify .ref() and .undefine() implementations
# TODO - .ref() should be inherited from Object
# TODO - .meta should give access to .name, etc
# TODO - 'is readonly'
# TODO - dispatch .grep, .map, ... to the Value - see AUTOMETH
# TODO - store(Scalar) => store(Scalar->value)
# TODO - allow fetch/store unboxed contents
# TODO - Constant class
# TODO - Scalar id is stored in the Cell

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
#    id
#
# 'tieable' can be reused as a pointer to the tied object

$Perl6::Cell::_id = rand;
sub Perl6::Cell::new { bless { 'id' => ++$Perl6::Cell::_id }, 'Perl6::Cell' } 
sub Perl6::Cell::store {
    die 'read only cell' if $_[0]{ro} && defined $_[0]{v};
    $_[0]{v} = $_[1]
}
sub Perl6::Cell::fetch {
    $_[0]{v}
}
sub Perl6::Cell::tie {
    die 'untieable cell' if ! $_[0]{tieable};
    $_[0]{tieable} = $_[1]
}
sub Perl6::Cell::untie {
    $_[0]{tieable} = 1;
}

# --- end Cell

# quick hack until we get AUTOMETH working
# - not proxied methods: id value defined undefine fetch store
my %ref_AUTOMETH = map {
        my $method = $_;
        ( $method => sub { 
            my $tmp = _('$:cell')->fetch;
            my @param = @_;
            shift @param;
            if ( defined $tmp ) {
                if ( $method eq 'increment' || $method eq 'decrement' ) {
                    _('$:cell')->store( $tmp->$method( @param ) );
                    return SELF; 
                }
                return $tmp->$method( @param );
            }
            else {
                # empty cell
                return Bit->new( '$.unboxed' => 0 ) if $method eq 'bit';  # XXX ?
                return CLASS if $method eq 'ref';
                return Str->new( '$.unboxed' => '\\undef' ) if $method eq 'perl';
                if ( $method eq 'increment' ) {
                    _('$:cell')->store( Int->new( '$.unboxed' => 1 ) );
                    return SELF 
                }
                if ( $method eq 'decrement' ) {
                    _('$:cell')->store( Int->new( '$.unboxed' => -1 ) );
                    return SELF 
                }
                return if $method eq 'unboxed';
                die "Method not found: .$method";
            }
        } )
    } 
    qw( num int str bit perl ref map grep increment decrement 
        shift pop unshift push key value unboxed referred );

class 'Scalar'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ [ '$:cell' => { 
                        access => 'rw', 
                        build => sub { bless {}, 'Perl6::Cell' } } ] ],
        DESTROY => sub {
            # XXX - didn't undefine the value 
            # _('$.value' => undef) },
            $_[0]->{'instance_data'}{'$:cell'} = undef;  
        },
        methods => { 
            %ref_AUTOMETH,
            'fetch' => sub { _('$:cell')->fetch },
            'store' => sub { my ( $self, $value ) = @_; _('$:cell')->store($value ) },
            'defined' => sub {
                my $def = defined _('$:cell')->fetch ? 1 : 0;
                Bit->new( '$.unboxed' => $def )
            },
            'undefine' => sub {
                # XXX - didn't undefine the value 
                # _('$.value' => undef) },
                my $self = shift;
                _('$:cell')->store( undef );
                return $self;
            },
            'access' => sub {
                die "access must be 'ro' or 'rw'"
                    if $_[1] ne 'ro' && $_[1] ne 'rw';
                _('$:cell')->{ro} = $_[1] eq 'ro';
                return SELF;
            },
            'bind' => sub {
                my ( $self, $scalar ) = @_;
                die "argument to bind() must be a Scalar"
                    unless $scalar->isa( 'Scalar' );
                _('$:cell', $scalar->_cell);
                return $self;
            },
            '_cell' => sub { _('$:cell') },  # _cell() is used by bind()
            'id' => sub { _('$:cell')->{id} },  
            'tieable' => sub { _('$:cell')->{tieable} != 0 },
            # TODO - tie, untie; return the proper results
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

Flavio S. Glock, E<lt>fglock@Egmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.


=cut
