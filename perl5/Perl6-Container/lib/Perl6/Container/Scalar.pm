
# This is a Perl5 file

# ChangeLog
#
# 2005-08-17
# - reimplemented Scalar - auto-deref; new methods .fetch, .store, .unboxed
#
# 2005-08-15
# - created the 'Scalar' container

# TODO - verify .ref() and .undefine() implementations
# TODO - .ref() should be inherited from Object
# TODO - 'is readonly'
# TODO - can a Scalar hold an unboxed value?
# TODO - .key, .value => dispatch to Value->key, Value->value
# TODO - dispatch .grep, .map, ... to the Value - see AUTOMETH
# TODO - store(Scalar) => store(Scalar->value)
# TODO - allow fetch/store unboxed contents
# TODO - 'value' is a Pair method - name 'fetch'/'store' instead
# TODO - create a 'undef' object?
# TODO - bind ':='
# TODO - Constant class

use strict;

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Value;

my $class_description = '-0.0.1-cpan:FGLOCK';

# quick hack until we get AUTOMETH working
# - not proxied methods: id value defined undefine fetch store
my %ref_AUTOMETH = map {
        my $method = $_;
        ( $method => sub { 
            my $tmp = _('$:cell');
            my @param = @_;
            shift @param;
            if ( defined $tmp ) {
                if ( $method eq 'increment' || $method eq 'decrement' ) {
                    _('$:cell', $tmp->$method( @param ) );
                    return SELF; 
                }
                return _('$:cell')->$method( @param );
            }
            else {
                # empty cell
                return Bit->new( '$.unboxed' => 0 ) if $method eq 'bit';  # XXX ?
                return CLASS if $method eq 'ref';
                return Str->new( '$.unboxed' => '\\undef' ) if $method eq 'perl';
                if ( $method eq 'increment' ) {
                    _('$:cell', Int->new( '$.unboxed' => 1 ) );
                    return SELF 
                }
                if ( $method eq 'decrement' ) {
                    _('$:cell', Int->new( '$.unboxed' => -1 ) );
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
        attrs => [ [ '$:cell' => { access => 'rw' } ] ],
        DESTROY => sub {
            # XXX - didn't undefine the value 
            # _('$.value' => undef) },
            my $self = shift;
            $self->{'instance_data'}{'$:cell'} = undef;  # XXX
        },
        methods => { 
            %ref_AUTOMETH,
            'fetch' => sub { _('$:cell') },
            'store' => sub { my ( $self, $value ) = @_; _('$:cell', $value ) },
            'defined' => sub {
                my $def = defined _('$:cell') ? 1 : 0;
                Bit->new( '$.unboxed' => $def )
            },
            'undefine' => sub {
                # XXX - didn't undefine the value 
                # _('$.value' => undef) },
                my $self = shift;
                $self->{'instance_data'}{'$:cell'} = undef;  # XXX
                return $self;
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

Flavio S. Glock, E<lt>fglock@Egmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.


=cut
