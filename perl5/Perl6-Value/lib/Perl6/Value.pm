#
# - Value classes 
# Num, Int, Str, Bit, Pair, Ref, List
#
# - functions for implementation of unboxed Perl6 Values in Perl5
# Perl6::Value::Num - Inf, NaN
# Perl6::Value::Int
# Perl6::Value::Str
# Perl6::Value::Bit - bool::* taint::*
# Perl6::Value::List (separate file)

# ChangeLog
#
# 2005-09-03
# * Fixed Pair stringification
#
# 2005-09-01
# * Added support for unboxed Pair.key, Pair.value
#
# 2005-08-18
# * clean-up Ref implementation - uses AUTOLOAD to auto-deref
# * added unboxed enums: bool::* taint::*
# * 'List' clean up - removed 'multisub' methods
#
# 2005-08-17
# * added boxed type: List (lazy, non-lazy, infinite)
# * More descriptive method names:
#   - .unboxed() - returns the unboxed value
#     Pair->unboxed returns two objects.
#   - Ref->referred - returns the referred object
#   - removed method .value() - except for Pair
#
# 2005-08-16
# * 'Scalar' temporarily implemented here 
# * added class Ref
# * added method .perl()
#
# 2005-08-15
# * added boxed types: Num, Int, Str, Bit, Pair
#
# 2005-08-13
# * refactored from Perl6::Value::List

# Notes:
# - All library functions - add, subtract, sqrt, ...
#   are implemented using multisubs
# - 'Ref' do not auto-deref List or any other Value - only Containers

# TODO - XXX - perl5 doesn't support string decrement (neither Pugs)

# TODO - implement tests from t/var/autoderef.t

# TODO - constant

# TODO - move .increment, .decrement to Primitives (this will break some tests)
# TODO - move Num::Inf, Num::NaN to Primitives

use strict;

use Perl6::MetaModel;

sub class1 {
    my ($name, $params) = @_;
    my $barename = $name; $barename =~ s/-0.*//;
    my $code="";
    $code .= "package $barename;\n";
    $code .= "our \$MM = \$::Class->new('\$:name' => '$barename');\n";
    $code .= "sub new { shift; \$MM->new(\@_) }\n";
    #$code .= "sub new { my \$c = shift; my \$o = \$MM->new(\@_); warn \"new: \$c  \$o\"; \$o; }\n";
    $code .= "\$MM;";
    my $MM = eval($code); die "$@\n$code" if $@;
    for my $key (keys %$params) {
	my $val = $params->{$key};
	if($key eq 'is') {
	    my $sc = [map{ref($_)?$_:do{no strict;my $n=$_."::MM";$$n}}@$val];
	    #print STDERR "$barename  @$sc\n";
	    $MM->superclasses($sc);
	} elsif($key eq 'class') {
	    for my $k (keys %$val) {
		my $v = $val->{$k};
		if($k eq 'methods') {
		    foreach my $m (keys %$v) {
			#warn "$barename $m\n";
			$MM->add_method($m => ::make_class_method($v->{$m}));
		    }
		}
	    }
	} elsif($key eq 'instance') {
	    for my $k (keys %$val) {
		my $v = $val->{$k};
		if($k eq 'attrs') {
		    #print STDERR "$barename  @$v\n";
		    foreach my $a (@$v) {
			my $a_config = {};
			if(ref($a)) {
			    $a_config = $a->[1];
			    $a = $a->[0];
			}
			my $am = $a; $am =~ s/\$.//;
			$MM->add_attribute($a => ::make_attribute($a));
			my $access = $a_config->{'access'};
			my $build  = $a_config->{'build'};
			my $is_rw = $access && $access eq 'rw';
			# XXX - everything treated as rw;
			$MM->add_method($am => ::make_method(sub {
			    my $self = shift;
			    ::opaque_instance_attr($self => $a) = shift if @_;
			    ::opaque_instance_attr($self => $a);
			}));
			if($build) {
			    $MM->add_method('BUILD' => ::make_submethod(sub{
				shift;
				_($a => $build->());
			    }));
			}
		    }
		} elsif($k eq 'DESTROY') {
		    $MM->add_method('DESTROY' => ::make_method(sub{
			shift;
			$v->();
		    }));
		} elsif($k eq 'methods') {
		    foreach my $m (keys %$v) {
			#warn "$barename $m\n";
			$MM->add_method($m => ::make_method($v->{$m}));
		    }
		}
	    }
	}
    }
    $MM;
}



my $class_description = '-0.0.1-cpan:FGLOCK';

class1 'Num'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {
            # TODO - pi ?
            'Inf' => sub {
                my ($class) = @_;           
                return $class->new( '$.unboxed' => &Perl6::Value::Num::Inf );
            },
            'NaN' => sub {
                my ($class) = @_;           
                return $class->new( '$.unboxed' => &Perl6::Value::Num::NaN );
            },
        }
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { $::SELF },
            'int' =>  sub { Int->new( '$.unboxed' => Perl6::Value::Num::to_int( _('$.unboxed') ) ) },
            'str' =>  sub { Str->new( '$.unboxed' => Perl6::Value::Num::to_str( _('$.unboxed') ) ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => Perl6::Value::Num::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { $::SELF->str },
            'increment' => sub { 
                my $value = _('$.unboxed');
                $::SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                $::SELF->ref->new( '$.unboxed' => --$value ) },
            'ref' => sub { $::CLASS }, 
        },
    }
};

class1 'Int'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { Num->new( '$.unboxed' => Perl6::Value::Int::to_num( _('$.unboxed') ) ) },
            'int' =>  sub { $::SELF },
            'str' =>  sub { Str->new( '$.unboxed' => Perl6::Value::Int::to_str( _('$.unboxed') ) ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => Perl6::Value::Int::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { $::SELF->str },
            'increment' => sub { 
                my $value = _('$.unboxed');
                $::SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                $::SELF->ref->new( '$.unboxed' => --$value ) },
            'ref' => sub { $::CLASS }, 
        },
    }
};

class1 'Str'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { Num->new( '$.unboxed' => Perl6::Value::Str::to_num( _('$.unboxed') ) ) },
            'int' =>  sub { Int->new( '$.unboxed' => Perl6::Value::Str::to_int( _('$.unboxed') ) ) },
            'str' =>  sub { $::SELF },
            'bit' =>  sub { Bit->new( '$.unboxed' => Perl6::Value::Str::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { 
                my $tmp = _('$.unboxed');
                $tmp =~ s/(\\|\')/\\$1/g;  # quote ' \
                return Str->new( '$.unboxed' => "'" . $tmp . "'" );
              },
            'increment' => sub { 
                my $value = _('$.unboxed');
                $::SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                # XXX - perl5 doesn't support string decrement
                $::SELF->ref->new( '$.unboxed' => --$value ) },
            'ref' => sub { $::CLASS }, 
        },
    }
};

class1 'Bit'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' => sub { Num->new( '$.unboxed' => Perl6::Value::Bit::to_num( _('$.unboxed') ) ) },
            'int' => sub { Int->new( '$.unboxed' => Perl6::Value::Bit::to_int( _('$.unboxed') ) ) },
            'str' => sub { Str->new( '$.unboxed' => Perl6::Value::Bit::to_str( _('$.unboxed') ) ) },
            'bit' => sub { $::SELF },
            'perl' => sub { $::SELF->str },
            'ref' => sub { $::CLASS }, 
        },
    }
};

class1 'Rat'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {},
    },
    instance => {
        attrs => [ '$.a', '$.b' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { Num->new( '$.unboxed' => _('$.a')/_('$.b') ) },
            'int' =>  sub { Int->new( '$.unboxed' => Perl6::Value::Num::to_int( _('$.a')/_('$.b') ) ) },
            'str' =>  sub { Str->new( '$.unboxed' => Perl6::Value::Num::to_str( _('$.a')/_('$.b') ) ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => Perl6::Value::Num::to_bit( _('$.a')/_('$.b') ) ) },
            'perl' => sub { $::SELF->str },
            'ref' =>  sub { $::CLASS }, 
        },
    }
};

class1 'Pair'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.key', '$.value' ],  # [ '$.value' => { access => 'rw' } ] ],
        DESTROY => sub {},
        methods => {
            'num' => sub { Num->new( '$.unboxed' => 0 ) },
            'int' => sub { Int->new( '$.unboxed' => 0 ) },
            'str' => sub { $_[0]->perl }, 
            'bit' => sub { Bit->new( '$.unboxed' => 0 ) },
            'perl' => sub { 
                my $self = shift;
                my $key =   Perl6::Value::stringify( $self->key );
                my $value = Perl6::Value::stringify( $self->value );
                Str->new( '$.unboxed' => "($key, $value)" ) 
              },
            'unboxed' => sub { ( _('$.key'), _('$.value') ) },
            'ref' => sub { $::CLASS }, 
            'isa' => sub { ::next_METHOD() },
            'does' => sub { ::next_METHOD() },
            'AUTOLOAD' => sub {
                my ($self, @param) = @_;
                my $method = __('$AUTOLOAD');
                die "unsupported pair method .$method";
            },

        },
    }
};

class1 'Ref'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.referred' ],
        DESTROY => sub {
            # XXX - didn't undefine the value 
            # _('$.cell' => undef) },
            my $self = shift;
            $self->{'instance_data'}{'$.referred'} = undef;  # XXX
        },
        methods => { 
             # See perl5/Perl6-MetaModel/t/14_AUTOLOAD.t  
            'isa' => sub { ::next_METHOD() },
            'does' => sub { ::next_METHOD() },
            'unboxed' => sub { \(_('$.referred')) },
            'AUTOLOAD' => sub {
                my ($self, @param) = @_;
                my $method = __('$AUTOLOAD');
                my $tmp = _('$.referred');
                # Array and Hash are auto-dereferenced
                if ( ref $tmp && ( 
                        $tmp->isa( 'Array' ) || $tmp->isa( 'Hash' )
                    ) ) {
                    return $tmp->$method( @param );
                }
                # everything else is not auto-dereferenced
                return Bit->new( '$.unboxed' => 1 ) 
                    if $method eq 'bit';
                return $::CLASS 
                    if $method eq 'ref';
                if ( $method eq 'perl' || $method eq 'str' ) {
                    return Str->new( '$.unboxed' => '\\' . Perl6::Value::stringify($tmp) ) 
                }
                return Bit->new( '$.unboxed' => defined $tmp ? 1 : 0 ) 
                    if $method eq 'defined';                
                die "unsupported ref method .$method";
            },
        },
    }
};

role Lazy => {
    methods => {}
};

role Eager => {
    methods => {}
};

class1 'List'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { Num->new( '$.unboxed' => _('$.unboxed')->num  ) },
            'int' =>  sub { Int->new( '$.unboxed' => _('$.unboxed')->int  ) },
            'str' =>  sub { Str->new( '$.unboxed' => _('$.unboxed')->str  ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => _('$.unboxed')->bit  ) },
            'perl' => sub { Str->new( '$.unboxed' => _('$.unboxed')->perl ) },
            'ref' =>  sub { $::CLASS }, 

            'shift' => sub { _('$.unboxed')->shift },
            'pop' =>   sub { _('$.unboxed')->pop   },

            # These methods are implemented in List.pm but are not exposed here,
            # they are going to be moved to Primitives:
            #     elems, is_infinite, is_lazy, is_contiguous, flatten, grep, ...
        },
    }
};

# ---------- Implementation of unboxed values --------------

#package Perl6::Value;
#
#sub to_ref {
#    my $object = shift;
#    my $class_name = ::dispatch( ::meta( $object ), 'name' );  # ** get the class name, from object
#    my $class = $Perl6::Class::ALL_CLASSES{$class_name};  # ** get the class, from class name
#    return $class;
#}

$Perl6::Value::obj_id = '**ObJecT**' . rand;
sub Perl6::Value::identify {
    my $key = shift;
    return $Perl6::Value::obj_id unless defined $key;
    my $s = $key;
    if (
        UNIVERSAL::isa( $key, 'Int' ) ||
        UNIVERSAL::isa( $key, 'Num' ) ||
        UNIVERSAL::isa( $key, 'Str' ) ||
        UNIVERSAL::isa( $key, 'Bit' ) ||
        UNIVERSAL::isa( $key, 'Rat' )
    ) {
        $s = $key->str->unboxed
    }
    elsif ( UNIVERSAL::can( $key, 'id' ) ) {
        $s = $Perl6::Value::obj_id . $key->id
    }
    return $s;
}

sub Perl6::Value::stringify {
    my $s = shift;
    $s = $s->fetch if UNIVERSAL::isa( $s, 'Scalar');
    my $tmp;
    # warn "stringify - $s\n";
    eval { $tmp = $s->str(max=>3) };
    $s = $tmp unless $@;
    # warn "   str - $s - $@\n";
    eval { $tmp = $s->unboxed };
    $s = $tmp unless $@;
    # warn "   unboxed - $s - $@\n";
    return 'undef' unless defined $s;
    no warnings 'numeric';
    $s = Perl6::Value::Num::to_str( $s ) if $s+0 eq $s;
    return $s;
}

sub Perl6::Value::numify {
    my $s = shift;
    $s = $s->fetch if UNIVERSAL::isa( $s, 'Scalar');
    my $tmp;
    eval { $tmp = $s->num };
    $s = $tmp unless $@;
    eval { $tmp = $s->unboxed };
    $s = $tmp unless $@;
    unless ( defined $s ) {
        warn "attempting to use 'undef' as a number";
        return 0;
    }
    no warnings 'numeric';
    $s = Perl6::Value::Str::to_num( $s ) if $s+0 ne $s;
    return $s;
}

package Perl6::Value::Num;

use constant Inf => 100**100**100;
use constant NaN => Inf / Inf;

sub to_str        { 
    my $v = shift;
    # native
    return 'Inf'  if $v eq "".&Perl6::Value::Num::Inf;
    return '-Inf' if $v eq "".(-&Perl6::Value::Num::Inf);
    return 'NaN'  if $v eq "".&Perl6::Value::Num::NaN;
    $v = 0 + $v;
    return 'Inf'  if $v == Inf;
    return '-Inf' if $v == -&Inf;
    return 'NaN'  if $v =~ m/n/i;
    return "" . $v 
}
sub to_bit        { $_[0] == 0 ? '' : 1 }
sub to_num        { 0 + $_[0] }
sub to_int        { int( $_[0] ) }

package Perl6::Value::Int;

sub to_str        { 
    no warnings 'uninitialized';
    my $v = 0 + $_[0];
    return 'Inf'  if $v == Perl6::Value::Num::Inf;
    return '-Inf' if $v == -&Perl6::Value::Num::Inf;
    return 'NaN'  if $v =~ m/n/i;
    return "" . $v 
}
sub to_bit        { $_[0] == 0 ? '' : 1 }
sub to_num        { 0 + $_[0] }
sub to_int        { $_[0] }

package Perl6::Value::Str;

sub to_str        { $_[0] }
sub to_bit        { 
    return '' if $_[0] eq '0' || $_[0] eq '';
    return 1;
}
sub to_num        {
    my $v = $_[0];
    $v =~ s/\s+//g;
    # Perl 6
    return Perl6::Value::Num::Inf   if $v eq 'Inf';
    return -&Perl6::Value::Num::Inf if $v eq '-Inf';
    return Perl6::Value::Num::NaN   if $v eq 'NaN';
    # native
    return Perl6::Value::Num::Inf   if $v eq "".&Perl6::Value::Num::Inf;
    return -&Perl6::Value::Num::Inf if $v eq "".(-&Perl6::Value::Num::Inf);
    return Perl6::Value::Num::NaN   if $v eq "".&Perl6::Value::Num::NaN;

    no warnings 'numeric';
    return 0 + $v;
}
sub to_int        { Perl6::Value::Num::to_int( to_num( $_[0] ) ) }

package Perl6::Value::Bit;

# built-in, unboxed 'bit' enums
sub bool::false      { '' }
sub bool::true       { 1 }
sub taint::untainted { '' }
sub taint::tainted   { 1 }

sub to_str        { $_[0] ? 1 : '' }
sub to_bit        { $_[0] }
sub to_num        { $_[0] == 0 ? 0 : 1 }
sub to_int        { to_num( $_[0] ) }

1;
__END__

=head1 NAME

Perl6::Value - Perl6 boxed and unboxed Values

=head1 SYNOPSIS

  use Perl6::Value;
  
  # unboxed Perl5 value
  my $num = Perl6::Value::Str::to_num( 'NaN' );
 
  # Perl6 "Num" object
  my $num = Num->NaN;

=head1 DESCRIPTION

This module implements the "Value" classes - Num, Int, Str, Bit, Pair.

It also implements platform-specific, low-level functions for "unboxed" Values.

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
