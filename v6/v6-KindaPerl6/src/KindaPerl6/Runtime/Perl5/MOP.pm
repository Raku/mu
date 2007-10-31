
=head1 NAME

MOP = Meta Object Protocol

=head1 SYNOPSIS

Meta Object Protocol provides the basic infrastructure for objects and
variables.

=head1 DESCRIPTION

There are several key functions used though this package.

=head2 Internal macros

=over

=item &::DISPATCH

=item &::MODIFIED

=item &::CAPTURIZE (moved to Runtime::Perl5::GLOBAL.pm)

=item %::PROTO

=back

=head2 Entities used elsewhere.

These objects are primarly a hash that is accessed via &::DISPATCH, the method
dispatcher is $object->{'_dispatch'}.

=over

=item $::Class

=item $::Method

=item $::Role

=item $::Object

=item $::List

=item $::Subset

=item $::Array

=item $::ArrayCell

=item $::ArrayContainer

=item $::Hash

=item $::Cell

=item $::HashCell

=item $::Scalar

=item $::Bit

=item $::Code

=item $::Int

=item $::Num

=item $::Str

=item $::Value

=item $::Undef

=item $::Container

=item $::Multi

=item $::Routine

=item $::Signature

=item $::Signature::Item

=back

=head2 Notes

=over

=item .WHAT & .HOW

http://feather.perl6.nl/syn/S12.html#Introspection

=back

=cut

use v5;
use strict 'vars';

# Usage:
# my $meth = ::DISPATCH( $::Method, 'new', sub { 'hi' } );
# my $obj =  ::DISPATCH( $::Object, 'new', $candidate );

package KindaPerl6::Runtime::Perl5::MOP;
use Data::Dumper;
use Carp qw(confess);
use UNIVERSAL;

=begin notes

# for some possible later use

    {
        package P6opaque;
        # This is the most minimal object system, which could be used by the high-level implementation

        my $methods  = {};
        my $dispatch = sub {
            # $self, $method
        };
        $methods->{new} = sub { my $class = shift;  my $v = sugar { _dispatch => $dispatch, $_[0]{_value}, @_ } };
    }

=end notes

=head2 ::DISPATCH

This is scoped into the main:: package.

Sample call:

 ::DISPATCH( $object, @args )

If $object->{ _dispatch } does not exist this object dies with a notice that
$object is not valid.

 $object->{ _dispatch } is a method dispatcher.

 $object = {
     _dispatch => sub { ... };
 }

returns the results of $object->{ _dispatch }( $object, @args )

 Example calls

 ::DISPATCH($::Undef,'new',0);

 Translates to

 return a new Undef class with value 0  or $::Undef->new(0);

 ::DISPATCH( $::Bit, 'new', exists ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} } ? 1 : 0 )

 Translates to

 return a new Bit class with the result of the expression "exists ${ ... } ? 1 : 0 "

=cut

# sugar routines

sub ::DISPATCH {
    my $invocant = shift;

    unless ( $invocant->{_dispatch} ) {
        confess "DISPATCH: calling @_ on invalid object:", Dumper($invocant), "\n";
    }
    $invocant->{_dispatch}( $invocant, @_ );
}

=head2 ::DISPATCH_VAR

 similar to _dispatch, except calls $invocant->_dispatch_VAR()

=cut

sub ::DISPATCH_VAR {
    my $invocant = shift;

    confess "DISPATCH_VAR:calling @_ on invalid object:", Dumper($invocant), "\n"
        unless $invocant->{_dispatch_VAR};
    $invocant->{_dispatch_VAR}( $invocant, @_ );
}

=head2 ::MODIFIED

 marks the variable as modified, such that we can track side-effects in BEGIN blocks

=cut

sub ::MODIFIED {
    $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
}

=head2 make_class

 make_class( methods => { method1 => ???, method2 => ???, ...  },
             attributes => [ attribute1 => ???, attribute2 => ???, ...  ],
             parents => [ parent => ???, parent2 => ???, ... ],
             proto => $proto
 );

 $proto is created via DispatchSugar, as $proto->{ _dispatch } is required

 See Also: %::PROTO (in this file)

=cut

sub make_class {
    my %args  = @_;
    my $proto = delete $args{proto};
    my $meta  = ( defined($proto) && ::DISPATCH( $proto, 'HOW' ) )
        || ::DISPATCH( $::Class, 'new', $args{name} );

    my %methods = %{ $args{methods} };

    while ( my ( $method_name, $sub ) = each %methods ) {
        ::DISPATCH( $meta, "redefine_method", $method_name, ::DISPATCH( $::Method, 'new', { code => $sub } ) );
    }

    for my $attribute_name ( @{ $args{attributes} } ) {
        ::DISPATCH( $meta, "add_attribute", $attribute_name );
    }

    for my $parent ( @{ $args{parents} } ) {
        ::DISPATCH( $meta, "add_parent", $parent );
    }

    return ::DISPATCH( $meta, "PROTOTYPE" );
}

# MOP implementation

# see below for defintion: search for "$meta_Object = "
my $meta_Object;

=head2 get_method_from_metaclass

$object->get_method_from_metaclass( $method_name );

returns the method for $method_name will go though the parent(s) B<{ _isa }> to
find $method_name

returns undef on failure

TODO: Should this "return;" intead of returning undef explicitly?

=cut

sub get_method_from_metaclass {
    my ( $self, $method_name ) = ( shift, shift );

    #print "looking in $self\n", Dumper($self);
    return $self->{_value}{methods}{$method_name}
        if exists $self->{_value}{methods}{$method_name};
    for my $parent ( @{ $self->{_value}{isa} } ) {

        #print "trying $parent ",$parent->{_isa}[0]{_value}{class_name},"\n", Dumper($parent);
        #print "available $method_name ? @{[ keys %{$parent->{_value}{methods}} ]}\n";
        my $m = get_method_from_metaclass( $parent, $method_name );
        return $m
            if $m;
    }
    return undef;
}

=head2 get_method_from_object

$object->get_method_from_object( $method_name )

returns the method for $method_name will go though the parent(s) AND
B<$meta_Object> to find $method_name

returns undef on failure

TODO: Should this "return;" intead of returning undef explicitly?

=cut

sub get_method_from_object {
    my ( $self, $method_name ) = ( shift, shift );

    # lookup local methods
    return $self->{_methods}{$method_name}
        if exists $self->{_methods}{$method_name};

    # lookup method in the metaclass
    for my $parent ( @{ $self->{_isa} }, $meta_Object ) {
        my $m = get_method_from_metaclass( $parent, $method_name );

        #print "found\n" if $m;
        return $m
            if $m;
    }
    return undef;
}

=head2 $dispatch

This closure is put into %::PROTO = ( _dispatch => $dispatch ).  $dispatch
is also used in various places to well, dispatch calls.

=cut

my $dispatch = sub {

    # $method_name is unboxed
    my ( $self, $method_name ) = ( shift, shift );

    #print "lookup $method_name in $self\n";

    unless ( ref($self) eq 'HASH'
        or ref($self) eq DISPATCH )
    {
        warn "internal error: wrong object format";
        print Dumper($self);
        return ::DISPATCH( $::Str, 'new', 'Error' );
    }

    if ( $self->{_roles}{auto_deref} ) {

        # this object requires FETCH
        my $value = ::DISPATCH_VAR( $self, 'FETCH' );
        return ::DISPATCH( $value, $method_name, @_ );
    }

    if (   !defined $self->{_value}
        && $method_name eq 'Str'
        && !exists( $self->{_methods}{'Str'} ) )
    {

        # 'self' is a prototype object
        # it stringifies to the class name
        # (unless the .Str method was replaced)
        #print "Class.Str: ",$self->{_isa}[0]{_value}{class_name},"\n";
        return ::DISPATCH( $::Str, 'new', $self->{_isa}[0]{_value}{class_name} );
    }

    my $meth = get_method_from_object( $self, $method_name );
    confess "no method '$method_name' in Class '", $self->{_isa}[0]{_value}{class_name}, "'\n"
        unless $meth;

    if ( ref( $meth->{_value} ) eq 'HASH' && exists $meth->{_value}{code} ) {

        # a properly boxed Method
        return ::DISPATCH( $meth, 'APPLY', $self, @_ );

        #return $meth->{_value}{code}->( $self, @_ );
    }

    # low-level Method - APPLY can't dispatch itself!
    # warn 'LOW-LEVEL APPLY '.$method_name."\n".join("\n", map { join ",", caller($_) } 1..6)."\n";
    #local $::ROUTINE = $meth;  # XXX
    return $meth->{_value}->( $self, @_ );
};

=head2 $::dispatch_VAR

this closure is used in various places for making "variables" work like plain
objects.

That is, if a method is dispatched through $::dispatch_VAR, it gets executed
on the $variable, not on the $variable contents.

used in KindaPerl6/Runtime/Perl5/
 Hash.pm
 List.pm
 Array.pm
 GLOBAL.pm
 MOP.pm

The &GLOBAL::VAR function in GLOBAL.pm provides a Perl 6 API for dispatch_VAR

=cut

$::dispatch_VAR = sub {

    # VAR() is just like CALL(), but it doesn't call FETCH
    # $method_name is unboxed
    my ( $self, $method_name ) = ( shift, shift );
    my $meth = get_method_from_object( $self, $method_name );

    die "no method '$method_name' in Class '", $self->{_isa}[0]{_value}{class_name}, "'\n"
        unless $meth;
    die "malformed Method object"
        if ( ref( $meth->{_value} ) ne 'HASH' || !exists $meth->{_value}{code} );

    return $meth->{_value}{code}->( $self, @_ );
};

=head2 %::PROTO

Basic (private) methods for all objects.

 %::PROTO = {
     '_methods'    => undef, # hash
     '_roles'      => undef  # hash
     '_value'      => undef, # whatever | %attributes
     '_isa'        => undef, # array
     '_dispatch'   => $dispatch, # see MOP.pm $dispatch = sub { ...
 };

=cut

%::PROTO = (
    _methods  => undef,        # hash
    _roles    => undef,        # hash
                               # _modified => undef,
    _value    => undef,        # whatever | %attributes
    _isa      => undef,        # array
    _dispatch => $dispatch,    # obtained from the $dispatch = sub {} above.
);

#--- Method

# XXX 'Method' is actually a container, it should probably inherit from Routine,
#     and it would be better if the internals matched Routine's

=head2 $method_new

a Method instance, implements B<.new>.

=cut

my $method_new = {
    %::PROTO,    # provides _methods, _roles, _value, _isa, _dispatch.
    _value => {
        code => sub {
            my $v = bless { %{ $_[0] }, _value => $_[1] }, DISPATCH;
            for my $arg_count ( 1 .. $#_ ) {
                my $arg = $_[$arg_count];
                if (   $::NamedArgument
                    && ref $arg
                    && UNIVERSAL::isa( $arg, 'HASH' )
                    && $arg->{_isa}
                    && @{ $arg->{_isa} }
                    && grep { $_ == ::DISPATCH( $::NamedArgument, 'HOW' ) } @{ $arg->{_isa} } )
                {
                    my $key = GLOBAL::_str( ::DISPATCH( $arg, '_argument_name_' ) );
                    my $value = ::DISPATCH( $arg, 'value' );
                    $v->{_value} = {} unless ref $v->{_value} eq 'HASH';
                    $v->{_value}{$key} = $value;
                }
            }
            $v;
            }
    },
    };

=head2 $method_APPLY

a Method instance, implements B<.APPLY>.

=cut

my $method_APPLY =  {
    %::PROTO,    # provides _methods, _roles, _value, _isa, _dispatch.
    _value =>    # { code =>
        sub {
        my $meth = shift;
        $meth->{_value}{code}->(@_);
        },

    # },
    };

=head2 $meta_Method

$meta_Method contains the "Class" object for Method.
$::Method contains the "Prototype object" for Method
which means it is a Method object with value "undef"

# http://irclog.perlgeek.de/perl6/2007-10-30#i_134420

=cut

my $meta_Method =  {
    %::PROTO,    # provides _methods, _roles, _value, _isa, _dispatch.
    _value => {
        methods => {
            new   => $method_new,
            APPLY => $method_APPLY,
        },
        class_name => 'Method',
    },
    };

=head2 $::Method

$::Method is a KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch object
consisting of multiple nested KindaPerl6::..::Dispatch objects.

 $::Method = {
         '_methods' => {},
         '_value'    => undef,
         '_roles'    => ${$::Method}->{'_isa'}->[0]->{'_isa'}->[0]->{'_value'}->{'methods'}->{'add_role'}->{'_roles'},
         '_dispatch' => ${$::Method}->{'_isa'}->[0]->{'_isa'}->[0]->{'_value'}->{'methods'}->{'add_role'}->{'_dispatch'},
         '_isa'     => [ $meta_Method ]
     }
 );

=head3 Parents:

=over

=item $::meta_Routine

Located near end of file

=back

=head3 Attributes:

none

=head3 Methods:

=over

=item $::Method->signature

Located near end of file

=back

=cut

$::Method = {
    %::PROTO,    # provides _methods, _roles, _value, _isa, _dispatch.
    _isa => [$meta_Method],
    };

# add .new & .APPLY methods into the parent list. (Defaults?)
# (14:54:54) fglock: at lines 513, 514 - the two Methods in the Method Class are
# told that they belong themselves to the Method class
# http://irclog.perlgeek.de/perl6/2007-10-30#i_134431

push @{ $method_new->{_isa} },   $meta_Method;
push @{ $method_APPLY->{_isa} }, $meta_Method;

$meta_Method->{_value}{methods}{WHAT} = ::DISPATCH( $::Method, 'new', sub {$::Method} );
$meta_Method->{_value}{methods}{HOW}  = ::DISPATCH( $::Method, 'new', sub {$meta_Method} );

#--- Object

# $meta_Object was declared above.
# my $meta_Object;

=head2 $meta_Object

a Class instance, implements Object

=cut

$meta_Object = {
    %::PROTO,    # provides _methods, _roles, _value, _isa, _dispatch.

    # _name     => $_[3],
    _value => { class_name => 'Object', },
    };

$meta_Object->{_value}{methods}{WHAT} = ::DISPATCH( $::Method, 'new', sub {$::Object} );
$meta_Object->{_value}{methods}{HOW}  = ::DISPATCH( $::Method, 'new', sub {$meta_Object} );
$meta_Object->{_value}{methods}{new}  = $method_new;

=head2 $::Object

$::Object is a KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch object

=head3 Parents

none

=head3 Attributes

none

=head3 Methods

=over

=item isa

=item does

=item Str

=item Int

=item true

=item defined

=item FETCH

no op

=item STORE

will die, this is read only.

=back

=cut

$::Object = {
    %::PROTO,

    # _name     => '$::Object',
    #_isa      => [ $meta_Object ],
    };

#--- Class

=head2 $meta_Class

_isa, should be thought of like package @ISA, ie, it looks back though the
_isa(s), to find a method that actually has the method name needed, before
calling.  I believe that the method name is called on the current context
the original $object->{ _dispatch }

See bottom of document for an example $meta_Class

=cut

my $meta_Class = {
    %::PROTO,    # provides _methods, _roles, _value, _isa, _dispatch.
    _value => {
        methods    => {},
        roles      => {},
        class_name => 'Class',
    },
    };

# push $meta_Class into it's own parent listing
push @{ $meta_Class->{_isa} }, $meta_Class;

# adds the 'add_method' method
$meta_Class->{_value}{methods}{add_method} = ::DISPATCH(
    $::Method,
    'new',
    {   code => sub {
            my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
            warn "redefining method $_[0]{_value}{class_name}.$meth_name"
                if exists $_[0]{_value}{methods}{$meth_name};
            $_[0]{_value}{methods}{$meth_name} = $_[2];
            }
    }
);

::DISPATCH(
    $meta_Class,
    'add_method',
    'redefine_method',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {
                my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
                $_[0]{_value}{methods}{$meth_name} = $_[2];
                }
        }
    )
);

::DISPATCH(
    $meta_Class,
    'add_method',
    'add_role',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {
                my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
                warn "redefining role $_[0]{_value}{class_name}.$meth_name"
                    if exists $_[0]{_value}{roles}{$meth_name};
                $_[0]{_value}{roles}{$meth_name} = $_[2];
                }
        }
    )
);

# TODO - "get attributes" ???
::DISPATCH(
    $meta_Class,
    'add_method',
    'add_attribute',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {
                my $meth_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
                $_[0]{_value}{attributes}{$meth_name} = sub {1};    # TODO ???
                                                                    #$_[0]{_value}{methods}{$meth_name} = sub : lvalue { $_[0]{_value}{$meth_name} };
                ::DISPATCH(
                    $_[0],
                    'add_method',
                    $meth_name,
                    ::DISPATCH(
                        $::Method,
                        'new',
                        {   code => sub  : lvalue {

                                # : lvalue is not needed, because we use .STORE() instead

                                #print "# accessing attribute $meth_name\n";

                                # XXX this should come from the Pad, at compile-time !!!
                                our $_MODIFIED;

                                # XXX - when is the right time to initialize attributes?

                                $_[0]{_value}{$meth_name} = ::DISPATCH(
                                    $::Scalar,
                                    'new',
                                    {   modified => $_MODIFIED,
                                        name     => '...',        # XXX name??? - get name from 'self'
                                    }
                                ) unless defined $_[0]{_value}{$meth_name};

                                # do we have more parameters? we should store it as the value.
                                if ( $_[1] ) {
                                    ::DISPATCH_VAR( $_[0]{_value}{$meth_name}, 'STORE', $_[1] );
                                }

                                $_[0]{_value}{$meth_name};
                                }
                        }
                    )
                );
                }
        }
    )
);

# WHAT & HOW ( See: http://feather.perl6.nl/syn/S12.html#Introspection )
::DISPATCH(
    $meta_Class,
    'add_method',
    'WHAT',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {$::Class}
        }
    )
);

::DISPATCH(
    $meta_Class,
    'add_method',
    'HOW',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {$meta_Class}
        }
    )
);

::DISPATCH(
    $meta_Class,
    'add_method',
    'add_parent',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub { push @{ $_[0]{_value}{isa} }, $_[1] }
        }
    )
);

::DISPATCH(
    $meta_Class,
    'add_method',
    'methods',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {

                # TODO - show inherited methods
                # ??? - should this return the Methods and they stringify to method name ???
                ::DISPATCH(
                    $::Array, 'new',
                    {   _array => [
                            map { ::DISPATCH( $::Str, 'new', $_ ) }
                                keys %{ $_[0]{_value}{methods} }
                        ]
                    }
                );
                }
        }
    )
);

::DISPATCH(
    $meta_Class,
    'add_method',
    'attributes',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {

                # TODO - show inherited methods
                # ??? - should this return the Methods and they stringify to method name ???
                ::DISPATCH(
                    $::Array, 'new',
                    {   _array => [
                            map { ::DISPATCH( $::Str, 'new', $_ ) }
                                keys %{ $_[0]{_value}{attributes} }
                        ]
                    }
                );
                }
        }
    )
);

::DISPATCH(
    $meta_Class,
    'add_method',
    'new',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {

                # new Class( $class_name )
                my $meta_class = $_[0];
                my $class_name = ref( $_[1] ) ? $_[1]{_value} : $_[1];
                my $self_meta  = {
                    %::PROTO,    # provides _methods, _roles, _value, _isa, _dispatch.
                    _isa   => [$meta_Class],
                    _value => {
                        class_name => $class_name,    # XXX should be ::Str
                    },
                    };
                my $proto = {
                    %::PROTO,                         # provides _methods, _roles, _value, _isa, _dispatch.
                    _isa => [$self_meta],
                    };
                $self_meta->{_value}{methods}{WHAT} = ::DISPATCH( $::Method, 'new', sub {$proto} );
                $self_meta->{_value}{methods}{HOW}  = ::DISPATCH( $::Method, 'new', sub {$self_meta} );
                $self_meta->{_methods}{PROTOTYPE}   = ::DISPATCH( $::Method, 'new', sub {$proto} );
                $self_meta;
                }
        }
    )
);

=head2 $::Class

$::Class is a KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch object
consisting of multiple nested KindaPerl6::..::Dispatch objects.

A $::Class object consists of $::Methods ($meta_Method?) objects

Note: The method dispatcher in Class knows how to handle inheritance and roles.

=head3 Parents

none

=head3 Attributes

none

=head3 Methods

=over

=item TODO: need to find the methods

=back

=cut

$::Class = {
    %::PROTO,    # provides _methods, _roles, _value, _isa, _dispatch.
    _isa => [$meta_Class],
    };

# add $meta_Class into $meta_Method & $meta_Object parent tree
push @{ $meta_Method->{_isa} }, $meta_Class;
push @{ $meta_Object->{_isa} }, $meta_Class;

#push @{$meta_Class->{_isa}}, $meta_Object;

#--- Roles

=head2 $::Role

$::Role is a $::Class object

=head3 Parents

none

=head3 Attributes

none

=head3 Methods

All methods are the same as the ::Class methods

=cut

my $meta_Role = ::DISPATCH( $::Class, 'new', "Role" );

$::Role = ::DISPATCH( $meta_Role, 'PROTOTYPE' );

# copy Class methods
$meta_Role->{_value}{methods} = { %{ $meta_Class->{_value}{methods} } };

#--- Values

# $meta_Value (sugar created via sugar, though $::Class)
my $meta_Value = ::DISPATCH( $::Class, 'new', "Value" );

=head2 $::Value

$::Value is a $::Class object

=head3 Parents

none

=head3 Attributes

none

=head3 Methods

=over

=item WHICH

=item p5landish

=item print

=item FETCH

=back

=cut

$::Value = ::DISPATCH( $meta_Value, 'PROTOTYPE' );

::DISPATCH(
    $meta_Value,
    'add_method',
    'WHICH',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub { ::DISPATCH( $::Str, 'new', "$_[0]{_value}" ) }
        }
    )
);

::DISPATCH(
    $meta_Value,
    'add_method',
    'p5landish',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub { $_[0]{_value} }
        }
    )
);

::DISPATCH(
    $meta_Value,
    'add_method',
    'print',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub { print $_[0]{_value} }
        }
    )
);

# -- FETCH is implemented in Object
::DISPATCH(
    $meta_Value,
    'add_method',
    'FETCH',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            $_[0];
        }
    )
);

my $meta_Str = ::DISPATCH( $::Class, 'new', "Str" );

=head2 $::Str

$::Str is a $::Class object

=head3 Parents

none

=head3 Attributes

none

=head3 Methods

=over

=item perl

=item Str

=item true

=item chars

=back

=cut

$::Str = ::DISPATCH( $meta_Str, 'PROTOTYPE' );

::DISPATCH( $meta_Str, 'add_parent', $meta_Value );

::DISPATCH(
    $meta_Str,
    'add_method',
    'perl',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub { my $v = ::DISPATCH( $::Str, 'new', '\'' . $_[0]{_value} . '\'' ) }
        }
    )
);

::DISPATCH(
    $meta_Str,
    'add_method',
    'Str',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub { $_[0] }
        }
    )
);

::DISPATCH(
    $meta_Str,
    'add_method',
    'true',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {
                ::DISPATCH( $::Bit, 'new', ( $_[0]{_value} ne '' && $_[0]{_value} ne '0' ) ? 1 : 0 );
                }
        }
    )
);

::DISPATCH(
    $meta_Str,
    'add_method',
    'chars',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {
                ::DISPATCH( $::Int, 'new', length( $_[0]{_value} ) );
                }
        }
    )
);

my $meta_Int = ::DISPATCH( $::Class, 'new', "Int" );

=head2 $::Int

$::Int is a $::Class object

=head3 Parents

none

=head3 Attributes

none

=head3 Methods

=over

=item perl

=item increment

=item Str

=item true

=back

=cut

$::Int = ::DISPATCH( $meta_Int, 'PROTOTYPE' );

::DISPATCH( $meta_Int, 'add_parent', $meta_Value );

::DISPATCH( $meta_Int, 'add_method', 'perl', ::DISPATCH( $::Method, 'new', sub { my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} ) } ) );

# XXX - Bind to attributes fail, so to move on, a increment is interesting
::DISPATCH( $meta_Int, 'add_method', 'increment', ::DISPATCH( $::Method, 'new', sub { $_[0]{_value}++; $_[0] } ) );

::DISPATCH( $meta_Int, 'add_method', 'Str', ::DISPATCH( $::Method, 'new', sub { my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} ) } ) );

::DISPATCH( $meta_Int, 'add_method', 'true', ::DISPATCH( $::Method, 'new', sub { my $v = ::DISPATCH( $::Bit, 'new', ( $_[0]{_value} == 0 ? 0 : 1 ) ) } ) );

my $meta_Num = ::DISPATCH( $::Class, 'new', "Num" );

=head2 $::Num

$::Num is a $::Class object

=head3 Parents

none

=head3 Attributes

none

=head3 Methods

=over

=item perl

=item Str

=item true

=back

=cut

$::Num = ::DISPATCH( $meta_Num, 'PROTOTYPE' );

::DISPATCH( $meta_Num, 'add_parent', 'meta_Value' );

::DISPATCH( $meta_Num, 'add_method', 'perl', ::DISPATCH( $::Method, 'new', sub { my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} ) } ) );

::DISPATCH( $meta_Num, 'add_method', 'Str', ::DISPATCH( $::Method, 'new', sub { my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} ) } ) );

::DISPATCH( $meta_Num, 'add_method', 'true', ::DISPATCH( $::Method, 'new', sub { my $v = ::DISPATCH( $::Bit, 'new', $_[0]{_value} == 0 ? 0 : 1 ) } ) );

my $meta_Bit = ::DISPATCH( $::Class, 'new', "Bit" );

=head2 $::Bit

$::Bit is a $::Class object

=head3 Parents

$meta_Value

=head3 Attributes

none

=head3 Methods

=over

=item Str

=item true

=item perl

Note, this returns a string 'True' or 'False'

=back

=cut

$::Bit = ::DISPATCH( $meta_Bit, 'PROTOTYPE' );

::DISPATCH( $meta_Bit, 'add_parent', $meta_Value );

::DISPATCH( $meta_Bit, 'add_method', 'perl', ::DISPATCH( $::Method, 'new', sub { my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} ? 'True' : 'False' ) } ) );

::DISPATCH( $meta_Bit, 'add_method', 'Str', ::DISPATCH( $::Method, 'new', sub { my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} ) } ) );

::DISPATCH( $meta_Bit, 'add_method', 'true', ::DISPATCH( $::Method, 'new', sub { my $v = ::DISPATCH( $::Bit, 'new', $_[0]{_value} ) } ) );

#--- finish Object

sub meta_isa {
    my $meta = shift;
    my $obj  = shift;
    return 1
        if $meta->{_value}{class_name} eq $obj->{_value};
    for my $parent ( @{ $meta->{_value}{isa} } ) {
        return 1
            if meta_isa( $parent, $obj );
    }
    return 0;
}

::DISPATCH(
    $meta_Object,
    'add_method',
    'isa',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            my $self = shift;
            my $obj  = shift;    # Proto, Subset, Str  ???
            $obj = ::DISPATCH( $obj, 'Str' );
            my $meta = ::DISPATCH( $self, 'HOW' );
            return ::DISPATCH(
                $::Bit, 'new',
                (   meta_isa( $meta, $obj )
                        || $obj->{_value} eq 'Object'    # XXX
                    ? 1
                    : 0
                )
            );
        }
    )
);

::DISPATCH(
    $meta_Object,
    'add_method',
    'does',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            my $self = shift;
            my $obj  = shift;

            if ( ::DISPATCH( $obj, 'isa', ::DISPATCH( $::Str, 'new', 'Str' ) )->{_value} ) {

                # Str
                # the call to .Str is needed in order to stringify the ::Str prototype
                $obj = eval '$::' . ::DISPATCH( $obj, "Str" )->{_value};
                return ::DISPATCH( $::Bit, 'new', 0 )
                    unless defined $obj;
            }

            if ( ::DISPATCH( $obj, 'isa', ::DISPATCH( $::Str, 'new', 'Subset' ) )->{_value} ) {

                # Subset
                my $base_class = $obj->{_value}{base_class};
                my $block      = $obj->{_value}{block};
                my $does       = ::DISPATCH( $self, 'does', $base_class );

                #print "does == ", $does->{_value},"\n";
                return $does
                    unless $does->{_value};

                # XXX TODO - Subset.block should be a ::Code
                return ::DISPATCH( $::Bit, 'new', ( $block->($self)->{_value} ? 1 : 0 ) );

            }

            #print "Testing not-Subset\n";
            $obj = ::DISPATCH( $obj, 'Str' );    # Proto or Str  XXX
            return ::DISPATCH( $::Bit, 'new', 1 )
                if exists( $self->{_roles}{ $obj->{_value} } );
            return ::DISPATCH( $self, 'isa', $obj );
        }
    )
);

# add .Str to $meta_Object
::DISPATCH(
    $meta_Object,
    'add_method',
    'Str',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            my $v = ::DISPATCH( $::Str, 'new', '::' . $_[0]{_isa}[0]{_value}{class_name} . '(...)' );
        }
    )
);

# add .Int
::DISPATCH(
    $meta_Object,
    'add_method',
    'Int',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            my $v = ::DISPATCH( $::Int, 'new', 0 + $_[0]{_value} );    # ???
        }
    )
);

# add .true (Bit)
::DISPATCH(
    $meta_Object,
    'add_method',
    'true',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            ::DISPATCH( $::Bit, 'new', 1 );    # ???
        }
    )
);

# add .defined (Bit)
::DISPATCH(
    $meta_Object,
    'add_method',
    'defined',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            my $v = ::DISPATCH( $::Bit, 'new', ( defined $_[0]{_value} ? 1 : 0 ) );
        }
    )
);

# Object.FETCH is a no-op
::DISPATCH( $meta_Object, 'add_method', 'FETCH', ::DISPATCH( $::Method, 'new', sub { $_[0] } ) );

# Object.STORE is forbidden
my $method_readonly = ::DISPATCH(
    $::Method,
    'new',
    {   code => sub {
            die "attempt to modify a read-only value";
            }
    }
);
::DISPATCH( $meta_Object, 'add_method', 'STORE', $method_readonly );

#--- back to Value

=head2 $::Undef

=head3 Parents

none

=head3 Attributes

none

=head3 Methods

=over

=item perl

returns $::Str 'undef' a string

=item Str

returns $::Str '' as a string

=item true

returns $::Bit 0

=item defined

returns $::Bit 0

=back

=cut

$::Undef = make_class(
    proto   => $::Undef,
    name    => "Undef",
    parents => [$meta_Value],
    methods => {
        perl    => sub { ::DISPATCH( $::Str, 'new', 'undef' ) },
        Str     => sub { ::DISPATCH( $::Str, 'new', '' ) },
        true    => sub { ::DISPATCH( $::Bit, 'new', 0 ) },
        defined => sub { ::DISPATCH( $::Bit, 'new', 0 ) },
    }
);

# $meta_code
my $meta_Code = ::DISPATCH( $::Class, 'new', "Code" );

=head2 $::Code

$::Code is a $::Class object

=head3 Parents

none

=head3 Attributes

=over

=item code

=item signature

=item ast

=back

=head3 Methods

=over

=item perl

=item APPLY

warning: apply attempts to use $::Junction (not implemeneted (yet))

=back

=cut

$::Code = ::DISPATCH( $meta_Code, 'PROTOTYPE' );

::DISPATCH( $meta_Code, 'add_parent', $meta_Value );

::DISPATCH( $meta_Code, 'add_attribute', 'code' );

::DISPATCH( $meta_Code, 'add_attribute', 'signature' );

::DISPATCH( $meta_Code, 'add_attribute', 'ast' );

::DISPATCH(
    $meta_Code,
    'add_method',
    'perl',
    ::DISPATCH(
        $::Method,
        'new',
        sub {

            # TODO - emit from $.ast
            my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value}{src} );
            return ::DISPATCH( $::Str, 'new', '{ ... }' );
        }
    )
);

my $_apply;

::DISPATCH(
    $meta_Code,
    'add_method',
    'APPLY',
    ::DISPATCH(
        $::Method,
        'new',
        (   $_apply = sub {

                # XXX - use the attributes

                my $self  = shift;    # the Code object
                my @param = @_;

                #print "param: \n"; #,Dumper(\@param);
                # is there a Junction class?
                if ($::Junction) {

                    #print "we've got junctions\n";
                    for my $index ( 0 .. $#param ) {
                        my $j = $param[$index];
                        next unless ref $j;
                        if ( ::DISPATCH( $j, 'does', $::Junction )->{_value} ) {
                            my @things = @{ ::DISPATCH( ::DISPATCH( $j, 'things' ), 'array' )->{_value}{_array} };
                            return ::DISPATCH(
                                $::Junction,
                                'new',
                                {   type   => $j->{_value}{type},
                                    things => ::DISPATCH(
                                        $::Array, 'new',
                                        {   _array => [
                                                map {
                                                    $param[$index] = $_;
                                                    $_apply->( $self, @param );
                                                    } @things
                                            ],
                                        }
                                    ),
                                }
                            );
                        }
                    }
                }

                local $::ROUTINE = $self;
                $self->{_value}{code}->(@_);
            }
        )
    )
);

::DISPATCH( $meta_Code, 'add_method', 'p5landish', ::DISPATCH( $::Method, 'new', sub { $_[0]{_value}{code} } ) );

my $meta_List = ::DISPATCH( $::Class, 'new', "List" );

=head2 $::List

$::List at this time just uses its parent $meta_Value ( $::Value? )

=cut

$::List = ::DISPATCH( $meta_List, 'PROTOTYPE' );

::DISPATCH( $meta_List, 'add_parent', $meta_Value );

# TODO - finish List implementation ...

#--- Subset
# TODO - hierarchical constraints - Array of Foo
#    - use a linked list of Subsets ???
# -> you can't subclass a subset
my $meta_Subset = ::DISPATCH( $::Class, 'new', "Subset" );

=head2 $::Subset

$::Subset is a $::Class object

=head3 Parents:

=over

=item $meta_Value

=back

=head3 Attributes:

=over

=item base_class

=item block

=back

=head3 Methods:

=over

=item perl

=back

=cut

$::Subset = ::DISPATCH( $meta_Subset, 'PROTOTYPE' );

::DISPATCH( $meta_Subset, 'add_parent', $meta_Value );

::DISPATCH( $meta_Subset, 'add_attribute', 'base_class' );    # Class

::DISPATCH( $meta_Subset, 'add_attribute', 'block' );         # Code

# -> if you instantiate a subset type you get an object of its base type
#    ??? how to implement this?
#    $subset->new() creates a Subset, for now
#$meta_Subset->add_method(
#    'new',
#    ::DISPATCH( $::Method, 'new',
#        sub {
#            my $self = shift;
#            my $base_type = $self->{_value}{base_type};
#            ::DISPATCH( $base_type, 'new', @_ );
#        }
#    )
#);

::DISPATCH( $meta_Subset, 'add_method', 'perl', ::DISPATCH( $::Method, 'new', sub { my $v = ::DISPATCH( $::Str, 'new', '::Subset( base_class => "...", block => "..." )' ) } ) );

#--- Containers

$::meta_Container = ::DISPATCH( $::Class, 'new', "Container" );

=head2 $::Container

$::Container is a $::Class object

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

=over

=item FETCH

=item BIND

=item STORE

dies, Read only

=back

=cut

$::Container = ::DISPATCH( $::meta_Container, 'PROTOTYPE' );

::DISPATCH(
    $::meta_Container,
    'add_method',
    'FETCH',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {
                $_[0]{_value}{cell} ? $_[0]{_value}{cell} : ::DISPATCH( $::Undef, "new", 0 );
                }
        }
    )
);

::DISPATCH(
    $::meta_Container,
    'add_method',
    'STORE',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {
                die "attempt to modify a read-only value"
                    if $_[0]{_roles}{readonly};
                $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
                $_[0]{_value}{cell} = $_[1];
                }
        }
    )
);

::DISPATCH(
    $::meta_Container,
    'add_method',
    'BIND',
    ::DISPATCH(
        $::Method,
        'new',
        {   code => sub {

                # XXX - see old 'Type.pm'
                $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
                $_[1]{_value}{modified}{ $_[1]{_value}{name} } = 1;
                if ( $_[1]{_roles}{container} ) {

                    # Container := Container
                    $_[0]{_value} = $_[1]{_value};
                    $_[0]{_roles}{readonly} = $_[1]{_roles}{readonly};
                }
                else {

                    # Container := Object
                    # - add the read-only trait
                    $_[0]{_value}{cell}     = $_[1];
                    $_[0]{_roles}{readonly} = 1;
                }
                $_[0];
                }
        }
    )
);

my $meta_Scalar = ::DISPATCH( $::Class, 'new', "Scalar" );

=head2 $::Scalar

$::Scalar is a $::Class object

=head3 Parent:

=over

=item $::meta_Container

=back

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=back

=cut

$::Scalar = ::DISPATCH( $meta_Scalar, 'PROTOTYPE' );

::DISPATCH( $meta_Scalar, 'add_parent', $::meta_Container );

::DISPATCH(
    $meta_Scalar,
    'add_method',
    'new',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1],                                     # { %{$_[1]}, cell => undef },
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
    )
);

my $meta_ArrayContainer = ::DISPATCH( $::Class, 'new', "ArrayContainer" );

=head2 $::ArrayContainer

$::ArrayContainer is a $::Class object

=head3 Parent:

=over

=item $::meta_Container

=back

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=back

=cut

$::ArrayContainer = ::DISPATCH( $meta_ArrayContainer, 'PROTOTYPE' );

::DISPATCH( $meta_ArrayContainer, 'add_parent', $::meta_Container );

::DISPATCH(
    $meta_ArrayContainer,
    'add_method',
    'new',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1],                                     # { %{$_[1]}, cell => undef },
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
            $v->{_value}{cell} = ::DISPATCH( $::Array, "new" )
                unless exists $v->{_value}{cell};
            $v;
        },
    )
);

my $meta_Routine = ::DISPATCH( $::Class, 'new', "Routine" );

=head2 $::Routine

$::Routine is a $::Class object

=head3 Parent:

=over

=item $::meta_Routine

=back

=head3 Attributes:

=over

=item $::ArrayContainer->

=back

=head3 Methods:

=over

=item new

=item APPLY

=item perl

=back

=cut

$::Routine = ::DISPATCH( $meta_Routine, 'PROTOTYPE' );

::DISPATCH( $meta_Routine, 'add_parent', $::meta_Container );

::DISPATCH( $meta_Routine, 'add_method', 'STORE', $method_readonly );

::DISPATCH(
    $meta_Routine,
    'add_method',
    'APPLY',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            my $self = shift;
            local $::ROUTINE = $self->{_value}{cell};
            $self->{_value}{cell}{_value}{code}->(@_);
        }
    )
);

::DISPATCH(
    $meta_Routine,
    'add_method',
    'new',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1],                                     # { cell => undef },
                _roles        => { 'container' => 1, 'auto_apply' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
                };
        },
    )
);

::DISPATCH(
    $meta_Routine,
    'add_method',
    'perl',
    ::DISPATCH(
        $::Method,
        'new',
        sub {
            ::DISPATCH( $::Str, 'new', $_[0]{_value}{cell}{_value}{src} );
        },
    )
);

# Method isa Routine
::DISPATCH( $meta_Method, 'add_parent', $meta_Routine );

::DISPATCH( $meta_Method, 'add_parent', $meta_Routine );

# XXX should not need this!
::DISPATCH(
    $meta_Method,
    'add_method',
    'signature',
    ::DISPATCH(
        $::Method,
        'new',
        sub {

            #print "SIG ", keys %{ $_[0]{_value} }, "\n";
            $_[0]{_value}{signature};
        }
    )
);

# tests if a variable was initialized at all
# we need this because defined() always return false with prototype objects
$GLOBAL::Code_VAR_defined = ::DISPATCH(
    $::Code, 'new',
    {   code => sub {

            #print "(MOP)DEFINED? \n";
            return ::DISPATCH( $::Bit, 'new', ( defined $_[0] ? 1 : 0 ) );
        },
        src => '&GLOBAL::VAR_defined'
    }
);

# class prototypes are needed because we have circular dependencies

=head2 $::Signature

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

none

=cut

$::Signature = make_class( proto => $::Signature, name => "Signature", methods => {} );

=head2 $::Signature::Item

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

none

=cut

$::Signature::Item = make_class( proto => $::Signature::Item, name => "Signature::Item", methods => {} );

=head2 $::Array

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

none

=cut

$::Array = make_class( proto => $::Array, name => "Array", methods => {} );

=head2 $::Hash

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

none

=cut

$::Hash = make_class( proto => $::Hash, name => "Hash", methods => {} );

=head2 $::Cell

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=item STORE

=item FETCH

=back

=cut

$::Cell = make_class(
    proto   => $::Cell,
    name    => "Cell",
    parent  => [$::meta_Container],
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1],
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
        STORE => sub {
            ${ $_[0]{_value}{cell} } = $_[1];
        },
        FETCH => sub {
            return ${ $_[0]{_value}{cell} } || ::DISPATCH( $::Undef, 'new', 0 );
        },
    }
);

=head2 $::HashCell

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=item STORE

=item FETCH

=item exists

=back

=cut

$::HashCell = make_class(
    proto   => $::HashCell,
    name    => "HashCell",
    parent  => [$::meta_Container],
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1],
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
        STORE => sub {
            ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} } = $_[1];
        },
        FETCH => sub {
            exists ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} }
                ? ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} }
                : ::DISPATCH( $::Undef, 'new', 0 );
        },
        exists => sub {
            ::DISPATCH( $::Bit, 'new', exists ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} } ? 1 : 0 );
        },
    }
);

=head2 $::ArrayCell

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

=over

=item new

=item STORE

=item FETCH

=item exists

=back

=cut

$::ArrayCell = make_class(
    proto   => $::ArrayCell,
    name    => "ArrayCell",
    parent  => [$::meta_Container],
    methods => {
        new => sub {
            my $v = {
                %{ $_[0] },
                _value        => $_[1],
                _roles        => { 'container' => 1, 'auto_deref' => 1 },
                _dispatch_VAR => $::dispatch_VAR,
            };
        },
        STORE => sub {
            ${ $_[0]{_value}{cell} }[ $_[0]{_value}{key} ] = $_[1];
        },
        FETCH => sub {
            exists ${ $_[0]{_value}{cell} }[ $_[0]{_value}{key} ]
                ? ${ $_[0]{_value}{cell} }[ $_[0]{_value}{key} ]
                : ::DISPATCH( $::Undef, 'new', 0 );
        },
        exists => sub {
            ::DISPATCH( $::Bit, 'new', exists ${ $_[0]{_value}{cell} }[ $_[0]{_value}{key} ] ? 1 : 0 );
        },
    }
);

=head2 $::Multi

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

=over

=item APPLY

=back

=cut

$::Multi = make_class(
    proto   => $::Multi,
    name    => "Multi",
    parent  => [$meta_Code],
    methods => {
        APPLY => sub {
            my $self = shift;
            my $code = ::DISPATCH( $self, 'select', ::CAPTURIZE( \@_ ) );
            ::DISPATCH( $code, 'APPLY', @_ );
        },
    }
);

1;

__END__

=head2 Example $meta_Class

See bottom of document for an example $meta_Class

 $meta_Class = {   '_methods' => {},
         '_isa'     => [ ${$meta_Class} ],
         '_value'   => {
             'class_name' => 'Class',
             'roles'      => {},
             'methods'    => {
                 'add_role' => bless(
                     {   '_methods' => {},
                         '_isa'     => [
                             bless(
                                 {   '_methods' => {},
                                     '_isa'     => [ ${$meta_Class} ],
                                     '_value'   => {
                                         'class_name' => 'Method',
                                         'isa'        => [
                                             bless(
                                                 {   '_methods' => {
                                                         'PROTOTYPE' => bless(
                                                             {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                 '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                 '_value'    => sub {"DUMMY"},
                                                                 '_dispatch' => sub {"DUMMY"},
                                                                 '_roles'    => {}
                                                             },
                                                             'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                         )
                                                     },
                                                     '_isa'   => [ ${$meta_Class} ],
                                                     '_value' => {
                                                         'class_name' => 'Routine',
                                                         'isa'        => [
                                                             bless(
                                                                 {   '_methods' => {
                                                                         'PROTOTYPE' => bless(
                                                                             {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                                 '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                                 '_value'    => sub {"DUMMY"},
                                                                                 '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                                 '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                             },
                                                                             'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                                         )
                                                                     },
                                                                     '_isa'   => [ ${$meta_Class} ],
                                                                     '_value' => {
                                                                         'class_name' => 'Container',
                                                                         'methods'    => {
                                                                             'BIND' => bless(
                                                                                 {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                                     '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                                     '_value'   => { 'code' => sub {"DUMMY"} },
                                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                                 },
                                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                                             ),
                                                                             'FETCH' => bless(
                                                                                 {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                                     '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                                     '_value'   => { 'code' => sub {"DUMMY"} },
                                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                                 },
                                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                                             ),
                                                                             'HOW' => bless(
                                                                                 {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                                     '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                                     '_value'    => sub {"DUMMY"},
                                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                                 },
                                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                                             ),
                                                                             'STORE' => bless(
                                                                                 {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                                     '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                                     '_value'   => { 'code' => sub {"DUMMY"} },
                                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                                 },
                                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                                             ),
                                                                             'WHAT' => bless(
                                                                                 {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                                     '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                                     '_value'    => sub {"DUMMY"},
                                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                                 },
                                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                                             )
                                                                         }
                                                                     },
                                                                     '_roles'    => {},
                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'}
                                                                 },
                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                             )
                                                         ],
                                                         'methods' => {
                                                             'perl' => bless(
                                                                 {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                     '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                     '_value'    => sub {"DUMMY"},
                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                 },
                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                             ),
                                                             'APPLY' => bless(
                                                                 {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                     '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                     '_value'    => sub {"DUMMY"},
                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                 },
                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                             ),
                                                             'new' => bless(
                                                                 {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                     '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                     '_value'    => sub {"DUMMY"},
                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                 },
                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                             ),
                                                             'HOW' => bless(
                                                                 {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                     '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                     '_value'    => sub {"DUMMY"},
                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                 },
                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                             ),
                                                             'STORE' => bless(
                                                                 {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                     '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                     '_value'   => { 'code' => sub {"DUMMY"} },
                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                 },
                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                             ),
                                                             'WHAT' => bless(
                                                                 {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                                     '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                                     '_value'    => sub {"DUMMY"},
                                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                                 },
                                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                                             )
                                                         }
                                                     },
                                                     '_roles'    => {},
                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'}
                                                 },
                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                             )
                                         ],
                                         'methods' => {
                                             'signature' => bless(
                                                 {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                     '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                     '_value'    => sub {"DUMMY"},
                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                 },
                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                             ),
                                             'APPLY' => bless(
                                                 {   '_methods'  => undef,
                                                     '_isa'      => [ ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0] ],
                                                     '_value'    => sub {"DUMMY"},
                                                     '_roles'    => undef,
                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'}
                                                 },
                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                             ),
                                             'HOW' => bless(
                                                 {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                     '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                     '_value'    => sub {"DUMMY"},
                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                 },
                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                             ),
                                             'new' => bless(
                                                 {   '_methods' => {},
                                                     '_isa'     => [ ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0] ],
                                                     '_value'   => { 'code' => sub {"DUMMY"} },
                                                     '_roles'    => {},
                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'}
                                                 },
                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                             ),
                                             'WHAT' => bless(
                                                 {   '_methods'  => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                                                     '_isa'      => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                                                     '_value'    => sub {"DUMMY"},
                                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                                                     '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                                                 },
                                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                                             )
                                         }
                                     },
                                     '_roles'    => {},
                                     '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'}
                                 },
                                 'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                             )
                         ],
                         '_value' => { 'code' => sub {"DUMMY"} },
                         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                         '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                     },
                     'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                 ),
                 'HOW' => bless(
                     {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                         '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                         '_value'   => { 'code' => sub {"DUMMY"} },
                         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                         '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                     },
                     'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                 ),
                 'add_parent' => bless(
                     {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                         '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                         '_value'   => { 'code' => sub {"DUMMY"} },
                         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                         '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                     },
                     'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                 ),
                 'redefine_method' => bless(
                     {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                         '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                         '_value'   => { 'code' => sub {"DUMMY"} },
                         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                         '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                     },
                     'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                 ),
                 'methods' => bless(
                     {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                         '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                         '_value'   => { 'code' => sub {"DUMMY"} },
                         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                         '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                     },
                     'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                 ),
                 'new' => bless(
                     {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                         '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                         '_value'   => { 'code' => sub {"DUMMY"} },
                         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                         '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                     },
                     'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                 ),
                 'add_method' => bless(
                     {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                         '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                         '_value'   => { 'code' => sub {"DUMMY"} },
                         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                         '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                     },
                     'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                 ),
                 'add_attribute' => bless(
                     {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                         '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                         '_value'   => { 'code' => sub {"DUMMY"} },
                         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                         '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                     },
                     'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                 ),
                 'attributes' => bless(
                     {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                         '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                         '_value'   => { 'code' => sub {"DUMMY"} },
                         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                         '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                     },
                     'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                 ),
                 'WHAT' => bless(
                     {   '_methods' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_methods'},
                         '_isa'     => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'},
                         '_value'   => { 'code' => sub {"DUMMY"} },
                         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'},
                         '_roles'    => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_roles'}
                     },
                     'KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch'
                 )
             }
         },
         '_roles'    => {},
         '_dispatch' => ${$meta_Class}->{'_value'}->{'methods'}->{'add_role'}->{'_isa'}->[0]->{'_value'}->{'isa'}->[0]->{'_methods'}->{'PROTOTYPE'}->{'_dispatch'}
     }
 );

=cut
