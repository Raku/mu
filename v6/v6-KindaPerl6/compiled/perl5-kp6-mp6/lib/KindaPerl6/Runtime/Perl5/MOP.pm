
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

=head2 Notes

=over

=item .WHAT & .HOW

http://feather.perl6.nl/syn/S12.html#Introspection

=back

=head2 NOTES:

In order to keep track of object "creation"
I have put in Flags to identify various change points

IE

##DLOCAUS ::Object - AddMethod

DLOCAUS is my IRC name, and is there so I can grep for it easily.

=cut

use v5;
use strict 'vars';

# Usage:
# my $meth = ::DISPATCH( $::Method, 'new', sub { 'hi' } );
# my $obj =  ::DISPATCH( $::Object, 'new', $candidate );

package KindaPerl6::Runtime::Perl5::MOP;
use Data::Dumper;
use Carp qw(confess);
use Scalar::Util qw | refaddr |;
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
        $methods->{new} = sub {
            my $class = shift;
            my $v = {
                _dispatch => $dispatch,
                $_[0]{_value}, @_
            }
        };
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

 ::DISPATCH($::Undef,'new');

 Translates to

 return a new Undef instance with value $::Undef->new();

 ::DISPATCH( $::Bit, 'new', exists ${ $_[0]{_value}{cell} }{ $_[0]{_value}{key} } ? 1 : 0 )

 Translates to

 return a new Bit class with the result of the expression "exists ${ ... } ? 1 : 0 "

=cut

# sugar routines

# for $ENV{ DEBUG }
my %_dispatch_signatures;
my $_dispatch_recursion = 0;

sub ::DISPATCH {
    my $invocant = shift;

    unless ( $invocant->{_dispatch} ) {
        confess "DISPATCH: calling @_ on invalid object:", Dumper($invocant), "\n";
    }

    if ( $ENV{ DEBUG } ) {
        # setenv DEBUG 1   (or 2 call/leaving statements)
        # detect circular invocations, if we invoke the same subroutine more
        # than $failure times we wil die out.  I'm assuming that if we see
        # the exact same call again, that we have started an infinate loop.
        my $failure = 1;
        my $signature = join '',
            "::DISPATCH( ", (join ', ', refaddr ($invocant), @_), ")", "\n";

        # add signature to list of seen signatures.
        $_dispatch_signatures{ $signature }++;

        confess "(depth = $_dispatch_recursion ) I've seen this call before! $signature\n"
            if ( $_dispatch_signatures{ $signature } > $failure );

        $_dispatch_recursion++;
        print '  ' x $_dispatch_recursion, "Calling: $signature" if $ENV{ DEBUG } == 2;
        my $return_value = $invocant->{_dispatch}( $invocant, @_ );
        print '  ' x $_dispatch_recursion, "Leaving: $signature" if $ENV{ DEBUG } == 2;

        $_dispatch_recursion--;

        # remove signature from list.
        $_dispatch_signatures{ $signature }--;
        delete $_dispatch_signatures{ $signature } if $_dispatch_signatures{ $signature } == 0;

        if ( $_dispatch_recursion == 0 ) {
            print "\n" if $ENV{ DEBUG } == 2; # provides a seperator between base calls.
            if ( %_dispatch_signatures ) {
                $DB::single=1;
                confess "We never completed: " . join ' ', keys %_dispatch_signatures;
            }
        }

        return $return_value;
    }

    $invocant->{_dispatch}( $invocant, @_ );
}

=head2 ::MODIFIED

 marks the variable as modified, such that we can track side-effects in BEGIN blocks

=cut

sub ::MODIFIED {
    $_[0]{_value}{modified}{ $_[0]{_value}{name} } = 1;
}

=head2 make_class

 make_class( methods => { method1 => ???, method2 => ???, ...  },
             attributes => [ attribute1,attribute2, ... ],
             parents => [ parent1,parent2, ... ],
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
##DLOCAUS $meta_Object Delcared

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

##DLOCAUS get_method_from_object depends on $meta_Object being declared.

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

##DLOCAUS %::PROTO depends on $dispatch - declared above
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

##DLOCAUS $method_new depends on $::NamedArgument

my $method_new = {
    %::PROTO,    # provides _methods, _roles, _value, _isa, _dispatch.
    _value => {
        code => sub {
            my $v = { %{ $_[0] }, _value => $_[1] };
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

my $method_APPLY = {
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

##DLOCAUS $meta_Method depends on %::PROTO

my $meta_Method = {
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

$::Method is a object, that functions as a pseudo object.  These pseudo objects
have "roles", a dispatcher $obj->{_dispatch}, a parent listing _isa, etc...

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

##DLOCAUS $::Method depends on %::PROTO, $meta_Method

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

##DLOCAUS $meta_Object depends on %::PROTO

$meta_Object = {
    %::PROTO,    # provides _methods, _roles, _value, _isa, _dispatch.

    # _name     => $_[3],
    _value => { class_name => 'Object', },
};

##DLOCAUS $meta_Object depends on $meta_Object, $method_new

$meta_Object->{_value}{methods}{WHAT} = ::DISPATCH( $::Method, 'new', sub {$::Object} );
$meta_Object->{_value}{methods}{HOW}  = ::DISPATCH( $::Method, 'new', sub {$meta_Object} );
$meta_Object->{_value}{methods}{new}  = $method_new;

=head2 $::Object

$::Object is a meta object.  $::Object contains a perl6 object.

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

=cut

##DLOCAUS $meta_Class depends on %::PROTO

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

##DLOCAUS $meta_Class -> add_method -> new depends on %::PROTO, $meta_Class

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

##DLOCAUS $::Class depends on %::PROTO

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

$::Role = make_class(
    proto   => $::Role,
    name    => 'Role',
    methods => {},
);

my $meta_Role = ::DISPATCH( $::Role, 'HOW' );

# copy Class methods
$meta_Role->{_value}{methods} = { %{ $meta_Class->{_value}{methods} } };


#--- finish Object

##DLOCAUS &meta_isa is declared

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

##############################################################################
#  $meta_Object is finished being built here.

##DLOCAUS $meta_Object is also being declared here, depends on $::Bit

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

##DLOCAUS $meta_Object->does added depends on $Str, $::Bit
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


##DLOCAUS $meta_Object depends on $::Str
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

##DLOCAUS $meta_Object depends on $::Int
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

##DLOCAUS $meta_Object depends on $::Bit
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

##DLOCAUS $meta_Object depends on $::Bit
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
##DLOCAUS $meta_Object gets FETCH (no-op)

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

##DLOCAUS $meta_Object gets $method_readonly
::DISPATCH( $meta_Object, 'add_method', 'STORE', $method_readonly );


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

# class prototypes are needed because we have circular dependencies

=head2 $::Signature

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

none

=cut

$::Signature = make_class(
    proto   => $::Signature,
    name    => 'Signature',
    methods => {}
);

=head2 $::Signature::Item

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

none

=cut

$::Signature::Item = make_class(
    proto   => $::Signature::Item,
    name    => "Signature::Item",
    methods => {}
);

=head2 $::Array

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

none

=cut

$::Array = make_class(
    proto   => $::Array,
    name    => "Array",
    methods => {}
);

=head2 $::Hash

=head3 Parents:

none

=head3 Attributes:

none

=head3 Methods:

none

=cut

$::Hash = make_class(
    proto   => $::Hash,
    name    => 'Hash',
    methods => {}
);


1;

__END__
