
=head1 NAME

Value.pm

=head1 SYNOPSIS

...

=head1 DESCRIPTION

This "file" not a "perl5 package" contains the "$::Value" object and
all subsequent objects that use $meta_Value for a parent

 * $::Int
 * $::Num
 * $::Bit
 * $::Undef

=head2 ALSO in this package

 * $::Code
 * $::List
 * $::Subset
 * $::Multi

These 3 have parents of $meta_Value, which is a lexically scoped entity to this
file. "my $meta_Value"

=cut

use v5;
use strict 'vars';
use Data::Dumper;
use Carp;

#--- Values

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

You have a Perl 6 string object that you want to compare using `eq`, for
example. The object itself would be a Perl 6 object, and you would use
p5landish on it to get at the actual string contained within it so you can
compare it.

In short, p5landish returns the actual value contained in the object.

See: http://irclog.perlgeek.de/perl6/2007-11-27#i_152004

=item print

=item FETCH

=back

=cut

$::Value = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Value,
    name    => 'Value',
    methods => {
        WHICH => sub {
            ::DISPATCH( $::Str, 'new', "$_[0]{_value}" );
        },
        p5landish => sub {
            $_[0]{_value};
        },
        print => sub {  # XXX
            print $_[0]{_value};
        },
        FETCH => sub {

            # -- FETCH is implemented in Object
            $_[0];
        },
    }
);

my $meta_Value = ::DISPATCH( $::Value, 'HOW' );

=head2 $::Str

$::Str is a $::Class object

=head3 Parents

$::Value

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

$::Str = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Str,
    name    => 'Str',
    parents => [$meta_Value],
    methods => {
        perl => sub {
            my $v = ::DISPATCH( $::Str, 'new', '\'' . $_[0]{_value} . '\'' );
        },
        Str => sub {
            $_[0];
        },
        say => sub {
            print $_[0]{_value},"\n";
        },
        true => sub {
            ::DISPATCH( $::Bit, 'new', ( $_[0]{_value} ne '' && $_[0]{_value} ne '0' ) ? 1 : 0 );
        },
        chars => sub {
            ::DISPATCH( $::Int, 'new', length( $_[0]{_value} ) );
        },
    }
);


my $meta_Str = ::DISPATCH( $::Class, 'HOW', );

=head2 $::Int

$::Int is a $::Class object

=head3 Parents

$::Value

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

$::Int = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Int,
    name    => 'Int',
    parents => [$meta_Value],
    methods => {
        perl => sub {
            my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} );
        },
        Str => sub {
            my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} );
        },
        true => sub {
            my $v = ::DISPATCH( $::Bit, 'new', ( $_[0]{_value} == 0 ? 0 : 1 ) );
        },

        # XXX - Bind to attributes fail, so to move on, a increment is interesting

        increment => sub {
            $_[0]{_value}++;
            $_[0];
        },
    }
);

my $meta_Int = ::DISPATCH( $::Int, 'HOW' );

=head2 $::Num

$::Num is a $::Class object

=head3 Parents

$::Value

=head3 Attributes

none

=head3 Methods

=over

=item perl

=item Str

=item true

=back

=cut

$::Num = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Num,
    name    => 'Num',
    parents => [$meta_Value],
    methods => {
        perl => sub {
            my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} );
        },
        Str => sub {
            my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} );
        },
        true => sub {
            my $v = ::DISPATCH( $::Bit, 'new', $_[0]{_value} == 0 ? 0 : 1 );
        },
    }
);

my $meta_Num = ::DISPATCH( $::Num, 'HOW' );

=head2 $::Bit

$::Bit is a $::Class object

=head3 Parents

$::Value

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

$::Bit = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Bit,
    name    => 'Bit',
    parents => [$meta_Value],
    methods => {
        perl => sub {
            my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} ? 'True' : 'False' );
        },
        Str => sub {
            my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value} );
        },
        true => sub {
            my $v = ::DISPATCH( $::Bit, 'new', $_[0]{_value} );
        },
    }
);

my $meta_Bit = ::DISPATCH( $::Bit, 'HOW' );

#--- back to Value

=head2 $::Undef

=head3 Parents

$::Value

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

$::Undef = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Undef,
    name    => 'Undef',
    parents => [$meta_Value],
    methods => {
        new     => sub { $_[0] },
        perl    => sub { ::DISPATCH( $::Str, 'new', 'undef' ) },
        Str     => sub { ::DISPATCH( $::Str, 'new', '' ) },
        true    => sub { ::DISPATCH( $::Bit, 'new', 0 ) },
        defined => sub { ::DISPATCH( $::Bit, 'new', 0 ) },
    }
);


=head2 $::Code

$::Code is a $::Class object

=head3 Parents

$::Value

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

my $_apply;

$_apply = sub {

    # Note call to $_apply -> in deepest part of the for loop.
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
                            $::List, 'new',
                            {   _array => [
                                    map {
                                            $param[$index] = $_;
                                            $_apply->( $self, @param );
                                        }
                                        @things
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
};

$::Code = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto        => $::Code,
    name         => 'Code',
    parents      => [$meta_Value],
    attributes => [qw | code signature ast |],
    methods      => {
        perl => sub {

            # TODO - emit from $.ast
            my $v = ::DISPATCH( $::Str, 'new', $_[0]{_value}{src} );
            return ::DISPATCH( $::Str, 'new', '{ ... }' );
        },
        'APPLY'     => $_apply,
        'p5landish' => sub {
            $_[0]{_value}{code};
        },
    },
);

my $meta_Code = ::DISPATCH( $::Code, 'HOW' );

=head2 $::List

$::List at this time just uses its parent $meta_Value ( $::Value? )

$::List is a $::Class object

=head3 Parents:

=over

=item $meta_Value

=back

=head3 Attributes:

none

=head3 Methods:

none

=cut

$::List = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::List,
    name    => 'List',
    parents => [$meta_Value],
    methods => {},
);

my $meta_List = ::DISPATCH( $::List, 'HOW' );

# TODO - finish List implementation ...

#--- Subset
# TODO - hierarchical constraints - Array of Foo
#    - use a linked list of Subsets ???
# -> you can't subclass a subset

=head2 $::Subset

$::Subset is a $::Class object

=head3 Parents:

$::Value

=head3 Attributes:

=over

=item base_class

=item block

=back

=head3 Methods:

=over

=item perl

=back

=head3 Notes

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

=cut

$::Subset = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto        => $::Subset,
    name         => 'Subset',
    parents      => [$meta_Value],
    attributes => [
        'base_class',    # Class
        'block'          # Code
    ],
    methods => {
        perl => sub {
            my $v = ::DISPATCH( $::Str, 'new', '::Subset( base_class => "...", block => "..." )' );
        },
    },
);

=head2 $::Multi

=head3 Parents:

$::Code

=head3 Attributes:

none

=head3 Methods:

=over

=item APPLY

=back

=cut

$::Multi = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto   => $::Multi,
    name    => 'Multi',
    parents => [$meta_Code],
    methods => {
        APPLY => sub {
            my $self = shift;
            my $code = ::DISPATCH( $self, 'select', ::CAPTURIZE( \@_ ) );
            ::DISPATCH( $code, 'APPLY', @_ );
        },
    }
);

1;

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
