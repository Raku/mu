
# This is a Perl5 file

# ChangeLog
#
# 2005-08-31
# * New methods: tied(), keys(), values(), pairs(), kv()

# * Fixed elems(), buckets() to return boxed Int, Str
# * hash iterator (firstkey/nextkey) works
#
# 2005-08-19
# * New Perl6 class 'Hash'
#
# 2005-08-18
# * New Perl5 class "Perl6::Container::Hash::Object"
#   implements a hash in which the keys can be objects
#
# 2005-08-14
# * added functions clone(), elems(), buckets()

# TODO - warn for odd number of elements, on construction
# TODO - keys(), values(), pairs(), kv() - lazy (non-infinite)
# TODO - each() methods
# TODO - hash cells with rw, ro, binding hash elements
# TODO - tied hashes
# TODO - hash slices
# TODO - %a = %b - whole hash fetch/store
# TODO - pick()

# TODO - (iblech) probably with a warning "uninitialized warning used in numeric contect"
#        (Same for hashes: %h{undef} =:= %h{""})
# TODO - is (undef=>undef) a valid Pair?
#        fglock PIL-Run currently prints {('undef', undef)}
#        buu fglock: For which?
#        fglock for { undef=>undef }
#        iblech fglock: Right, I'd think so (and this is how I've implemented it in PIL2JS). 
#        But: By default, hash can autoconvert their keys to Strs, so {undef()=>undef} would have 
#        the same effect as {""=>undef}. But if a hash is declared with shape(Any), {undef()=>undef} 
#        should create a hash with .pairs[0].key being undef

# TODO - (for PIL-Run)
#        buu iblech: Er, let me rephrase. Why did it print an error message?
#        iblech buu: Because {undef,undef} is not a hash, but a Code, and Pugs tried to coerce 
#        the Code into a Hash, but failed
#        buu Er, so how do you create a hash ref?
#        iblech buu: \hash(undef,undef), {undef() => undef}, {pair undef, undef}, etc.

# TODO - test - how does a scalar that contains a hash is accessed?
# TODO - test $x := %hash - 'undefine $x'
# TODO - test %hash := $x - error if $x is not bound to a hash
# TODO - tieable hash - cleanup AUTOLOAD
# TODO - test 'readonly'

# Notes:
# * Cell is implemented in the Perl6::Container::Scalar package

use strict;

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Value;
use Perl6::Container::Scalar;

my $class_description = '-0.0.1-cpan:FGLOCK';

class 'Hash'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ [ '$:cell' => { 
                        access => 'rw', 
                        build => sub { 
                            my $cell = Perl6::Cell->new;
                            my $h = bless {}, 'Perl6::Container::Hash::Object';
                            $cell->{v} = $h;
                            $cell->{type} = 'Hash';
                            return $cell;
                        } } ] ],
        DESTROY => sub {
            $_[0]->{'instance_data'}{'$:cell'} = undef;  
        },
        methods => { 

            # %a := %b 
            'bind' =>     sub {
                my ( $self, $thing ) = @_;
                die "argument to Hash bind() must be a Hash"
                    unless $thing->cell->{type} eq 'Hash';
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

            'elems' =>    sub { Int->new( '$.unboxed' =>
                                _('$:cell')->{tied} ? 
                                _('$:cell')->{tied}->elems :
                                Perl6::Container::Hash::elems( _('$:cell')->{v} ) )
            },
            'buckets' =>  sub { Str->new( '$.unboxed' =>
                                _('$:cell')->{tied} ? 
                                _('$:cell')->{tied}->buckets :
                                Perl6::Container::Hash::buckets( _('$:cell')->{v} ) )
            },
            'pairs' => sub { 
                my $key = $_[0]->firstkey;
                my $ary = Array->new;
                while ( defined $key ) {
                    $ary->push( Pair->new( 
                        '$.key' =>   $key, 
                        '$.value' => $_[0]->fetch( $key ) ) );
                    $key = $_[0]->nextkey;
                }
                return $ary;
            },
            'kv' => sub { 
                my $key = $_[0]->firstkey;
                my $ary = Array->new;
                while ( defined $key ) {
                    $ary->push( $key, $_[0]->fetch( $key ) );
                    $key = $_[0]->nextkey;
                }
                return $ary;
            },
            'keys' => sub { 
                my $key = $_[0]->firstkey;
                my $ary = Array->new;
                while ( defined $key ) {
                    $ary->push( $key );
                    $key = $_[0]->nextkey;
                }
                return $ary;
            },
            'values' => sub { 
                my $key = $_[0]->firstkey;
                my $ary = Array->new;
                while ( defined $key ) {
                    $ary->push( $_[0]->fetch( $key ) );
                    $key = $_[0]->nextkey;
                }
                return $ary;
            },
            'str' => sub { 
                my $key = $_[0]->firstkey;
                my $value;
                my @pairs;
                while ( defined $key ) {
                    $value = $_[0]->fetch( $key );
                    my $p = Pair->new( '$.key'=>$key, '$.value'=>$value );
                    push @pairs, $p->str->unboxed;
                    $key = $_[0]->nextkey;
                }
                Str->new( '$.unboxed' => 
                    '{' . 
                    join(', ', @pairs) . 
                    '}' 
                );
            },
            'perl' => sub { $_[0]->str },

            'AUTOLOAD' => sub {
                my ($self, @param) = @_;
                my $method = ::AUTOLOAD($self);
                # TODO - add support for tied hash
                # TODO - check if scalar := hash works properly
                my $tmp = _('$:cell')->{tied} ? _('$:cell')->{tied} : _('$:cell')->{v};
                # warn ref($tmp), ' ', $method, " @param == " . $tmp->$method( @param );
                return $tmp->$method( @param );
            },
        },
    }
};

# ----- unboxed functions

package Perl6::Container::Hash::Object;

our $obj_id = '**ObJecT**' . rand;

sub get_id {
    my $key = shift;
    return $obj_id unless defined $key;
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
        $s = $obj_id . $key->id
    }
    return $s;
}

sub store {
    my ( $this, $key, $value ) = @_;
    my $s = get_id( $key );
    $this->{$s} = [ $key, $value ];
    return $value;
}
sub fetch {
    my ( $this, $key ) = @_;
    my $s = get_id( $key );
    $this->{$s}[1];
    # warn "fetching " . $this->{$s}[1];
}
sub firstkey {
    my ( $this ) = @_;
    keys %$this;  # force reset the iterator
    my $s = each %$this;
    return unless defined $s;
    $this->{$s}[0];
}
sub nextkey {
    my ( $this, $key ) = @_;
    my $s = each %$this;
    return unless defined $s;
    $this->{$s}[0];
}
sub exists {
    my ( $this, $key ) = @_;
    my $s = get_id( $key );
    exists $this->{$s};
}
sub delete {
    my ( $this, $key ) = @_;
    my $s = get_id( $key );
    delete $this->{$s};
}
sub clear {
    my ( $this ) = @_;
    %$this = ();
}
sub scalar {
    my ( $this ) = @_;
    0 + %$this;
}

package Perl6::Container::Hash;

sub clone { 
    my $tmp = { %{ $_[0] } };
    $tmp;
}

sub elems {
    my @tmp = %{ $_[0] };
    @tmp / 2
}

sub buckets { scalar %{ $_[0] } }

1;
__END__

=head1 NAME

Perl6::Container::Hash - Perl extension for Perl6 "Hash" class

=head1 SYNOPSIS

  use Perl6::Container::Hash;

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
