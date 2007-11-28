package GLOBAL;
use strict;
no strict 'refs';

=head1 NAME

GLOBAL -  Provides globally available functions to mimic perl6

=head1 VERSION

Version 0.01

=head1 SYNOPSIS

This packages provides a bunch of functions to mimic perl6 most are exported
into the callers package.

=head1 FUNCTIONS

=cut

#require Exporter;
use Data::Dumper;
use Carp 'confess';

#@ISA = qw(Exporter);
our @EXPORT = qw(
    VAR
    print
    warn
    die
    exit
    undef
    undefine
    defined
    exists
    true
    not
    sleep
    True
    False
    substr
    chars
    match_p5rx
    require
    slurp
    keys
    push
    qw
    take
    length

    print_backtrace

    ternary_58__60__63__63__32__33__33__62_

    infix_58__60_eq_62_
    infix_58__60_ne_62_
    infix_58__60__61__61__62_
    infix_58__60__33__61__62_
    infix_58__60__126__62_
    infix_58__60__42__62_
    infix_58__60__43__62_
    infix_58__60__45__62_
    infix_58__60__47__62_
    infix_58__60__60__61__62__62_
    infix_58__60__62__61__62_
    infix_58__60__60__61__62_
    infix_58__60__62__62_
    infix_58__60__60__62_
    infix_58__60_but_62_
    infix_58__60_x_62_
    infix_58__60__47__47__62_
    infix_58__60__126__126__62_

    prefix_58__60__33__62_
    prefix_58__60__36__62_
    prefix_58__60__37__62_
    prefix_58__60__126__62_
    prefix_58__60__64__62_
    prefix_58__60__43__43__62_
    prefix_58__60__124__62_
    prefix_58__60__45__62_

);

# This is used by Visitor::Namespace.pm, to store Namespaces with global variables
$GLOBAL::Hash_KP6 = ::DISPATCH( $::Hash, 'new', );

# %*ENV
$GLOBAL::Hash_ENV = ::DISPATCH( $::Hash, 'new', map { [ ::DISPATCH( $::Str, 'new', $_ ), ::DISPATCH( $::Str, 'new', $ENV{$_} ) ] } keys %ENV );

# @*ARGS
$GLOBAL::List_ARGS = ::DISPATCH( $::Array, 'new', { _array => [ map { ::DISPATCH( $::Str, 'new', $_ ) } @ARGV ] } );

# XXX - hash key autovivification is not rw! please fix in MOP.pm
#
#    # %*ENV
#    $GLOBAL::Hash_ENV =
#        ::DISPATCH( $::Hash, 'new' );
#    for ( keys %ENV ) {
#        ::DISPATCH(
#            ::DISPATCH( $GLOBAL::Hash_ENV, 'LOOKUP', ::DISPATCH( $::Str, 'new', $_ ) ),
#            'STORE',
#            ::DISPATCH( $::Str, 'new', $ENV{$_} )
#        );
#    }

${"GLOBAL::Code_$_"} = \&{"GLOBAL::$_"} for @EXPORT;
$GLOBAL::Code_import = ::DISPATCH( $::Code, 'new', { code => \&{"GLOBAL::import"}, src => '&GLOBAL::import' } );

=head2 init_global

=cut

sub init_global {

    #print "Init GLOBAL\n";
    for (@EXPORT) {

        #print "Init \$GLOBAL::Code_$_ \n";
        ${"GLOBAL::Code_$_"} = ::DISPATCH(
            $::Code, 'new',
            {   code => ${"GLOBAL::Code_$_"},

                #src  => '&GLOBAL::'.$_,
                ast => bless {
                    namespace => [ 'GLOBAL', ],
                    name      => $_,
                    twigil    => '',
                    sigil     => '&',
                },
                'Var',

                #ast  => ::Var->new(
                #            namespace => [ 'GLOBAL', ],
                #            name      => $_,
                #            twigil    => '',
                #            sigil     => '&',
                #        ),
            },
        );
    }
}

=head2 import

# XXX - obsolete - GLOBAL is looked up at compile-time

=cut

sub import {

    #print "@_\n";
    my $pkg = _str( $_[0] );

    #print "IMPORT $pkg\n";
    ${"${pkg}::Code_$_"} = ::DISPATCH( $::Code, 'new', { code => ${"GLOBAL::Code_$_"}, src => '&GLOBAL::' . $_ } ) for @EXPORT;
}

=head2 _str

unboxes $::Str

http://en.wikipedia.org/wiki/Autoboxing#Autoboxing

=cut

sub _str {
    my $v = $_[0];
    $v = ::DISPATCH( $v, 'Str' )   if ref($v);
    $v = ::DISPATCH( $v, 'FETCH' ) if ref($v);    # .Str may return a Scalar
    return $v->{_value} if ref($v);
    $v;
}

=head2 _int

unboxes $::Int

http://en.wikipedia.org/wiki/Autoboxing#Autoboxing

=cut

sub _int {
    my $v = $_[0];
    $v = ::DISPATCH( $v, 'Int' )   if ref($v);
    $v = ::DISPATCH( $v, 'FETCH' ) if ref($v);    # .Int may return a Scalar
    return $v->{_value} if ref($v);
    $v;
}

=head2 VAR

Returns a proxy object that dispatches to the container

=cut

sub VAR {
    my $container = shift;

    return $container
        unless exists $container->{_dispatch_VAR};

    return {
        _dispatch => sub {
            #print "dispatching method $_[1]\n";
            shift;
            $container->{_dispatch_VAR}->( $container, @_ );
            }
    };
}

=head2 print

=cut

sub print {
    CORE::print map { _str($_) } @_;
    True();
}

=head1 hash functions

=head2 exists

=cut

sub exists {
    ::DISPATCH( VAR( $_[0] ), 'exists' );

    # ::DISPATCH( $_[0], 'exists' );
}

=head2 keys

=cut

sub keys {
    ::DISPATCH( $_[0], 'keys' );
}

=head1 array functions

=head2 push

=cut

sub push {
    my $self = shift;
    ::DISPATCH( $self, 'push', @_ );
}

=head1 defined(ness) and undef

=head2 undef

=cut

my $undef = ::DISPATCH( $::Undef, 'new', 0 );
sub undef {
    $undef;
}

=head2 undefine

=cut

sub undefine {
    ::DISPATCH( $_[0], 'STORE', $undef );
}

=head2 defined

=cut

sub defined {
    #print "DEFINED? \n";
    return ::DISPATCH( $::Bit, 'new', 0 )
        unless defined $_[0];
    ::DISPATCH( $_[0], 'defined' );
}

=head1 Boolean operations

=head2 true

=cut

sub true {
    ::DISPATCH( $_[0], 'true' );
}

=head2 not

=cut

sub not {
    ::DISPATCH( $::Bit, 'new', !( ::DISPATCH( $_[0], 'true' )->{_value} ) );
}


=head2 False

=cut

sub False {
    ::DISPATCH( $::Bit, 'new', 0 );
}

=head2 True

=cut

sub True {
    ::DISPATCH( $::Bit, 'new', 1 );
}

=head1 Error operations

=head2 warn

Calls CORE::warn

=cut

sub warn {
    CORE::warn( map { _str($_) } @_ );
}

=head2 die

=cut

sub die {
    Carp::confess( map { _str($_) } @_ );
}

=head2 exit

Calls CORE::exit

=cut

sub exit {
    CORE::exit();
}

=head1 odd ball operations

=cut

=head2 ternary_58__60__63__63__32__33__33__62_ : lvalue

 ternary:<?? !!>

TODO - macro

=cut

sub ternary_58__60__63__63__32__33__33__62_ : lvalue {
    #print "ternary: ",caller(2), " $#_ $_[0], $_[1]\n";
    #print ::DISPATCH( $_[0], 'true' );
    ::DISPATCH( $_[0], 'true' )->{_value} ? $_[1] : $_[2];
}

=head1 infix operations

=cut

# &&
# TODO - macro (moved to EmitPerl5 for now)
# sub infix_58__60__38__38__62_   { true($_[0])->{_value} ? $_[1] : False }

# ||
# TODO - macro (moved to EmitPerl5 for now)
# sub infix_58__60__124__124__62_ { ::DISPATCH( $::Bit, 'new', (::DISPATCH( $_[0], 'true' )->{_value} && $_[0] || ::DISPATCH( $_[1], 'true' )->{_value} && $_[1])) }

=head2 infix_58__60_eq_62_

infix:<eq>

=cut

sub infix_58__60_eq_62_ {
    ::DISPATCH( $::Bit, 'new', ( _str( $_[0] ) eq _str( $_[1] ) ) ? 1 : 0 );
}

=head2 infix_58__60_ne_62_

infix:<ne>

=cut

sub infix_58__60_ne_62_ {
    ::DISPATCH( $::Bit, 'new', ( _str( $_[0] ) ne _str( $_[1] ) ) ? 1 : 0 );
}

=head2 infix_58__60__61__61__62_

infix:<==>

=cut

sub infix_58__60__61__61__62_ {
    ::DISPATCH( $::Bit, 'new', ( _int( $_[0] ) == _int( $_[1] ) ) ? 1 : 0 );
}

=head2 infix_58__60__60__61__62__62_

infix:<<=>>

=cut

sub infix_58__60__60__61__62__62_ {
    ::DISPATCH( $::Int, 'new', ( _int( $_[0] ) <=> _int( $_[1] ) ) );
}

=head2 infix_58__60__62__61__62_

infix: <>=>

=cut

sub infix_58__60__62__61__62_ {
    ::DISPATCH( $::Bit, 'new', ( _int( $_[0] ) >= _int( $_[1] ) ) ? 1 : 0 )
}

=head2 infix_58__60__60__61__62_

infix: <<=>

=cut

sub infix_58__60__60__61__62_ {
    ::DISPATCH( $::Bit, 'new', ( _int( $_[0] ) <= _int( $_[1] ) ) ? 1 : 0 )
}

# infix: <<>
sub infix_58__60__60__62_ {
    ::DISPATCH( $::Bit, 'new', ( _int( $_[0] ) < _int( $_[1] ) )  ? 1 : 0 )
}

=head2 infix_58__60__33__61__62_

infix:<!=>

=cut

sub infix_58__60__33__61__62_ {
    ::DISPATCH( $::Bit, 'new', ( _int( $_[0] ) != _int( $_[1] ) ) ? 1 : 0 );
}

=head2 infix_58__60__126__62_

infix:<~>

=cut

sub infix_58__60__126__62_ {
    ::DISPATCH( $::Str, 'new', _str( $_[0] ) . _str( $_[1] ) )
}

=head2 infix_58__60__42__62_

infix:<*>

=cut

sub infix_58__60__42__62_  {
    ::DISPATCH( $::Int, 'new', _int( $_[0] ) * _int( $_[1] ) )
}

=head2 infix_58__60__43__62_

infix:<+>

=cut

sub infix_58__60__43__62_  {
    ::DISPATCH( $::Int, 'new', _int( $_[0] ) + _int( $_[1] ) )
}

=head2 infix_58__60__45__62_

infix:<->

=cut

sub infix_58__60__45__62_  {
    ::DISPATCH( $::Int, 'new', _int( $_[0] ) - _int( $_[1] ) )
}

=head2 infix_58__60__47__62_

infix:</>   XXX - Num

This will die, if you attempt to divide by zero.

=cut

sub infix_58__60__47__62_ {
    Carp::confess("Divide by zero error") unless _int( $_[1] );

    ::DISPATCH( $::Int, 'new', _int( $_[0] ) / _int( $_[1] ) )
}

=head2 infix_58__60__62__62_

infix:<>>

=cut

sub infix_58__60__62__62_ {
    ::DISPATCH( $::Bit, 'new', ( _int( $_[0] ) > _int( $_[1] ) ) ? 1 : 0 )
}

=head2 infix_58__60__47__47__62_

infix:<//>

=cut

sub infix_58__60__47__47__62_ {
    die "not implemented";
}

=head2 infix_58__60_but_62_

infix:<but>

=cut

sub infix_58__60_but_62_ {
    my $value = ::DISPATCH( $_[0], 'FETCH' );
    my $but   = ::DISPATCH( $_[1], 'FETCH' );
    my $class = ::DISPATCH( ::DISPATCH( $but, 'WHAT' ), 'Str' )->{_value};

    #print "class: $class \n";

    # XXX fixme
    my $meth = "";

    #$meth = "Str"  if $class eq "Str";
    #$meth = "Int"  if $class eq "Int";
    $meth = "true" if $class eq "Bit";

    # ???
    #die "don't know how to implement 'but' with $class"
    #    unless $meth;

    $value = { %{$value} };

    # XXX
    $value->{_methods}{$meth} = ::DISPATCH( $::Method, 'new', sub {$but} )
        if $meth;

    $value->{_methods}{$class} = ::DISPATCH( $::Method, 'new', sub {$but} );

    return $value;
}

=head2 infix_58__60_x_62_

infix:<x>

=cut

sub infix_58__60_x_62_ {
    return ::DISPATCH( $::Str, 'new', _str( $_[0] ) x _int( $_[1] ) );
}

=head2 infix_58__60__126__126__62_

infix:<~~>

=cut

sub infix_58__60__126__126__62_ {
    return ::DISPATCH( $_[1], 'smartmatch', $_[0] );
}

=head1 prefix operations

=cut

=head2 prefix_58__60__36__62_

prefix:<$>

XXX: TODO: This should force scalar|hash context.

=cut

sub prefix_58__60__36__62_ {
    ::DISPATCH( $_[0], 'scalar' )
}

=head2 prefix_58__60__37__62_

prefix:<%>

=cut

sub prefix_58__60__37__62_ {
    ::DISPATCH( $_[0], 'hash' )
}

=head2 prefix_58__60__126__62_

prefix:<~>

=cut

sub prefix_58__60__126__62_ {
    ::DISPATCH( $::Str, 'new', _str( $_[0] ) )
}

=head2 prefix_58__60__33__62_

prefix:<!>

=cut

sub prefix_58__60__33__62_ {
    ::DISPATCH( $::Bit, 'new', !( ::DISPATCH( $_[0], 'true' )->{_value} ) )
}


=head2 prefix_58__60__64__62_

prefix:<@>

=cut

sub prefix_58__60__64__62_ {
    ::DISPATCH( $_[0], 'array' );
}

=head2 prefix_58__60__43__43__62_

prefix:<++>

=cut

sub prefix_58__60__43__43__62_ {
    my $counter = $_[0];
    $counter->{_dispatch_VAR}( $counter, 'STORE', ::DISPATCH( $::Int, 'new', _int($counter) + 1 ) );
}

=head2 prefix_58__60__124__62_

prefix:<|>

explodes a Capture

=cut

sub prefix_58__60__124__62_ {
    my $capture = shift;
    my $array = ::DISPATCH( $capture, 'array' );
    @{ $array->{_value}{_array} };
}

=head2 prefix_58__60__45__62_

prefix:<->

=cut

sub prefix_58__60__45__62_ {
    ::DISPATCH( $::Int, 'new', -_int( $_[0] ) );
}

=head1 Misc. functions

=head2 sleep

Calls CORE::sleep

=cut

sub sleep {
    CORE::sleep( _int( $_[0] ) );
    return True;
}

=head2 TODO

=cut

sub TODO {
    confess("TODO");
}

=head2 qw

=cut

sub qw {
    ::DISPATCH( $::Array, 'new', { _array => [ map { ::DISPATCH( $::Str, 'new', $_ ) } eval( "qw/" . _str( $_[0] ) . "/" ) ] } );
}

=head2 take

=cut

sub take {

    # this assumes 'Coro' and 'Scalar::Util' are already loaded

    # XXX this "does nothing" but gather has problems without it ???
    _str( $_[0] );

    push @{ $::GATHER{ Scalar::Util::refaddr($Coro::current) } }, ::DISPATCH( $_[0], 'FETCH' );
    Coro::cede();
    return $_[0];
}

=head2 require

=cut

sub require {
    eval "CORE::require " . _str( $_[0] );
    die $@ if $@;
}

=head2 slurp

=cut

sub slurp {

    #warn '#XXX# Slurp only a prototype here!';
    return ::DISPATCH( $::Str, 'new', ( join '', <> ) );
}

=head2 match_p5rx

=cut

sub match_p5rx {
    my ( $regex, $string, $pos ) = ( _str( $_[0] ), _str( $_[1] ), _int( $_[2] ) );
    if ( $_[1] == $::Undef ) {
        $string = _str($_);
    }
    pos($string) = $pos;
    if ( $ENV{KP6_TOKEN_DEBUGGER} ) {
        print ">>> inside p5 token $regex at $pos of ($string)\n";
    }
    my $bool = $string =~ /\G$regex/gc;

    #print "regex:<$regex> string:<$string>\n";
    if ($bool) {
        if ( $ENV{KP6_TOKEN_DEBUGGER} ) {
            print "<<< p5 token $regex returned true\n";
        }

        #print "matched up to:",pos($string),"\n";
        my $m = ::DISPATCH(
            $::Match, 'new',
            ::DISPATCH(
                $::NamedArgument,
                "new",
                {   _argument_name_ => ::DISPATCH( $::Str, 'new', 'match_str' ),
                    value           => $_[1]
                }
            ),
            ::DISPATCH(
                $::NamedArgument,
                "new",
                {   _argument_name_ => ::DISPATCH( $::Str, 'new', 'from' ),
                    value           => $_[2]
                }
            ),
            ::DISPATCH(
                $::NamedArgument,
                "new",
                {   _argument_name_ => ::DISPATCH( $::Str, 'new', 'to' ),
                    value           => ::DISPATCH( $::Int, 'new', pos($string) ),
                }
            ),
            ::DISPATCH(
                $::NamedArgument,
                "new",
                {   _argument_name_ => ::DISPATCH( $::Str, 'new', 'bool' ),
                    value           => ::DISPATCH( $::Bit, 'new', 1 )
                }
            ),
        );
        return $m;
    }
    else {
        if ( $ENV{KP6_TOKEN_DEBUGGER} ) {
            print "<<< p5 token $regex returned false\n";
        }

        #print "false match\n";
        my $m = ::DISPATCH(
            $::Match, 'new',
            ::DISPATCH(
                $::NamedArgument,
                "new",
                {   _argument_name_ => ::DISPATCH( $::Str, 'new', 'bool' ),
                    value           => ::DISPATCH( $::Bit, 'new', 0 )
                }
            ),
        );
        return $m;
    }
}

=head2 print_backtrace

=cut

sub print_backtrace {

    package DB;
    my $depth = 0;
    while ( my ( $package, $filename, $line, $subroutine, $hasargs, $wantarray, $evaltext, $is_require, $hints, $bitmask ) = caller( ++$depth ) ) {
        if ( $subroutine ne '(eval)' ) {
            print "$subroutine(", join(
                ',',
                map {
                    package GLOBAL;
                    if ( ref $_ ) {
                        _str($_);
                    }
                    else {
                        $_;
                    }
                    } @DB::args
                ),
                ")\n";
        }
    }
}

# -------

# match the parameter list to the signature specification
# XXX TODO
#print "# CAPTURIZE: now at Code == $::ROUTINE \n";
#my $signature = ::DISPATCH( ::DISPATCH( $::ROUTINE, 'signature' ), 'array' );
# -- get the signature specification
#my @sigs = @{ $signature->{_value}{_array} };
#print "# sig = @{[ keys %{ $sigs[0] } ]} \n";

=head2 ::CAPTURIZE

=cut

sub ::CAPTURIZE {
    my @params = @{ $_[0] };    # get the runtime parameter list

    my @array;
    my @hash;
    my $add_parameter;
    $add_parameter = sub {
        my $p = $_[0];
        if ( ::DISPATCH( $p, 'does', $::NamedArgument )->{_value} ) {
            push @hash, [ ::DISPATCH( $p, '_argument_name_' ), ::DISPATCH( $p, 'value' ), ];
        }
        elsif ( ::DISPATCH( $p, 'does', $::List )->{_value} ) {
            my $list = ::DISPATCH( $p, 'eager' );
            $add_parameter->($_) for @{ ::DISPATCH( $p, 'eager' )->{_value}{_array} };
        }
        else {
            push @array, $p;
        }
    };

    $add_parameter->($_) for @params;

    ::DISPATCH(
        $::Capture,
        'new',
        {   invocant => undef,                                                # TODO
            array => ::DISPATCH( $::Array, 'new', { _array => \@array, } ),
            hash => ::DISPATCH( $::Hash, 'new', @hash ),
        }
    );
}

=head1 String functions

=cut

=head2 substr

=cut

sub substr {
    #print " substr() parameters: ",
    #    join( ", " ,
    #        _str( $_[0] ), _int( $_[1] ), _int( $_[2] ), _str( $_[3] )
    #    )
    #;

    if ( $#_ == 1 ) {
        return ::DISPATCH( $::Str, 'new', substr( _str( $_[0] ), _int( $_[1] ) ) );
    }

    if ( $#_ == 2 ) {

        #warn "STACK TRACE--------------------------------------\n";
        #warn join("\n", map { join ', ', caller($_) } 1..6)."\n";
        return ::DISPATCH( $::Str, 'new', substr( _str( $_[0] ), _int( $_[1] ), _int( $_[2] ) ) );
    }

    if ( $#_ == 3 ) {
        return ::DISPATCH( $::Str, 'new', substr( _str( $_[0] ), _int( $_[1] ), _int( $_[2] ), _str( $_[3] ) ) );
    }

    die "Not enough arguments for substr";
}

=head2 chars

=cut

sub chars {
    ::DISPATCH( $::Int, 'new', length( _str( $_[0] ) ) );
}

=head2 length

dies, this is not valid in perl6

=cut

sub length {
    die "length() is not specced - do you mean chars() ? ";
}

{
    GLOBAL::init_global;
}

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

