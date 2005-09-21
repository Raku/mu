#!/usr/bin/perl

# ChangeLog
#
# 2005-09-03
# * Fixed Code-do() return context
#
# 2005-09-01
# * New Class Perl6::Type
# * Signature can be stringified - new methods Code::signature_str and Perl6::Param::str
#
# 2005-08-22
# * bugfix - MultiSub wasn't checking parameter signature
# * @_ is bound - '.say' works!
# * Added 'slurpy' parameter
# * Compatible with Perl 5 scalar/list context
# * Removed classes Params, Signature
# * Added 'default' parameter hook
# * Added required/optional parameters
# * Param 'type' is a closure, allowing the use of 'subtype'
# * New methods: .arity, .name
# * Finished migration to Object model
#
# 2005-08-21
# * Refactored Code.pm from MetaModel t/80_Code.t
# * 'Code', 'Sub', 'MultiSub', 'Block' are now Perl6 classes

# Notes:
# Subroutine global names are not created by the Code object.
# - code.t stores names in %Perl6::[Multi]Sub::SUBS

# Algorithm for matching/binding:
# extract all +$x, ?+$x - must be Pairs
# extract all positionals, counting (but skipping) Pairs. extract slurp
# extract remaining Pairs
# $^a, $^b count like '+'

# MORE TODO - See "S06"

# TODO - Perl6::Param - PIL gives these extra attributes:
#        is_invocant - ???
#        is_writable 
#        is_lazy   - ???
#        is_lvalue - ???
#
#        Note: paramContext is an object that contains the 'type'
#           'Context' can be Slurpy or Item

# TODO - Code - PIL gives these extra attributes:
#        is_lvalue

# TODO - slurpy parameter spec should generate a boxed Array or Hash

# TODO - XXX - in Code->do() - @ret should be a boxed Array

# TODO - add want() data
# TODO - add caller() data

# TODO - junctive types

# TODO - does 'returns' calls coerce:<as> ?
# TODO - the 'match' argument to Perl6::Type->new() should be a P6 'Block' instance

# TODO - 'Pad' structure

# TODO - create base "types" and reuse - see Perl6::Param

# TODO - name_required() parameter to Perl6::Param signature ( +$x )
# TODO - 'returns'

# TODO - unify parameter types and binding with MetaModel's methods/multimethods
# TODO - multisub add_sub($sub)
# TODO - multisub .arity, .name
# TODO - escape continuations ?
# TODO - modify constant parameter is an error
# TODO - splat can't be rw
# TODO - examples - subname, tail recursion, caller
# TODO - return lvalue; Proxy
# TODO - multisubs are checking parameters twice
# TODO - parameter types - is rw, is copy, is ref, is context(Scalar)
# TODO - slurpy Code *$block
# TODO - caller context - want()
# TODO - test the integration with Value and Container types
# TODO - add hooks for signature checks, autoboxing/un-boxing, return value checking and autoboxing/un-boxing
# TODO - add support for optional parameters ?$x, pairs
# TODO - say { @_ } - test it with Int->new(3)->say
# TODO - Coro
# TODO - implement infinite list parameter - (1..Inf).shift
# TODO - box-up .arity and .name return values

use strict;
use warnings;

use Carp 'confess';

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Value;

my $class_description = '-0.0.1-cpan:FGLOCK';

{
    package Perl6::Type;

    sub new {
        # TODO - autobox/un-box hook
        my ($class, %param ) = @_;
        my ( $name, $match, $supertype ) = 
           ( $param{name}, $param{match}, $param{supertype} );
        bless {
            name =>      $name,
            match =>     $match, 
            supertype => $supertype,  
        } => $class;
    }
    
    sub name    { $_[0]{name} }
    sub match   { 
        return 0 
            if  defined $_[0]{supertype} &&
                ( ref($_[1]) ? 
                    ! $_[0]{supertype}{match}( $_[1] ) : 
                    ! $_[0]{supertype}{match}( \$_[1] )
                );
        ref($_[1]) ? 
            $_[0]{match}( $_[1] ) : 
            $_[0]{match}( \$_[1] )
    }     
}

{
    package Perl6::Param;

    sub new {
        # TODO - autobox/un-box hook
        # NOTE - default is a closure
        my ($class, %param ) = @_;
        my ( $type, $name, $default, $required, $slurpy ) = 
           ( $param{type}, $param{name}, $param{default}, $param{required}, $param{slurpy} );
        $default = sub {} unless defined $default;

        my $optional = $name =~ s/^.?\?//;
        $required = defined $required ? $required : ! $optional;

        my $slurp = $name =~ s/^.?\*//;
        $slurpy = defined $slurpy ? $slurpy : $slurp;

        $type = 
            defined $type ? $type :
            $name =~ /^\$/ ? 
                Perl6::Type->new( 
                    name => 'Any', 
                    match => sub { 1 } ) : 
            $name =~ /^\@/ ? 
                Perl6::Type->new( 
                    name => 'Array', 
                    match => sub { ref($_[0]) eq 'ARRAY' || ref($_[0]) eq 'Array' } ) : 
            $name =~ /^\%/ ? 
                Perl6::Type->new( 
                    name => 'Hash', 
                    match => sub { ref($_[0]) eq 'HASH'  || ref($_[0]) eq 'Hash' } ) : 
            $name =~ /^\&/ ?
                Perl6::Type->new( 
                    name => 'Any',  # XXX - should be Sub or Code or something
                    match => sub { 1 } ) : 
            die 'Sigil required in the parameter name ('.$name.')';
        bless {
            name =>     $name,
            type =>     $type,   
            default =>  $default,    
            required => $required,  
            slurpy =>   $slurpy,                    
        } => $class;
    }
    
    sub name     { $_[0]{name} }
    sub default  { $_[0]{default}() }
    sub required { $_[0]{required} }
    sub optional { ! $_[0]{required} }
    sub slurpy   { $_[0]{slurpy} }
    sub match_type { 
        ref($_[1]) ? 
        $_[0]{type}->match( $_[1] ) : 
        $_[0]{type}->match( \$_[1] )
    }  
    sub str { 
        my $s = '';
        $s .= $_[0]{type}->name . ' ';
        $s .= '*' if $_[0]->slurpy;
        $s .= '?' if $_[0]->optional;
        # $s .= '+' if $_[0]->name_required; -- not used in PIL-v1
        $s .= $_[0]->name;
        $s .= ' = ' . Perl6::Value::stringify( $_[0]->default ) if $_[0]->default;
        return $s;
    }      
}

class 'Code'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.body', '$.params', '$.name', '$.returns' ],
        DESTROY => sub {},
        methods => {
            # TODO
            #'num' =>  sub { Num->new( '$.unboxed' => _('$.unboxed')->num  ) },
            #'int' =>  sub { Int->new( '$.unboxed' => _('$.unboxed')->int  ) },
            'str' =>  sub { $_[0]->perl },
            'bit' =>  sub { Bit->new( '$.unboxed' => 1 ) },
            'perl' => sub { 
                #my $s = "sub "; 
                my $s = lc( ::CLASS->{instance_data}{name} ) . " ";
                $s .= Perl6::Value::stringify( _('$.name') ) . " " if defined _('$.name');
                $s .= "(" . $_[0]->signature_str . ") {...}"; 
                return Str->new( '$.unboxed' => $s ) 
            },
            'ref' =>  sub { ::CLASS }, 
            'defined' => sub { Bit->new( '$.unboxed' => 1 ) },
    
            do => sub {
                my ($self, @arguments) = @_;
                $self->check_params(@arguments)
                    || confess "Signature does not match - (" . $self->signature_str . ")";
                # my %bound_params = ::SELF->bind_params(@arguments); 
                # warn "entering sub ".$self->name;   
                my ($ret) = $self->body->( $self, @arguments );  # @_ = self + raw arguments
                # warn "RETURN $ret\n";
                # @ret is a boxed Array
                warn "Return type does not match - should return " . $self->returns->name
                    if defined $self->returns && ! $self->returns->match( $ret );
                return $ret;
            },
            arity => sub {
                scalar @{ ::SELF->params }
            }, 
            signature_str => sub {
                my $self = shift;
                my @s;
                for (my $i = 0; $i < @{ $self->params }; $i++) {
                    my $spec = ${ $self->params }[$i];
                    push @s, $spec->str;
                }
                return join( ', ', @s );        
            },
            check_params => sub {
                my ($self, @params) = @_;
                # my $max = ( scalar @params > scalar @{ $self->params } ) ? scalar @params : @{ $self->params };
                my $i = 0;
                # warn "params ". @{ $self->params } . " -- @params";
                while(1) {
                    return 1 if $i > scalar $#{ $self->params } && $i > scalar $#params;
                    return 0 if $i > scalar $#{ $self->params } && $i <= scalar $#params;
                    # warn $i;
                    my $spec = ${ $self->params }[$i];
                    my $candidate = $params[$i];
                    next if $spec->optional && ! defined $candidate;
                    if ( $spec->slurpy ) {
                        return 0 unless $spec->match_type( [ @params[$i..$#params] ] );
                        return 1;   # @params = ();
                    }
                    else {
                        return 0 unless $spec->match_type($candidate);
                    }
                }
                continue { $i++ }
                return 1;
            },
            bind_params => sub {
                my ($self, @params) = @_;
                my %bound_params;
                for (my $i = 0; $i < @{ $self->params }; $i++) {
                    my $spec = ${ $self->params }[$i];
                    my $candidate = $params[$i];
                    $candidate = $spec->default if $spec->optional && ! defined $candidate;
                    if ( $spec->slurpy ) {
                        my $ary = Array->new;
                        $ary->push( @params[$i..$#params] );
                        $bound_params{ ${$self->params}[$i]->name } = $ary;
                        # $bound_params{ ${$self->params}[$i]->name } = [ @params[$i..$#params] ];
                        @params = ();
                    }
                    else {
                        $bound_params{ ${$self->params}[$i]->name } = $candidate;
                    }
                }
                return %bound_params;        
            },
        },
    }
};

class 'Sub'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {
        attrs => [], 
        methods => {},
    },
};
    
class 'MultiSub'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {
        attrs => [ '@.subs' ],     # , '$.return_value' ],
        methods => {
            do => sub {
                my ($self, @args) = @_;
                # warn "testing multisub, ".( scalar @{ _('@.subs') } );
                foreach my $_sub ( @{ _('@.subs') } ) {
                    if ( $_sub->check_params( @args ) ) {
                        return $_sub->do(@args);
                    }
                }
                die 'No compatible MultiSub';
            },
        },
    },
};

class 'Block'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {},
};
        
1;

__END__

=pod

=head1 AUTHOR

Original code and tests by Stevan Little E<lt>stevan@iinteractive.comE<gt>

The code was refactored into it's own Class from MetaModel t/80_Code.t
by Flavio S. Glock E<lt>fglock@gmail.comE<gt>

=cut
