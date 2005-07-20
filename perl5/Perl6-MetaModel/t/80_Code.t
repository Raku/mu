#!/usr/bin/perl

use strict;
use warnings;

=pod

Code
    Sub   (can return)
    Block (no return)
    
    
Code has-a two slots both of Signature type
    one is the Signature it accepts, $.params
    one is the Signature it returns, $.returns
   
- OR -
    
class Signature { has ($.params, $.returns) is Array of Param;}    

=cut

{
    package Perl6::Signature;

    sub new {
        my ($class, $params, $returns) = @_;
        bless {
            params  => $params,
            returns => $returns,
        } => $class;
    }
    
    sub params  { (shift)->{params}  }
    sub returns { (shift)->{returns} }
}

{
    package Perl6::Params;

    sub new {
        my ($class, @params) = @_;
        bless {
            params => [ @params ]
        } => $class;
    }
    
    sub check_params {
        my ($self, @params) = @_;
        for (my $i = 0; $i < scalar @params; $i++) {
            my $spec = $self->{params}->[$i];
            my $canidate = $params[$i];
            if (my $container_type = ref($canidate)) {
                return 0 unless $container_type eq $spec->container_type;
            }
            else {
                return 0 unless $spec->container_type eq 'SCALAR';
            }
        }
        return 1;
    }
    
    sub bind_params {
        my ($self, @params) = @_;
        my %bound_params;
        for (my $i = 0; $i < scalar @params; $i++) {
            $bound_params{$self->{params}->[$i]->name} = $params[$i];
        }
        return %bound_params;        
    }
}

{
    package Perl6::Param;

    sub new {
        my ($class, $name, $options) = @_;
        my $container_type = $name =~ /^\$/ ? 'SCALAR' : 
                             $name =~ /^\@/ ? 'ARRAY'  : 
                             $name =~ /^\%/ ? 'HASH'   : 
                             $name =~ /^\&/ ? 'CODE'   : 
                             undef;
        bless {
            name           => $name,
            options        => $options,
            container_type => $container_type,                             
        } => $class;
    }
    
    sub name           { (shift)->{name}           }
    sub options        { (shift)->{options}        }
    sub container_type { (shift)->{container_type} }        
}

{
    package Perl6::Code;
    
    sub new {
        my ($class, $body, $signature) = @_;
        bless {
            body      => $body,
            signature => $signature,
        } => $class;
    }
    
    sub _check_signature {
        my ($self, @arguments) = @_;
        return $self->{signature}->params->check_params(@arguments);        
    }
    
    sub _bind_params {
        my ($self, @arguments) = @_; 
        return $self->{signature}->params->bind_params(@arguments);
    }
    
    sub _call_body {
        my ($self, %bound_params) = @_;
        $self->{body}->(%bound_params);
    }
    
    sub execute {
        my ($self, @arguments) = @_;
        $self->_check_signature(@arguments) 
            || die "Signature does not match";
        my %bound_params = $self->_bind_params(@arguments);        
        $self->_call_body(%bound_params);
    }
}

{
    package Perl6::Sub;
    
    use base 'Perl6::Code';
    
    sub new {
        my ($class, @args) = @_;
        my $self = $class->SUPER::new(@args);
        $self->{return_value} = undef;
        return $self;
    }
    
    sub execute {
        my ($self, @arguments) = @_;
        $self->{return_value} = $self->SUPER::execute(@arguments);
    }
    
    sub return_value { (shift)->{return_value} }
}

{
    package Perl6::Block;

    use base 'Perl6::Code';

}

use Test::More tests => 3;

my $sub = Perl6::Sub->new(
    sub {
        my %__ = @_;
        "Hello from " . $__{'$name'};
    },
    Perl6::Signature->new(
        Perl6::Params->new(
            Perl6::Param->new('$name')
        )
    )
);
isa_ok($sub, 'Perl6::Sub');
isa_ok($sub, 'Perl6::Code');

$sub->execute('Stevan');
is($sub->return_value, 'Hello from Stevan', '... got the right return value');





