#!/usr/bin/perl

# ChangeLog
#
# 2005-08-21
# * refactored Code.pm from MetaModel t/80_Code.t
# * 'Code', 'Sub', 'MultiSub', 'Block' are now Perl6 classes

# TODO - finish instance initialization code
# TODO - more tests
# TODO - test integration with Value and Container
# TODO - *@slurpyarray params, boxed data
# TODO - implement say { @_ }

use strict;
use warnings;

use Carp 'confess';
# use PadWalker;

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Value;

my $class_description = '-0.0.1-cpan:FGLOCK';

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
    
    sub num_params { 
        scalar @{(shift)->{params}}
    }
    
    sub check_params {
        my ($self, @params) = @_;
        for (my $i = 0; $i < scalar @params; $i++) {
            my $spec = $self->{params}->[$i];
            my $candidate = $params[$i];
            if (my $container_type = ref($candidate)) {
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

class 'Code'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.body', '$.signature' ],
        DESTROY => sub {},
        methods => {
            # TODO
            #'num' =>  sub { Num->new( '$.unboxed' => _('$.unboxed')->num  ) },
            #'int' =>  sub { Int->new( '$.unboxed' => _('$.unboxed')->int  ) },
            'str' =>  sub { Str->new( '$.unboxed' => 'sub {...}' ) },
            #'bit' =>  sub { Bit->new( '$.unboxed' => _('$.unboxed')->bit  ) },
            'perl' => sub { Str->new( '$.unboxed' => 'sub {...}' ) },
            #'ref' =>  sub { ::CLASS }, 
    
            check_signature => sub {
                my ($self, @arguments) = @_;
                return ::SELF->signature->params->check_params(@arguments);        
            },
            bind_params => sub {
                my ($self, @arguments) = @_; 
                return ::SELF->signature->params->bind_params(@arguments);
            },
            call_body => sub {
                my ($self, %bound_params) = @_;     
                ::SELF->body->();
            },
            do => sub {
                my ($self, @arguments) = @_;
                ::SELF->check_signature(@arguments) 
                    || confess "Signature does not match";
                my %bound_params = ::SELF->bind_params(@arguments);        
                ::SELF->call_body(%bound_params);
            },
        },
    }
};

class 'Sub'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {
        attrs => [ '$.return_value' ],
        methods => {
            do => sub {
                _('$.return_value', ::next_METHOD() );
            #    # my ($self, @arguments) = @_;
            #    # $self->{return_value} = ::next_METHOD;  # ::do(@arguments);
            },
            # return_value => sub { ::SELF->return_value },
        },
    },
};

%Perl6::NamedSub::SUBS = ();
    
class 'NamedSub'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {
        attrs => [ '$.name', '$.return_value' ],
        methods => {
            do => sub {
                _('$.return_value', ::next_METHOD() );
            },
        },
    },
};

%Perl6::MultiSub::SUBS = ();

class 'MultiSub'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {
        attrs => [ '$.name', '@.subs', '$.return_value' ],
        # TODO - how to initialize %Perl6::MultiSub::SUBS ?   
        methods => {
            do => sub {
                my ($self, @args) = @_;
                my $num_params = scalar(@args);
                my $sub;
                foreach my $_sub ( @{ _('@.subs') } ) {
                    if ($_sub->signature->params->num_params == $num_params) {
                        $sub = $_sub;
                        last;
                    }
                }
                $sub->do(@args);
                _('$.return_value', $sub->return_value );
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
