#!/usr/bin/perl

use strict;
use warnings;

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
    
    sub signature { (shift)->{signature} }
    
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
    
    sub do {
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
    
    sub do {
        my ($self, @arguments) = @_;
        $self->{return_value} = $self->SUPER::do(@arguments);
    }
    
    sub return_value { (shift)->{return_value} }
}

{
    package Perl6::NamedSub;
    
    use base 'Perl6::Sub';
    
    %Perl6::NamedSub::SUBS = ();
    
    sub new {
        my ($class, $name, @args) = @_;
        my $self = $class->SUPER::new(@args);
        $Perl6::NamedSub::SUBS{$name} = $self;
        return $self;
    }
}

{
    package Perl6::MultiSub;
    
    %Perl6::MultiSub::SUBS = ();
    
    sub new {
        my ($class, $name, @subs) = @_;
        my $self = bless {
            subs => \@subs,
            rval => undef
        }, $class;
        $Perl6::MultiSub::SUBS{$name} = $self;
        return $self;
    }    
    
    sub do {
        my ($self, @args) = @_;
        my $num_params = scalar(@args);
        my $sub;
        foreach my $_sub (@{$self->{subs}}) {
            if ($_sub->signature->params->num_params == $num_params) {
                $sub = $_sub;
                last;
            }
        }
        $sub->do(@args);
        $self->{return_value} = $sub->return_value;
    }
    
    sub return_value { (shift)->{return_value} }
    
}

{
    package Perl6::Block;
    use base 'Perl6::Code';
}

use Test::More tests => 11;

sub body (&) { @_ }
sub params {
    Perl6::Signature->new(Perl6::Params->new(map { Perl6::Param->new($_) } @_))    
}
sub mksub {
    my ($params, $body) = @_;
    return Perl6::Sub->new($body, $params);
}
sub mk_named_sub {
    my ($name, $params, $body) = @_;
    return Perl6::NamedSub->new($name, $body, $params);
}
sub call_named_sub {
    my ($name, @args) = @_;
    (exists $Perl6::NamedSub::SUBS{$name})
        || die "No sub found called '$name'";
    my $sub = $Perl6::NamedSub::SUBS{$name};
    $sub->do(@args);
    $sub->return_value;
}
sub mk_multi_sub {
    my ($name, @subs) = @_;
    return Perl6::MultiSub->new($name, @subs);
}
sub call_multi_sub {
    my ($name, @args) = @_;
    (exists $Perl6::MultiSub::SUBS{$name})
        || die "No sub found called '$name'";
    my $sub = $Perl6::MultiSub::SUBS{$name};
    $sub->do(@args);
    $sub->return_value;
}

{
    my $sub = mksub params('$name'), body {
        my %__ = @_;
        "Hello from " . $__{'$name'};        
    };
    isa_ok($sub, 'Perl6::Sub');
    isa_ok($sub, 'Perl6::Code');

    $sub->do('Stevan');
    is($sub->return_value, 'Hello from Stevan', '... got the right return value');
}

{
    my $sub = mksub params('$name', '@others'), body {
        my %__ = @_;
        "Hello from " . $__{'$name'} . " and " . (join ", " => @{$__{'@others'}});
    };
    isa_ok($sub, 'Perl6::Sub');

    $sub->do('Stevan', [ 'autrijus', 'iblech', 'putter' ]);
    is($sub->return_value, 'Hello from Stevan and autrijus, iblech, putter', '... got the right return value');
}

{
    my $sub = mksub params('%more'), body {
        my %__ = @_;
        join ", " => sort keys %{$__{'%more'}};
    };
    isa_ok($sub, 'Perl6::Sub');

    $sub->do({ foo => 1, bar => 2 });
    is($sub->return_value, 'bar, foo', '... got the right return value');
}

{
    my $sub = mksub params('&code', '@rest'), body {
        my %__ = @_;
        $__{'&code'}->(@{$__{'@rest'}});
    };
    isa_ok($sub, 'Perl6::Sub');

    $sub->do(sub { join "|" => @_ }, [ 1, 2, 3, 4 ]);
    is($sub->return_value, '1|2|3|4', '... got the right return value');
}

# named subs ...

{
    mk_named_sub 'length' => params('@a'), body {
        my %__ = @_;
        return 0 unless @{$__{'@a'}};
        shift @{$__{'@a'}};
        return 1 + call_named_sub('length', $__{'@a'});
    };

    is(call_named_sub('length', [ 1 .. 20 ]), 20, '... called recursive named sub');
}

{
    mk_multi_sub 'length' => (
        mksub(params('@a'), body {
            my %__ = @_;
            return call_multi_sub('length', $__{'@a'}, 0);
        }),
        mksub(params('@a', '$acc'), body {
            my %__ = @_;
            return $__{'$acc'} unless @{$__{'@a'}};
            shift @{$__{'@a'}};
            return call_multi_sub('length', $__{'@a'}, $__{'$acc'} + 1);
        })        
    );    
    
    is(call_multi_sub('length', [ 1 .. 20 ]), 20, '... called recursive multi sub');    
}

