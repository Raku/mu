#!/usr/bin/perl

use strict;
use warnings;

use Perl6::Code;
# use Data::Dumper;
use PadWalker;

use Test::More tests => 19;

%Perl6::MultiSub::SUBS = ();
%Perl6::NamedSub::SUBS = ();
sub body (&) { @_ }
sub params {
    [ map { Perl6::Param->new( 'type' => undef, 'name' => $_ ) } @_ ]  
}
sub mksub {
    my ($params, $body) = @_;
    return Sub->new( '$.body' => $body, '$.params' => $params);
}
sub mk_named_sub {
    my ($name, $params, $body) = @_;
    my $sub = Sub->new( '$.name' => $name, '$.body' => $body, '$.params' => $params);
    $Perl6::NamedSub::SUBS{$name} = $sub;
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
    my $sub = MultiSub->new( '$.name' => $name, '@.subs' => \@subs);
    $Perl6::MultiSub::SUBS{$name} = $sub;
}
sub call_multi_sub {
    my ($name, @args) = @_;
    (exists $Perl6::MultiSub::SUBS{$name})
        || die "No sub found called '$name'";
    my $sub = $Perl6::MultiSub::SUBS{$name};
    $sub->do(@args);
    $sub->return_value;
}

sub bind_params {
    my $local_pad = PadWalker::peek_my(1);
    #print "local" . Data::Dumper::Dumper $local_pad;   
    my $pad = PadWalker::peek_my(2);   
    #print "pad" . Data::Dumper::Dumper $pad;  
    foreach my $local (keys %{$local_pad}) {
        if (ref($local_pad->{$local}) eq 'SCALAR') {
            ${$local_pad->{$local}} = $pad->{'%bound_params'}{$local};                    
        }
        elsif (ref($local_pad->{$local}) eq 'ARRAY') {
            @{$local_pad->{$local}} = @{$pad->{'%bound_params'}{$local}};                     
        }
        elsif (ref($local_pad->{$local}) eq 'HASH') {
            %{$local_pad->{$local}} = %{$pad->{'%bound_params'}{$local}};                     
        }        
    }   
}


{
    # un-named Sub

    my $sub = mksub params('$name'), body {
        my $name; bind_params();
        "Hello from $name";        
    };
    isa_ok($sub, 'Sub');
    isa_ok($sub, 'Code');
    is( $sub->perl->unboxed, 'sub {...}', '... $sub.perl' );
    is( $sub->arity, 1, '... $sub.arity' );
    is( $sub->name, undef, '... $sub.name' );

    $sub->do('Stevan');
    is($sub->return_value, 'Hello from Stevan', '... got the right return value');
}

{
    # un-named Sub with default parameter value

    my $sub = mksub 
        [ 
            Perl6::Param->new( 'type' => undef, 'name' => '?$name', 'default' => sub{'Flavio'} ),
        ], 
        sub {
            my $name; bind_params();
            "Hello from $name";        
        };
    isa_ok($sub, 'Sub');
    isa_ok($sub, 'Code');
    is( $sub->perl->unboxed, 'sub {...}', '... $sub.perl' );
    is( $sub->arity, 1, '... $sub.arity' );
    is( $sub->name, undef, '... $sub.name' );

    $sub->do();
    is($sub->return_value, 'Hello from Flavio', '... got the right return value');
}

{
    # un-named Sub with a calculated return value

    my $sub = mksub params('$name', '@others'), body {
        my ($name, @others); bind_params;
        "Hello from $name and " . (join ", " => @others);
    };
    isa_ok($sub, 'Sub');

    $sub->do('Stevan', [ 'autrijus', 'iblech', 'putter' ]);
    is($sub->return_value, 'Hello from Stevan and autrijus, iblech, putter', '... got the right return value');
}

{
    my $sub = mksub params('%more'), body {
        my %more; bind_params;
        join ", " => sort keys %more;
    };
    isa_ok($sub, 'Sub');

    $sub->do({ foo => 1, bar => 2 });
    is($sub->return_value, 'bar, foo', '... got the right return value');
}

=pod

Can't yet handle &sub params

{
    my $sub = mksub params('$code', '@rest'), body {
        my ($code, @rest); bind_params;
        $code->(@rest);
    };
    isa_ok($sub, 'Perl6::Sub');

    $sub->do(sub { join "|" => @_ }, [ 1, 2, 3, 4 ]);
    is($sub->return_value, '1|2|3|4', '... got the right return value');
}
=cut

# named subs ...

{
    my $sub = mk_named_sub 'length' => params('@a'), body {
        my @a; bind_params;
        return 0 unless @a;
        shift @a;
        return 1 + call_named_sub('length', \@a);
    };

    is( $sub->name, 'length', '... $sub.name' );

    is(call_named_sub('length', [ 1 .. 20 ]), 20, '... called recursive named sub');
}

{
    mk_multi_sub 'length' => (
        mksub(params('@a'), body {
            my @a; bind_params;
            return call_multi_sub('length', \@a, 0);
        }),
        mksub(params('@a', '$acc'), body {
            my (@a, $acc); bind_params;
            return $acc unless @a;
            shift @a;
            return call_multi_sub('length', \@a, $acc + 1);
        })        
    );    
    
    is(call_multi_sub('length', [ 1 .. 20 ]), 20, '... called recursive multi sub');    
}

