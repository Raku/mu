#!/usr/bin/perl

use strict;
use warnings;

use Perl6::Code;
# use Data::Dumper;
use PadWalker;

use Test::More tests => 23;

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
    # $sub->return_value;
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
    # $sub->return_value;
}

# see t/80_Code.t
#    sub bind_params { ...

{
    # un-named Sub

    my $sub = mksub params('$name'), body {
            my $sub = shift;
            my %param = $sub->bind_params( @_ );  
            "Hello from $param{'$name'}";        
    };
    isa_ok($sub, 'Sub', 'un-named Sub');
    isa_ok($sub, 'Code', '... Code');
    is( $sub->perl->unboxed, 'sub {...}', '... $sub.perl' );
    is( $sub->arity, 1, '... $sub.arity' );
    is( $sub->name, undef, '... $sub.name' );

    # $sub->do('Stevan');
    is( $sub->do('Stevan'), 'Hello from Stevan', '... got the right return value');
}

{
    # un-named Sub with default parameter value

    my $sub = mksub 
        [ 
            Perl6::Param->new( 'type' => undef, 'name' => '?$name', 'default' => sub{'Flavio'} ),
        ], 
        sub {
            my $sub = shift;
            my %param = $sub->bind_params( @_ );  
            # my $name; bind_params();
            "Hello from $param{'$name'}";        
        };
    isa_ok($sub, 'Sub', 'Sub with default parameter value');
    isa_ok($sub, 'Code', '... Code');
    is( $sub->perl->unboxed, 'sub {...}', '... $sub.perl' );
    is( $sub->arity, 1, '... $sub.arity' );
    is( $sub->name, undef, '... $sub.name' );

    # $sub->do();
    is($sub->do(), 'Hello from Flavio', '... got the right return value');
}

{
    # un-named Sub with a calculated return value

    my $sub = mksub params('$name', '@others'), body {
            my $sub = shift;
            my %param = $sub->bind_params( @_ );  
            # my ($name, @others); bind_params;
            "Hello from $param{'$name'} and " . (join ", " => @{$param{'@others'}} );
    };
    isa_ok($sub, 'Sub', 'Sub with calculated return value');

    is ($sub->do('Stevan', [ 'autrijus', 'iblech', 'putter' ]),
        'Hello from Stevan and autrijus, iblech, putter', '... got the right return value');
}

{
    my $sub = mksub params('%more'), body {
            my $sub = shift;
            my %param = $sub->bind_params( @_ );  
            # my %more; bind_params;
            join ", " => sort keys %{$param{'%more'}};
    };
    isa_ok($sub, 'Sub');

    is( $sub->do({ foo => 1, bar => 2 }),
        'bar, foo', '... got the right return value');
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
            my $sub = shift;
            my %param = $sub->bind_params( @_ );  
            my @a = @{$param{'@a'}};
            # my @a; bind_params;
            return 0 unless @a;
            shift @a;
            return 1 + call_named_sub('length', \@a);
    };

    is( $sub->name, 'length', '... $sub.name' );

    is(call_named_sub('length', [ 1 .. 20 ]), 20, '... called recursive named sub');
}

{
    my $sub1 = mksub(params('@a'), body {
            my $sub = shift;
            my %param = $sub->bind_params( @_ );  
            my @a = @{$param{'@a'}};
            # warn "a only, @a";
            return call_multi_sub('length', \@a, 0);
        });
    my $sub2 = mksub(params('@a', '$acc'), body {
            my $sub = shift;
            my %param = $sub->bind_params( @_ );  
            my @a = @{$param{'@a'}};
            my $acc= $param{'$acc'};
            # warn "a @a, acc $acc";
            return $acc unless @a;
            shift @a;
            return call_multi_sub('length', \@a, $acc + 1);
        });        
    mk_multi_sub 'length' => ( $sub1, $sub2 );    
    
    is(call_multi_sub('length', [ 1 .. 20 ]), 20, '... called recursive multi sub');    
}

{
    # un-named Sub with slurpy parameter 

    my $sub = mksub 
        [ 
            Perl6::Param->new( 'type' => undef, 'name' => '@names' ),
            Perl6::Param->new( 'type' => undef, 'name' => '*@numbers' ),
        ], 
        sub {
            my $sub = shift;
            my %param = $sub->bind_params( @_ );  
            my @names = @{$param{'@names'}}; 
            my @numbers = @{$param{'@numbers'}};
            return ( \@numbers, \@names );        
        };

    my ( $x, $y ) = $sub->do( [ qw(a b c) ], 1 .. 5 );
    # $sub->return_value;
    is("@$x", '1 2 3 4 5', '... got the right return value');
    is("@$y", 'a b c', '... got the right return value');
}

{
    # "primitive" example with slurpy @_

    my $output;
    my $sub = Sub->new( 
        '$.name' => 'say',
        '$.params' => 
            [ 
                Perl6::Param->new( 'name' => '*@_' ),
            ], 
        '$.body' => 
            sub {
                my $sub = shift;
                $output = "@_\n";
                # print $output;        
            } 
    );

    $sub->do('"say()" works', '!' );
    is( $output, "\"say()\" works !\n", '... .say() works' );
}

{
    # "primitive" example with hash of parameters

    my $output;
    my $sub = Sub->new( 
        '$.name' => 'say',
        '$.params' => 
            [ 
                Perl6::Param->new( 'name' => '*@param' ),
            ], 
        '$.body' => 
            sub {
                my $sub = shift;
                my %param = $sub->bind_params( @_ );  
                $output = "@{$param{'@param'}}\n";
            } 
    );

    $sub->do('"say()" works', '!' );
    is( $output, "\"say()\" works !\n", '... parameterized .say() works' );
}

