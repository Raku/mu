#!/usr/bin/perl

package Blondie::Backend::Perl::Builtins;

use strict;
use warnings;

use Blondie::Prelude;
use Blondie::ADTs;
use Blondie::Nodes;

{
    package Blondie::Backend::Perl::Prim;
    use base qw/Blondie::Map Blondie::Node/;
    sub atomic { 1 }
}

sub native {
    my $params = @_ == 1 ? shift : { @_ };

    my $node = Blondie::Prelude->env->get($params->{name});

    my $digest = $node->digest;

    $digest => Blondie::Backend::Perl::Prim->new(
    equals => $digest,
    body => $params->{body},
    arity => $params->{arity},
    name => $params->{name},
    );
}

my %p6p5 = (
    '~' => '.',
);

my %by_digest = (
    Blondie::Prelude->env->get('$*OUT')->digest => Val(\*STDOUT),
    map { native($_) } (
        {
            arity => 3,
            name => '&ternary:<?? !!>',
            body => sub {
                $_[1] ? $_[2] : $_[3]
            }
        },
        {
            arity => 2,
            name => '&print',
            body => sub {
                my $self = shift;
                my $fh = shift->val;
                my $string = shift;
                print $fh $string;
            }
        },

        (
            map {{
                arity => 2,
                name => "&infix:<$_>",
                body => eval 'sub { $_[1] ' . (exists $p6p5{$_} ? $p6p5{$_} : $_) . ' $_[2] }' || die $@,
            }} qw(+ - == <= ~ * ** /), '<',
        ),
    ),
);

sub find {
    my $class = shift;
    my $digest = shift;

    $by_digest{$digest};
}


__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Backend::Perl::Builtins - 

=head1 SYNOPSIS

    use Blondie::Backend::Perl::Builtins;

=head1 DESCRIPTION

=cut


