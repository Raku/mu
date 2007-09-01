#!/usr/bin/perl
use strict;
use warnings;
use Test::More qw(no_plan);

BEGIN {
    eval 'use Log::Log4perl qw(:easy)' if $ENV{DEBUG};
    eval 'use Log::Log4perl::Resurrector;' if $ENV{DEBUG};
}
use Data::Transform::Trivial;
use Data::Transform::Trivial::Rule;
eval 'Log::Log4perl->easy_init($DEBUG)' if $ENV{DEBUG};
use lib 't/lib'; # might not always work
use SimpleNode;

BEGIN{
*Transform::=\*Data::Transform::Trivial::;
*Rule::=\*Data::Transform::Trivial::Rule::;
}

sub table {
    return SimpleNode->new('table',{},@_);
}
sub row {
    return SimpleNode->new('row',{},@_);
}
sub cell {
    return SimpleNode->new('cell',{},@_);
}

my $matrix=table([
    row([
        cell([1]),
        cell([2]),
        cell([3]),
    ]),
    row([
        cell([4]),
        cell([5]),
        cell([6]),
    ]),
    row([
        cell([7]),
        cell([8]),
        cell([9]),
    ]),
]);

my $printer=Transform->new([
    Rule->new('print',sub {$_->name() eq 'table'},
              sub {return join '',$main::_T->apply('print',$_->children)}),
    Rule->new('print',sub {$_->name() eq 'row'},
              sub {return '['.(join ', ',$main::_T->apply('print',$_->children))."]\n"}),
    Rule->new('print',sub {$_->name() eq 'cell'},
              sub {return join '',$_->children}),
]);

my ($print1)=$printer->apply('print',$matrix);
is($print1,<<'EOT',"printing works");
[1, 2, 3]
[4, 5, 6]
[7, 8, 9]
EOT


my $transposer=Transform->new([
    Rule->new('trans',sub {$_->name() eq 'table'},
              sub {
                  return table([$main::_T->apply('trans',$_->children)]) }),
    Rule->new('trans',sub {$_->name() eq 'row'},
              sub {
                  my $pos=$main::_POS;
                  return row([ $main::_T->apply
                                   ('trans',
                                    $main::_T->find($main::_C,
                                                    sub {
                                                        return grep {$_->position == $pos }
                                                            $_->children;
                                                    }),
                                ) ])
              }),
    Rule->new('trans',sub {$_->name() eq 'cell'},
              sub { return cell([$_->children]) }),
]);

my ($xirtam)=$transposer->apply('trans',$matrix);

my ($print2)=$printer->apply('print',$xirtam);
is($print2,<<'EOT',"transposing works");
[1, 4, 7]
[2, 5, 8]
[3, 6, 9]
EOT
