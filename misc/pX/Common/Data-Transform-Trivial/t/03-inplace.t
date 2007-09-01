#!/usr/bin/perl
use strict;
use warnings;
use Test::More qw(no_plan);
use vars qw($_T $_P @_L);
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

sub foo {
    return SimpleNode->new('foo',@_);
}

{
my $tree=foo({a=>1},[
    foo({a=>2}),
    foo({a=>3}),
    foo({a=>4}),
]);

my $t=Transform->new([
    Rule->new('insert',sub {$_P==1},
              sub {
                  splice @{$_->parent->children},1,1,foo({a=>5});
              }),
    Rule->new('insert',sub{1},
              sub { $_T->apply('insert',$_->children) },
              0.5),
    Rule->new('print',sub{1},
              sub { return $_->attributes->{a},$_T->apply('print',$_->children) }),
]);

{
my $ret=join '',$t->apply('print',$tree);
is($ret,'1234','printing ok');
}

{
$t->apply('insert',$tree);
my $ret=join '',$t->apply('print',$tree);
is($ret,'1254','inserting ok');
}
}


{
my $tree=foo({a=>1},[
    foo({a=>2}),
    foo({a=>3}),
    foo({a=>4}),
]);

my $t=Transform->new([
    Rule->new('insert-and-print',sub {$_P==1 and $_->attributes->{a} != 5},
              sub {
                  my $newnode=foo({a=>5});
                  splice @{$_->parent->children},1,1,$newnode;
                  splice @_L,1,1,$newnode;
                  $_P--;
                  return;
              }),
    Rule->new('insert-and-print',sub{1},
              sub { return $_->attributes->{a},$_T->apply('insert-and-print',$_->children) },
              0.5),
]);

{
my $ret=join '',$t->apply('insert-and-print',$tree);
is($ret,'1254','updating context');
}
}

