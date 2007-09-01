#!/usr/bin/perl
use strict;
use warnings;
use Test::More qw(no_plan);
use List::Util qw(sum);
use vars qw($_T);
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
sub bar {
    return SimpleNode->new('bar',@_);
}
my $tree=foo({a=>1},[
    bar({},[
        foo({a=>3}),
        foo({a=>4}),
        bar(),
    ]),
    bar({t=>0}),
]);

sub match_any {1};

sub match_foo {
    return $_->name() eq 'foo';
}

sub match_bar {
    return $_->name() eq 'bar';
}
{
my $t=Transform->new([
    Rule->new('fl',\&match_any,
              sub {
                  return 
                      substr($_->name(),0,1)
                      .(join '',$_T->apply('fl',$_->children));
              }),
]);

my ($ret)=$t->apply('fl',$tree);
is($ret,'fbffbb','simple first-letter');
}
sub match_with_a {
    return exists ${$_->attributes}{a};
}

{
my $t=Transform->new([
    Rule->new('count_a',\&match_with_a,
              sub {
                  return 1+(sum($_T->apply('count_a',$_->children))||0);
              }),
    Rule->new('count_a',\&match_any,
              sub {
                  return sum($_T->apply('count_a',$_->children))||0;
              },0.5),
]);

my ($ret)=$t->apply('count_a',$tree);
is($ret,3,'counter');
}

{
my $t=Transform->new([
    Rule->new('count_a',\&match_any,
              sub {
                  return scalar(grep {exists ${$_->attributes}{a}}
                                    $_->descendants_or_self);
              }),
]);

my ($ret)=$t->apply('count_a',$tree);
is($ret,3,'counter, different');
}

{
my $t=Transform->new([
    Rule->new('do',sub { $_->name() eq 'foo' and $_->parent() and $_->parent->name() eq 'bar'},
              sub {
                  return $_;
              }),
    Rule->new('do',\&match_any,
              sub {
                  return $_T->apply('do',$_->children);
              },0.5),
]);

my @ret=$t->apply('do',$tree);
is(scalar(@ret),2,'select');
}
