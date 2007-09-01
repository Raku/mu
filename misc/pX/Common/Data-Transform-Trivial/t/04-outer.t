#!/usr/bin/perl
use strict;
use warnings;
use Test::More qw(no_plan);
use vars qw($_T $_P $_OUTER);
BEGIN {
    eval 'use Log::Log4perl qw(:easy)' if $ENV{DEBUG};
    eval 'use Log::Log4perl::Resurrector;' if $ENV{DEBUG};
}
use Data::Transform::Trivial ':brief';
eval 'Log::Log4perl->easy_init($DEBUG)' if $ENV{DEBUG};
use lib 't/lib'; # might not always work
use SimpleNode;

sub foo {
    return SimpleNode->new('foo',@_);
}

my $tree=foo({a=>1},[
    foo({a=>2},[
        foo({a=>3}),
        foo({a=>4}),
    ]),
    foo({a=>5},[
        foo({a=>6},[
            foo({a=>7}),
            foo({a=>8}),
        ]),
        foo({a=>9},[
            foo({a=>10}),
            foo({a=>11}),
        ]),
    ]),
]);

my $t=Transform->new(
    Rule->new('o',sub{1},
              sub {
                  my $ret='-';
                  if (defined $_OUTER) {
                      $ret=$_OUTER->attributes->{a};
                  }
                  if (@{$_->children}) {
                      return $ret.(join '',$_T->apply('o',$_->children));
                  }
                  else {
                      return $ret.'('.($_->attributes->{a}).')';
                  }
              }),
    Rule->new('o2',sub{1},
              sub {
                  my $ret='-';
                  if (defined $_OUTER) {
                      $ret=$_OUTER->attributes->{a};
                  }
                  my $me="($ret,".($_->attributes->{a}).')';
                  if (!defined $_->parent) {
                      return $me.(join '',$_T->apply('o2',$_->children));
                  }
                  my $p=$_->parent;
                  if ($#{$p->children} > $_->position) {
                      return $me.(join '',$_T->apply('o2',$p->children->[$_->position+1]));
                  }
                  else {
                      return $me;
                  }
              }),
);

{
my ($ret)=$t->apply('o',$tree);
is($ret,'-12(3)2(4)156(7)6(8)59(10)9(11)','outer');
}

{
my ($ret)=$t->apply('o2',$tree);
is($ret,'(-,1)(1,2)(2,5)(1,5)','outer, jumping');
}
