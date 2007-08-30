#!/usr/bin/perl
use strict;
use warnings;
use Test::More qw(no_plan);
use Transform;
use Rule;
use SimpleNode;
use Data::Dumper;
use List::Util qw(sum);

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

my $t=Transform->new([
    Rule->new('fl',\&match_any,
              sub {
                  warn "buh\n";
                  warn Dumper($_);
                  return 
                      substr($_->name(),0,1)
                      .(join '',$main::_T->apply('fl',$_->children));
              }),
]);

my ($ret)=$t->apply('fl',$tree);
is($ret,'fbffbb','simple first-letter');

sub match_with_a {
    return exists ${$_->attributes}{a};
}

my $t=Transform->new([
    Rule->new('count_a',\&match_with_a,
              sub {
                  return 1+(sum($main::_T->apply('count_a',$_->children))||0);
              }),
    Rule->new('count_a',\&match_any,
              sub {
                  return sum($main::_T->apply('count_a',$_->children))||0;
              },0.5),
]);

($ret)=$t->apply('count_a',$tree);
is($ret,3,'counter');

my $t=Transform->new([
    Rule->new('count_a',\&match_any,
              sub {
                  return scalar($main::_T->find
                                    ($main::_C,
                                     sub {
                                         return grep {exists ${$_->attributes}{a}}
                                             $_->descendants_or_self;
                                     }));
              }),
]);

($ret)=$t->apply('count_a',$tree);
is($ret,3,'counter, different');

my $t=Transform->new([
    Rule->new('do',sub { $_->name() eq 'foo' and $_->parent() and $_->parent->name() eq 'bar'},
              sub {
                  return $_;
              }),
    Rule->new('do',\&match_any,
              sub {
                  return $main::_T->apply('do',$_->children);
              },0.5),
]);

my @ret=$t->apply('do',$tree);
is(scalar(@ret),2,'select');

__END__

sub { $_->type eq 'Sequence' and scalar(grep{$_->type eq 'Assign'} $_->children)==0 }
}

# NO type::Sequence[count(children::*[type::Assign])=0]

type='Sequence' and count(children::*[type='Assign'])=0
