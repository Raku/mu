#!/usr/bin/perl
use strict;
use warnings;
use HTML::TreeBuilder;
use List::Util qw(first);
use vars qw($_T);
BEGIN {
    eval 'use Log::Log4perl qw(:easy)' if $ENV{DEBUG};
    eval 'use Log::Log4perl::Resurrector;' if $ENV{DEBUG};
}
use Data::Transform::Trivial ':brief';
eval 'Log::Log4perl->easy_init($DEBUG)' if $ENV{DEBUG};

# list the titles on /., then their comment count

my $t=Transform->new([
    Rule->new('top',
              sub {ref($_) and !$_->parent()}, # root
              sub {
                  my $ret=HTML::Element->new_from_lol(
                      ['html',
                       ['head',
                        ['title','Slashdot summary'],
                    ],
                       ['body',
                        ['ol'],
                    ],
                   ],
                  );
                  my $list=$ret->find_by_tag_name('ol');
                  my @items=$_T->apply('list-item',$_->content_list);
                  $list->push_content(@items);
                  return $ret;
              }),
    Rule->new('list-item', # just descend recursively
              sub{###l4p DEBUG "list-item @{[$_->tag,$_->all_attr]}" if ref($_);
                  1}, # default
              sub {
                  return unless ref($_);
                  return $_T->apply('list-item',$_->content_list);
              },0),
    Rule->new('list-item',
              sub {
                  # div[@class='article']
                  ref($_) and
                      $_->tag eq 'div' and
                          ($_->attr('class')||'') eq 'article'},
              sub {
                  my $ret=HTML::Element->new('li');
                  $ret->push_content(
                      $_T->apply('article-title',$_), # get the title
                      ' (',
                      $_T->apply('comment-count', # get the comment count
                                 # /.'s structure is weird
                                 # following-siblings::div[@class='storylinks '][1]
                                 (first {
                                     ref($_) and
                                         $_->tag eq 'div' and
                                             $_->attr('class') eq 'storylinks '
                                         } $_->right)),
                      ')',
                  );
                  return $ret;
              }),
    Rule->new('article-title', # just descend recursively
              sub{###l4p DEBUG "article-title @{[$_->tag,$_->all_attr]}" if ref($_);
                  1}, # default
              sub {
                  return unless ref($_);
                  return $_T->apply('article-title',$_->content_list);
              },0),
    Rule->new('article-title',
              sub {
                  # div[@class='title']
                  ref($_) and
                      $_->tag eq 'div' and
                          ($_->attr('class')||'') eq 'title' },
              sub {
                  return $_->as_trimmed_text;
              }),
    Rule->new('comment-count', # just descend recursively
              sub{###l4p DEBUG "comment-count @{[$_->tag,$_->all_attr]}" if ref($_);
###l4p DEBUG "$_" if !ref($_);
                  1}, # default
              sub {
                  return unless ref($_);
                  return $_T->apply('comment-count',$_->content_list);
              },0),
    Rule->new('comment-count',
              sub {
                  # li[@class='comments']/a[2]
                  ref($_) and
                      $_->tag eq 'a'
                          and (grep {ref($_) and $_->tag eq 'a'} $_->left)==1
                              and $_->parent->tag eq 'li'
                                  and ($_->parent->attr('class')||'') eq 'comments' },
              sub {
                  return $_->as_trimmed_text;
              }),
]);

my $tree=HTML::TreeBuilder->new_from_file('slashdot.html');
my ($output)=$t->apply('top',$tree);
print $output->as_HTML();
