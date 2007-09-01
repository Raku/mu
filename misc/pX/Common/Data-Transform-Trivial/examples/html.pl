#!/usr/bin/perl
use strict;
use warnings;
use Data::Transform::Trivial;
use Data::Transform::Trivial::Rule;
use HTML::TreeBuilder;
use List::Util qw(first);

BEGIN{
*Transform::=\*Data::Transform::Trivial::;
*Rule::=\*Data::Transform::Trivial::Rule::;
}

###l4p Log::Log4perl->easy_init($DEBUG);

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
                  my @items=$main::_T->apply('list-item',$_->content_list);
                  $list->push_content(@items);
                  return $ret;
              }),
    Rule->new('list-item',
              sub{###l4p DEBUG "list-item @{[$_->tag,$_->all_attr]}" if ref($_);
                  1}, # default
              sub {
                  return unless ref($_);
                  return $main::_T->apply('list-item',$_->content_list);
              },0),
    Rule->new('list-item',
              sub {ref($_) and $_->tag eq 'div' and ($_->attr('class')||'') eq 'article'},
              sub {
                  my $ret=HTML::Element->new('li');
                  $ret->push_content(
                      $main::_T->apply('article-title',$_),
                      ' (',
                      $main::_T->apply('comment-count',
                                       (first {ref($_) and $_->tag eq 'div' and $_->attr('class') eq 'storylinks '} $_->right)),
                      ')',
                  );
                  return $ret;
              }),
    Rule->new('article-title',
              sub{###l4p DEBUG "article-title @{[$_->tag,$_->all_attr]}" if ref($_);
                  1}, # default
              sub {
                  return unless ref($_);
                  return $main::_T->apply('article-title',$_->content_list);
              },0),
    Rule->new('article-title',
              sub { ref($_) and $_->tag eq 'div' and ($_->attr('class')||'') eq 'title' },
              sub {
                  return $_->as_trimmed_text;
              }),
    Rule->new('comment-count',
              sub{###l4p DEBUG "comment-count @{[$_->tag,$_->all_attr]}" if ref($_);
###l4p DEBUG "$_" if !ref($_);
                  1}, # default
              sub {
                  return unless ref($_);
                  return $main::_T->apply('comment-count',$_->content_list);
              },0),
    Rule->new('comment-count',
              sub { ref($_) and $_->tag eq 'a'
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
