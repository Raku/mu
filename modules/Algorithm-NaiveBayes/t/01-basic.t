#!/usr/bin/perl6

use Test;
BEGIN { plan 13 }
use Algorithm::NaiveBayes;

ok 1; # If we made it this far, we're loaded.

my Algorithm::NaiveBayes $nb .= new(:purge(0));
ok $nb ~~ Algorithm::NaiveBayes;

# Populate
add_instance $nb: attributes => (:sheep :very :valuable :farming),
                  label      => <farming>;
is +$nb.labels, 1;

add_instance $nb: attributes => (:farming :requires :many :kinds :animals),
                  label      => <farming>;
is +$nb.labels, 1;

add_instance $nb: attributes => (:vampires :drink :blood :vampires :may :staked),
                  label      => <vampire>;
is +$nb.labels, 2;

add_instance $nb: attributes => (:vampires :cannot :see :their :images :mirrors),
                  label      => <vampire>;
is +$nb.labels, 2;

# Train
$nb.train();

ok $nb.purge;

# Predict
my $h = predict $nb: attributes => (:i :would :like :to :begin :farming :sheep);
ok $h;
ok $h.<farming> > 0.5;
ok $h.<vampire> < 0.5;

# XXX - C<'> allowed in C<:...>?
$h = predict $nb: attributes => (:i :see :that :many :vampires :may :have :eaten :my :beautiful :daughter's :blood); #'#--vim
ok $h;
ok $h.<farming> < 0.5;
ok $h.<vampire> > 0.5;
