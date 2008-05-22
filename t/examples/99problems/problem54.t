use v6;

# P54A (*) Check whether a given term represents a binary tree
# 
# Write a predicate istree which returns true if and only if its argument is a
# list representing a binary tree.
# 
# Example:
# * (istree (a (b nil nil) nil))
# T
# * (istree (a (b nil nil)))
# NIL

# We keep representing trees as lists
# but it could be interesting to use something like
#  subtype List::Tree of List where {istree($_)}
# or to define a proper class Node

sub istree($obj) returns Bool {
  return True if $obj === undef;
  return +$obj==3 and istree($obj[1]) and istree($obj[2]);
}

unless caller {
  use Test;
  plan 5;
  
  ok istree(undef), "We tell that an empty tree is a tree";
  ok istree(['a',undef,undef]), ".. and a one-level tree is a tree";
  ok istree(['a',undef,['c',undef,undef]]), ".. and n-level trees";
  ok !istree([]), ".. and fail with empty lists";
  ok !istree(<a b>),".. or other malformed trees";
}
