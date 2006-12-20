use v6-alpha;
use Test;
plan 1;

# P50 (***) Huffman code.
# 
# First of all, consult a good book on discrete mathematics or algorithms for a
# detailed description of Huffman codes!
# 
# We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. 
# Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. 
# 
# Our objective is to construct a list hc(S,C) terms, where C is the Huffman code
# word for the symbol S. In our example, the result could be Hs = [hc(a,'0'),
# hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')]
# [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2
# defined as follows:
# 
# % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
# 
# Binary Trees
# 
# A binary tree is either empty or it is composed of a root element and two
# successors, which are binary trees themselves.  In Lisp we represent the empty
# tree by 'nil' and the non-empty tree by the list (X L R), where X denotes the
# root node and L and R denote the left and right subtree, respectively. The
# example tree depicted opposite is therefore represented by the following list:
# 
# (a (b (d nil nil) (e nil nil)) (c nil (f (g nil nil) nil)))
# 
# Other examples are a binary tree that consists of a root node only:
# 
# (a nil nil) or an empty binary tree: nil.
# 
# You can check your predicates using these example trees. They are given as test
# cases in p54.lisp.

if 1 {
    skip 1, "Test(s) not yet written: (***) Huffman code.";
}
else {
    ok 1, '(***) Huffman code.';
}
