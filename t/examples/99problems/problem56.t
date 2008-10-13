use v6;

# P56 (**) Symmetric binary trees
# 
# Let us call a binary tree symmetric if you can draw a vertical line through the
# root node and then the right subtree is the mirror image of the left subtree.
# Write a predicate symmetric/1 to check whether a given binary tree is
# symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is
# the mirror image of another. We are only interested in the structure, not in
# the contents of the nodes.

sub symmetric($tree) {
  mirror(left($tree),right($tree))
}

 
# We use multi subs so that in theory we can replace this definitions 
# for example using classes or Array subtyping instead of lispish trees

multi sub mirror(Array @first, Array @second) {
  if (@first|@second == (undef,)) {
     return @first == @second ;
   }
   mirror(left(@first),right(@second)) and mirror(right(@first),left(@second))
}



multi sub left(Array @tree) {
  @tree[1]
}
multi sub right(Array @tree) {
  @tree[2]
}

unless caller() {
  use Test;
  plan 9;

  is left(['a',1,2]), 1, "left()  works";
  is right(['b',1,2]), 2, "right() works";

  ok mirror(undef,undef),"mirror works with empty trees";
  ok !mirror(undef,[]),"mirror spots differences";
  ok mirror([1,undef,undef],[2,undef,undef]),"mirror can recurse";
  ok !mirror([1,undef,[]],[2,undef,undef]),"mirror spots differences recurring";
  
  ok symmetric([1,undef,undef]), "symmetric works with 1-level trees";
  ok !symmetric([1,undef,[2,undef,undef]]),"symmetric find asymettric trees";
  ok symmetric([1,
                 [11,
                   [111,undef,undef],
                   [112,[1121,undef,undef],undef]],
                 [12,
                   [121,undef,[1221,undef,undef]],
                   [122,undef,undef]]]),
                "symmetric works with n-level trees"; 
}
