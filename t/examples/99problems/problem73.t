use v6-alpha;
use Test;
plan 1;

# P73 (**) Lisp-like tree representation
# 
# There is a particular notation for multiway trees in Lisp. Lisp is a prominent
# functional programming language, which is used primarily for artificial
# intelligence problems. As such it is one of the main competitors of Prolog. In
# Lisp almost everything is a list, just as in Prolog everything is a term.
# 
# The following pictures show how multiway tree structures are represented in Lisp.
# 
# Note that in the "lispy" notation a node with successors (children) in the
# tree is always the first element in a list, followed by its children. The
# "lispy" representation of a multiway tree is a sequence of atoms and
# parentheses '(' and ')', which we shall collectively call "tokens". We can
# represent this sequence of tokens as a Prolog list; e.g. the lispy expression
# (a (b c)) could be represented as the Prolog list ['(', a, '(', b, c, ')',
# ')']. Write a predicate tree-ltl(T,LTL) which constructs the "lispy token
# list" LTL if the tree is given as term T in the usual Prolog notation.
# 
# Example:
# * tree-ltl(t(a,[t(b,[]),t(c,[])]),LTL).
# LTL = ['(', a, '(', b, c, ')', ')']
# 
# As a second, even more interesting exercise try to rewrite tree-ltl/2 in a way
# that the inverse conversion is also possible: Given the list LTL, construct
# the Prolog tree T. Use difference lists.
# 
# Graphs
# 
# A graph is defined as a set of nodes and a set of edges, where each edge is a
# pair of nodes.
# 
# There are several ways to represent graphs in Prolog. One method is to
# represent each edge separately as one clause (fact). In this form, the graph
# depicted below is represented as the following predicate:
# 
# edge(h,g).
# edge(k,f).
# edge(f,b).
# ...
# 
# We call this edge-clause form. Obviously, isolated nodes cannot be
# represented. Another method is to represent the whole graph as one data
# object. According to the definition of the graph as a pair of two sets (nodes
# and edges), we may use the following Prolog term to represent the example
# graph:
# 
# graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])
# 
# We call this graph-term form. Note, that the lists are kept sorted, they are
# really sets, without duplicated elements. Each edge appears only once in the
# edge list; i.e. an edge from a node x to another node y is represented as
# e(x,y), the term e(y,x) is not present. The graph-term form is our default
# representation. In SWI-Prolog there are predefined predicates to work with
# sets.
# 
# A third representation method is to associate with each node the set of nodes
# that are adjacent to that node. We call this the adjacency-list form. In our
# example:
# 
# [n(b,[c,f]), n(c,[b,f]), n(d,[]), n(f,[b,c,k]), ...]
# 
# The representations we introduced so far are Prolog terms and therefore well
# suited for automated processing, but their syntax is not very user-friendly.
# Typing the terms by hand is cumbersome and error-prone. We can define a more
# compact and "human-friendly" notation as follows: A graph is represented by a
# list of atoms and terms of the type X-Y (i.e. functor '-' and arity 2). The
# atoms stand for isolated nodes, the X-Y terms describe edges. If an X appears
# as an endpoint of an edge, it is automatically defined as a node. Our example
# could be written as:
# 
# [b-c, f-c, g-h, d, f-b, k-f, h-g]
# 
# We call this the human-friendly form. As the example shows, the list does not
# have to be sorted and may even contain the same edge multiple times. Notice
# the isolated node d. (Actually, isolated nodes do not even have to be atoms in
# the Prolog sense, they can be compound terms, as in d(3.75,blue) instead of d
# in the example).
# 
# 
# When the edges are directed we call them arcs. These are represented by
# ordered pairs. Such a graph is called directed graph. To represent a directed
# graph, the forms discussed above are slightly modified. The example graph
# opposite is represented as follows:
# 
# Arc-clause form
# arc(s,u).
# arc(u,r).
# ...
# 
# Graph-term form
# digraph([r,s,t,u,v],[a(s,r),a(s,u),a(u,r),a(u,s),a(v,u)])
# 
# Adjacency-list form
# [n(r,[]),n(s,[r,u]),n(t,[]),n(u,[r]),n(v,[u])]
# 
# Note that the adjacency-list does not have the information on whether it is a
# graph or a digraph.
# 
# Human-friendly form
# [s > r, t, u > r, s > u, u > s, v > u] 
# 
# 
# Finally, graphs and digraphs may have additional information attached to nodes
# and edges (arcs). For the nodes, this is no problem, as we can easily replace
# the single character identifiers with arbitrary compound terms, such as
# city('London',4711). On the other hand, for edges we have to extend our
# notation. Graphs with additional information attached to edges are called
# labelled graphs.
# 
# Arc-clause form
# arc(m,q,7).
# arc(p,q,9).
# arc(p,m,5).
# 
# Graph-term form
# digraph([k,m,p,q],[a(m,p,7),a(p,m,5),a(p,q,9)])
# 
# Adjacency-list form
# [n(k,[]),n(m,[q/7]),n(p,[m/5,q/9]),n(q,[])]
# 
# Notice how the edge information has been packed into a term with functor '/'
# and arity 2, together with the corresponding node.
# 
# Human-friendly form
# [p>q/9, m>q/7, k, p>m/5]
# 
# 
# The notation for labelled graphs can also be used for so-called multi-graphs,
# where more than one edge (or arc) are allowed between two given nodes.

if 1 {
    skip 1, "Test(s) not yet written: (**) Lisp-like tree representation";
}
else {
    ok 1, '(**) Lisp-like tree representation';
}
