
# NOTE:
# this tree was taken from src/Pugs/Context.hs (~ line 89)

use v6;

class Perl::TypeTree-0.0.1;

use Tree;

has Tree $:type_tree;

method tree ($self:) returns Tree {
    # create the tree lazily ...
    $self.:create_tree() unless $:tree.defined;
    return $:tree;
}

method show_tree ($self:) returns Void {
    $self.tree().traverse(-> $t { 
        say(('  ' x $t.depth()) ~ " " ~ $t.node()) 
    });
}

my method create_tree returns Void {
    $:type_tree = Tree.new().add_children(
        Tree.new(:node<Any>).add_children(
            Tree.new(:node<Void>).add_children(
                Tree.new(:node<Object>).add_children(
                    Tree.new(:node<List>).add_children(
                        Tree.new(:node<Lazy>).add_children(
                            Tree.new(:node<Array>).add_children(
                                Tree.new(:node<Array::Const>),
                                Tree.new(:node<Array::Slice>),
                            ),
                            Tree.new(:node<Hash>).add_children(
                                Tree.new(:node<Hash::Const>),
                                Tree.new(:node<Hash::Env>),
                            ),
                        ),
                        Tree.new(:node<Eager>),
                    ),
                    Tree.new(:node<Pair>),
                    Tree.new(:node<Scalar>).add_children(
                        Tree.new(:node<Complex>).add_children(
                            Tree.new(:node<Num>).add_children(
                                Tree.new(:node<Rat>).add_children(
                                    Tree.new(:node<Int>).add_children(
                                        Tree.new(:node<Bit>)
                                    ),
                                ),
                            ),
                        ),                        
                        Tree.new(:node<Bool>),
                        Tree.new(:node<Str>),
                        Tree.new(:node<Ref>),
                        Tree.new(:node<IO>),
                        Tree.new(:node<Socket>),
                        Tree.new(:node<Thread>),
                        Tree.new(:node<Code>).add_children(
                            Tree.new(:node<Routine>).add_children(
                                Tree.new(:node<Sub>).add_children(
                                    Tree.new(:node<Method>),
                                    Tree.new(:node<Submethod>),
                                ),
                                Tree.new(:node<Macro>)
                            ),
                            Tree.new(:node<Block>).add_children(
                                Tree.new(:node<Bare>).add_children(
                                    Tree.new(:node<Parametric>)
                                ),
                            ),
                        ),
                        Tree.new(:node<Rule>),
                        Tree.new(:node<Match>),
                        Tree.new(:node<Junction>),
                        Tree.new(:node<Scalar::Const>),
                        Tree.new(:node<Scalar::Proxy>),
                        Tree.new(:node<Scalar::Lazy>),
                    ),
                ),
            ),
            Tree.new(:node<Grammar>),
            Tree.new(:node<Type>).add_children(
                Tree.new(:node<Package>).add_children(
                    Tree.new(:node<Module>).add_children(
                        Tree.new(:node<Class>)
                    ),
                ),
                Tree.new(:node<Trait>).add_children(
                    Tree.new(:node<PkgTrait>)
                ),
            )
        )
    );
}
