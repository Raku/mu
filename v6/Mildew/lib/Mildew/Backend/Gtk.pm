use v5.10;
use MooseX::Declare;
use Mildew::SSA;
use Mildew::Types;
class Mildew::Backend::Gtk with Mildew::Backend {
    has format=>(is=>'ro');
    method compile($ast,$output) {
        local $Mildew::no_setr = 1;
        require Gtk2;
        Gtk2->init;
        require Forest::Tree::Viewer::Gtk2;
        my $tree = Mildew::SSA::to_ssa($ast->simplified,{
            '$scope' => Mildew::Type::Scope->new(outer=> $Mildew::LexicalPreludeType)
        })->forest;
        my $tree_view = Forest::Tree::Viewer::Gtk2->new(tree=>$tree)->view;

        my $window = Gtk2::Window->new('toplevel');
        $window->add($tree_view);
        $window->show_all;
        Gtk2->main;
    }
}
__END__
=pod 

=head1 NAME

Mildew::Backend::Gtk

=head1 DESCRIPTION

This backend displays the Mildew::AST using Forest::Gtk2

=cut
