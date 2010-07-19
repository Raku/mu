use v5.10;
use MooseX::Declare;
use utf8;
{
package Mildew::AST;
sub unique_id {
    state $id = 0;
    '$id'.$id++;
}
sub unique_reg {
    Mildew::AST::Reg->new(name=>unique_id);
}
sub unique_label {
    state $lab = 0;
    'lab'.$lab++;
}
sub indent {
    my $x = shift;
    my $i = shift || 1;
    my $s = '    ' x $i;
    $x =~ s/^/$s/mg;
    $x;
}
sub terminate_stmt {
    my $stmt = shift;
    if ($stmt =~ /;|}$/) {
        $stmt . "\n";
    } elsif ($stmt =~ /\n$/) {
        $stmt;
    } else {
        $stmt . ";\n";
    }
}
}

use Mildew::AST::Base;
use Mildew::AST::Call;
use Mildew::AST::If;
use Mildew::AST::Let;
use Mildew::AST::Assign;
use Mildew::AST::Helpers;
use Mildew::AST::Comment;
use Mildew::AST::While;
use Mildew::AST::Pair;
use Mildew::AST::IntegerConstant;
use Mildew::AST::StringConstant;
use Mildew::AST::Branch;
use Mildew::AST::Reg;
use Mildew::AST::Capture;
use Mildew::AST::Goto;
use Mildew::AST::Block;
use Mildew::AST::Block::SSA;
use Mildew::AST::Seq;
use Mildew::AST::Loop;
use Mildew::AST::InferredTypeTest;
use Mildew::AST::Phi;
use Mildew::AST::Block::Simplified;
1;
