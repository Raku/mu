use v5.10;
use MooseX::Declare;
use utf8;
{
package AST;
sub unique_id {
    state $id = 0;
    '$id'.$id++;
}
sub unique_reg {
    AST::Reg->new(name=>unique_id);
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

use AST::Base;
use AST::Call;
use AST::If;
use AST::Let;
use AST::Assign;
use AST::Helpers;
use AST::Comment;
use AST::While;
use AST::Pair;
use AST::IntegerConstant;
use AST::StringConstant;
use AST::Branch;
use AST::Reg;
use AST::Capture;
use AST::Goto;
use AST::Block;
use AST::Block::SSA;
use AST::Seq;
use AST::Loop;
use AST::InferredTypeTest;
use AST::Phi;
use AST::Block::Simplified;
1;
