package AST::Helpers;
use Exporter 'import';
our @EXPORT = qw(string reg interger);
use AST;
sub string($) {
    AST::StringConstant->new(value=>$_[0]);
}
sub reg($) {
    AST::Reg->new(name=>$_[0]);
}
sub integer($) {
    AST::IntegerConstant->new(value=>$_[0]);
}

