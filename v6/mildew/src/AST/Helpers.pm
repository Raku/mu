package AST::Helpers;
use Exporter 'import';
our @EXPORT = qw(string reg integer call);
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
sub call {
    AST::Call->new(identifier=>string($_[0]),capture=>AST::Capture->new(invocant => $_[1],positional => $_[2]//[],named => $_[3]//[]));
}
