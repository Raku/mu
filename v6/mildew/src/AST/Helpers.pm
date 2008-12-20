package AST::Helpers;
use Exporter 'import';
our @EXPORT = qw(string reg integer call FETCH lookup capturize let);
use AST;
use strict;
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
sub FETCH {
    my $arg = shift;
    call FETCH => $arg
}
sub lookup {
    my $thing = shift;
    call lookup => reg '$scope',[string $thing];
}
sub capturize {
    my ($pos,$named) = @_;
    AST::Call->new(
        identifier => string "capturize",
        capture => AST::Capture->new(
            invocant => reg '?SMOP__S1P__Capturize',
            positional => $pos // [],
            named => $named // []
        )
    )
}
sub let {
    my ($value,$block) = @_;
    AST::Let->new(value=>$value,block=>$block);
}
