use v5.10;
use MooseX::Declare;
class VAST::circumfix__S_Paren_Thesis {
    use AST::Helpers;
    method emit_m0ld {
	$self->{semilist}{statement}[0]->emit_m0ld;
    }
}
