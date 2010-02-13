use v5.10;
use MooseX::Declare;
class AST::Capture extends AST::Base {
    has 'invocant' => (is=>'ro');
    has 'positional' => (is=>'ro',default=>sub {[]},isa=>'ArrayRef[AST::Base]');
    has 'named' => (is=>'ro',default=>sub {[]});
}
