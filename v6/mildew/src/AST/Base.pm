use v5.10;
use MooseX::Declare;
class AST::Base {
    use YAML::XS;
    method pretty {
        Dump($self);
    }
}
