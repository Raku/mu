use v5.10;
use Forest::Tree;
use MooseX::Declare;
class AST::Base {
    use YAML::XS;
    method pretty {
        Dump($self);
    }
    method forest {
        Forest::Tree->new(node=>$self->pretty,children=>[]);
    }
}
