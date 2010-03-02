use v5.10;
use Forest::Tree;
use MooseX::Declare;
class AST::Base {
    use YAML::XS;
    has id=>(is=>'ro',lazy_build=>1);
    method pretty {
        Dump($self);
    }
    method forest {
        Forest::Tree->new(node=>$self->pretty,children=>[]);
    }
    my $UID;
    method _build_id {
        $UID++;
    }
    method took {
        0;
    }
}
