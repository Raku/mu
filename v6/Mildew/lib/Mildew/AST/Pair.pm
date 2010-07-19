use v5.10;
use MooseX::Declare;
class Mildew::AST::Pair extends Mildew::AST::Base {
    has 'key' => (is=>'ro');
    has 'value' => (is=>'ro');
    method m0ld {
        die('Pairs are here just to be seen as named arguments, for now.');
    }
    method pretty {
        return ':'.$self->key->pretty.'('.$self->value->pretty.')';
    }
}
