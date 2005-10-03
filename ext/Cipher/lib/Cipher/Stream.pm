use Cipher;
class Cipher::Stream is Cipher;

# Things they need to implement.
method generate_keystream(Int $n) returns Array {...}

# Things we implement for them.
method _cipher(Array $data) returns Array {
    return *$data >>+^<< *.generate_keystream($data.elems);
}
