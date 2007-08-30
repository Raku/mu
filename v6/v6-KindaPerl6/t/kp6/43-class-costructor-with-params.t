class MyClass {
    has $.some_attribute;
    method get_attribute {
        return $.some_attribute;
    }
};
module Main {
    my $a = MyClass.new( some_attribute => 'ok 1' );
    say '1..1';
    say $a.get_attribute();
};