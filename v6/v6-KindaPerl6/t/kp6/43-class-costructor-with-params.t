class MyClass {
    has $.some_attribute;
    method get_attribute {
        return $.some_attribute;
    }
};
module Main {
    my $a;
    #$a  = MyClass.new( some_attribute => 'ok 1' );
    say '1..1';
    #say $a.get_attribute();
    say 'not ok 1 # TODO - grammar still not parsing new with arguments';
};