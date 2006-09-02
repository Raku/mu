use Test;
plan 1;

# This test could use a peer review to confirm it complies with the spec. 
# Please update or remove as appropriate. 

{
    my $test = ' &::($meth)(self:) should find method in parent class';
    class Foo {
        method foo () { "found" }
    }
    class Child is Foo {
        method b () {
            my $meth = 'foo';
            &::($meth)(self:);
        }
    }
    my $obj = Child.new;
    eval_is( $obj.b, 'found', $test);
}

