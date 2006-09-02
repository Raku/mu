use Test;
plan 2;

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

    {
        my $test = q"$obj.$meth is canonical (audreyt says)";
        my $meth = 'foo';
        eval_is( $obj.$meth(), 'found', $test);
    }
}

