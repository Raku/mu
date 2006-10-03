use Test; plan 1;

# L<S12/"Attributes">
# ( The spec doesn't explicitly method that this should
#   work, but audreyt and markstos agree it should. 
#   Perhaps the spec can be clarified on this point.)

{
    class Foo {
        has @.a is rw;
        method param (*%h) {
            @.a = @( %h<key> );
            self;
        }
    }
    # The workaround until this is fixed:
    # my @f = @( Foo.new.param( key => <c d>).a );
    my @f = Foo.new.param( key => <c d> ).a;
    is_deeply(@f, <c d>, "@.a attribute is returned as array");
}

