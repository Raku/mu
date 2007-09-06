class Meta {
    method PROTOTYPE {
        say "PROTOTYPE";
        my $dispatch = sub ($self,$method_name) {
            return "can't determine method name";
        };
        return {_dispatch=>$dispatch.p5landish}.p5landish;
    }
}
class Foo meta Meta {
}
my $foo = Foo.new();
say $foo;
