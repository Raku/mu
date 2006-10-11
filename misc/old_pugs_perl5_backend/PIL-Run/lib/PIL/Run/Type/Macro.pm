
# This is a temporary throwaway class.
# The real one will use the metamodel.

package PIL::Run::Type::Macro;
@ISA=qw(PIL::Run::Type::Object);

sub new {
    my($class,$f)=@_;
    bless {raw_sub => $f}, $class;
}

sub do {
    my($o,@args)=@_;
    $o->{'raw_sub'}(@args);
}

1;
__END__
