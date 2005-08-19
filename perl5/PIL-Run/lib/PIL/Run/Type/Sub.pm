
# This is a temporary throwaway class.
# The real one will use the metamodel.

package PIL::Run::Type::Sub;
@ISA=qw(PIL::Run::Type::Object);

sub new {
    my($class,$f)=@_;
    bless {raw_sub => $f}, $class;
}

sub apply {
    my($o,@args)=@_;
    $o->{'raw_sub'}(@args);
}

package PIL::Run::Type::Macro;
@ISA=qw(PIL::Run::Type::Sub);


1;
__END__
