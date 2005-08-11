
# This is a temporary throwaway class.
# The real one will use the metamodel.

package PIL::Run::Type::Num;
@ISA=qw(PIL::Run::Type::Object);

sub new {
    my($class,$n)=@_;
    bless {raw_num => $n+0}, $class;
}

sub internal_numify {
    my($o)=@_;
    $o->{'raw_num'};
}

sub internal_stringify {
    my($o)=@_;
    "".$o->{'raw_num'};
}

package PIL::Run::Type::Int;
@ISA=qw(PIL::Run::Type::Num);

package PIL::Run::Type::Rat;
@ISA=qw(PIL::Run::Type::Num);

1;
__END__
