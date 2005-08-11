
# This is a temporary throwaway class.
# The real one will use the metamodel.

package PIL::Run::Type::Str;
@ISA=qw(PIL::Run::Type::Object);

sub new {
    my($class,$s)=@_;
    bless {raw_string => $s}, $class;
}

sub internal_stringify {
    my($o)=@_;
    $o->{'raw_string'}
}

1;
__END__
