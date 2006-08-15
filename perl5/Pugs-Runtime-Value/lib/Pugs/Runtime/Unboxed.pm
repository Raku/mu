
# file Pugs::Runtime::Unboxed

# ---------- Implementation of unboxed values --------------

#package Pugs::Runtime::Value;
#
#sub to_ref {
#    my $object = shift;
#    my $class_name = ::dispatch( ::meta( $object ), 'name' );  # ** get the class name, from object
#    my $class = $Pugs::Runtime::Class::ALL_CLASSES{$class_name};  # ** get the class, from class name
#    return $class;
#}

sub Pugs::Runtime::Value::p6v_isa {
    my($o,$cls)=@_;
    my $ref = ref($o);
    return 1 if $ref eq 'Dispatchable' && $o->isa($cls);
    return 0;
}

$Pugs::Runtime::Value::obj_id = '**ObJecT**' . rand;
sub Pugs::Runtime::Value::identify {
    my $key = shift;
    return $Pugs::Runtime::Value::obj_id unless defined $key;
    my $s = $key;
    if ( ref($key) && (
        $key->isa( 'Int' ) ||
        $key->isa( 'Num' ) ||
        $key->isa( 'Str' ) ||
        $key->isa( 'Bit' ) ||
        $key->isa( 'Rat' ) )
    ) {
        $s = $key->str->unboxed
    }
    elsif ( UNIVERSAL::can( $key, 'id' ) ) {
        $s = $Pugs::Runtime::Value::obj_id . $key->id
    }
    return $s;
}

sub Pugs::Runtime::Value::stringify {
    my $s = shift;
    $s = $s->fetch if Pugs::Runtime::Value::p6v_isa( $s, 'Scalar');
    my $tmp;
    # warn "stringify - $s\n";
    eval { $tmp = $s->str(max=>3) };
    $s = $tmp unless $@;
    # warn "   str - $s - $@\n";
    eval { $tmp = $s->unboxed };
    $s = $tmp unless $@;
    # warn "   unboxed - $s - $@\n";
    return 'undef' unless defined $s;
    no warnings 'numeric';
    $s = Pugs::Runtime::Value::Num::to_str( $s ) if $s+0 eq $s;
    return $s;
}

sub Pugs::Runtime::Value::numify {
    my $s = shift;
    $s = $s->fetch if Pugs::Runtime::Value::p6v_isa( $s, 'Scalar');
    my $tmp;
    eval { $tmp = $s->num };
    $s = $tmp unless $@;
    eval { $tmp = $s->unboxed };
    $s = $tmp unless $@;
    unless ( defined $s ) {
        warn "attempting to use 'undef' as a number";
        return 0;
    }
    no warnings 'numeric';
    $s = Pugs::Runtime::Value::Str::to_num( $s ) if $s+0 ne $s;
    return $s;
}

package Pugs::Runtime::Value::Num;

use constant Inf => 100**100**100;
use constant NaN => Inf / Inf;

sub to_str        { 
    my $v = shift;
    # native
    return 'Inf'  if $v eq "".&Pugs::Runtime::Value::Num::Inf;
    return '-Inf' if $v eq "".(-&Pugs::Runtime::Value::Num::Inf);
    return 'NaN'  if $v eq "".&Pugs::Runtime::Value::Num::NaN;
    $v = 0 + $v;
    return 'Inf'  if $v == Inf;
    return '-Inf' if $v == -&Inf;
    return 'NaN'  if $v =~ m/n/i;
    return "" . $v 
}
sub to_bit        { $_[0] == 0 ? '' : 1 }
sub to_num        { 0 + $_[0] }
sub to_int        { int( $_[0] ) }

package Pugs::Runtime::Value::Int;

sub to_str        { 
    no warnings 'uninitialized';
    my $v = 0 + $_[0];
    return 'Inf'  if $v == Pugs::Runtime::Value::Num::Inf;
    return '-Inf' if $v == -&Pugs::Runtime::Value::Num::Inf;
    return 'NaN'  if $v =~ m/n/i;
    return "" . $v 
}
sub to_bit        { $_[0] == 0 ? '' : 1 }
sub to_num        { 0 + $_[0] }
sub to_int        { $_[0] }

package Pugs::Runtime::Value::Str;

sub to_str        { $_[0] }
sub to_bit        { 
    return '' if $_[0] eq '0' || $_[0] eq '';
    return 1;
}
sub to_num        {
    my $v = $_[0];
    $v =~ s/\s+//g;
    # Perl 6
    return Pugs::Runtime::Value::Num::Inf   if $v eq 'Inf';
    return -&Pugs::Runtime::Value::Num::Inf if $v eq '-Inf';
    return Pugs::Runtime::Value::Num::NaN   if $v eq 'NaN';
    # native
    return Pugs::Runtime::Value::Num::Inf   if $v eq "".&Pugs::Runtime::Value::Num::Inf;
    return -&Pugs::Runtime::Value::Num::Inf if $v eq "".(-&Pugs::Runtime::Value::Num::Inf);
    return Pugs::Runtime::Value::Num::NaN   if $v eq "".&Pugs::Runtime::Value::Num::NaN;

    no warnings 'numeric';
    return 0 + $v;
}
sub to_int        { Pugs::Runtime::Value::Num::to_int( to_num( $_[0] ) ) }

sub decrement {
    return '' if $_[0] eq '' || $_[0] eq 'a' || $_[0] eq '0';
    my ($s,$c) = $_[0] =~ /(.*)(.)/;
    return str_decr($s) . 'z' if $c eq 'a';
    return str_decr($s) . '9' if $c eq '0';
    return $s . chr( ord($c) - 1 );
}

package Pugs::Runtime::Value::Bit;

# built-in, unboxed 'bit' enums
sub bool::false      { '' }
sub bool::true       { 1 }
sub taint::untainted { '' }
sub taint::tainted   { 1 }

sub to_str        { $_[0] ? 1 : '' }
sub to_bit        { $_[0] }
sub to_num        { $_[0] == 0 ? 0 : 1 }
sub to_int        { to_num( $_[0] ) }

1;
