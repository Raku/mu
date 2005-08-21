=pod

This file is the beginning of a simple JavaScript-on-p6 runtime.
It seems likely there will be more than one.

Design goals:
 1 Correctness - mirror the spec.
 2 Interoperation - allow blending of p6 and js objects.
   (Even though this impairs 1?)
Please note efficiency isnt currently on the goal list.

CAVEAT - This file so far represents only a an hour or two of work.
It is very much in a state of flux.

=cut

class JSX::PseudoValue;
class JSX::InternalValue is JSX::PseudoValue;
class JSX::InternalValue::Reference  is JSX::InternalValue;
class JSX::InternalValue::List       is JSX::InternalValue;
class JSX::InternalValue::Completion is JSX::InternalValue;
class JSX::Value is JSX::PseudoValue;
class JSX::ValuePrimitive is JSX::Value;
class JSX::Value::Undefined is JSX::ValuePrimitive;
class JSX::Value::Null      is JSX::ValuePrimitive;
class JSX::Value::Boolean   is JSX::ValuePrimitive;
class JSX::Value::Number    is JSX::ValuePrimitive;
class JSX::Value::String    is JSX::ValuePrimitive;
class JSX::Value::Object is JSX::Value;
class JSX::Value::Array is JSX::Value::Object;

class JSX::PropertyAttributes {
    has $.ReadOnly;
    has $.DontEnum;
    has $.DontDelete;
}

class JSX::PsuedoValue;
has $.property_value;
has $.property_attributes;
# 8.6.2.1
method <[[Get]]> ($O: JSX::String $P) {
    my $result2 = $O.property_value{$P};
    return $result2 if defined $result2;
    my $proto = $O.<[[Prototype]]>;
    return JSX::undefined if $proto ~~ JSX::null;
    return $proto.<[[Get]]>($P);
}
# 8.6.2.2
method <[[Put]]> ($O: JSX::String $P, JSX::Value $V) {
    my $result1 = $O.<[[CanPut]]>($P);
    return if $result1 ~~ JSX::false;
    if $O.property_value.exists($P) {
	$O.property_value($P) = $V;
	return;
    }
    $O.property_value($P) = $V;
    return;
}    
# 8.6.2.3
method <[[CanPut]]> ($O: JSX::String $P) {
    if $O.property_value.exists($P) {
	my $attr = $O.property_attributes{$P};
	return JSX::false if $attr && $attr.ReadOnly;
	return JSX::true;
    }
    my $proto = $O.<[[Prototype]]>;
    return JSX::true if $proto ~~ JSX::null;
    return $proto.<[[CanPut]]>($P);
}
# 8.6.2.4
method <[[HasProperty]]> ($O: JSX::String $P) {
    return JSX::true if $O.property_value.exists($P);
    my $proto = $O.<[[Prototype]]>;
    return JSX::false if $proto ~~ JSX::null;
    return $proto.<[[CanPut]]>($P);
}
# 8.6.2.5
method <[[Delete]]> ($O: JSX::String $P) {
    return JSX::true if !$O.property_value.exists($P);
    my $attr = $O.property_attributes{$P};
    return JSX::false if $attr && $attr.ReadOnly;
    delete $O.property_value{$P};
    delete $O.property_attribute{$P};
    return JSX::true;
}
# 8.6.2.6
multi method <[[DefaultValue]]> ($O: JSX::String $hint) {
    my $result1 = $O.<[[Get]]>('toString');
    if $result1.isa(JSX::Value::Object) {
	my $result3 = JSX::with_this($O) { $result1.<[[Get]]>() };
	return $result3 if $result3.isa(JSX::ValuePrimitive);
    } # 5
    my $result5 = $O.<[[Get]]>('valueOf');
    if $result5.isa(JSX::Value::Object) {
	my $result7 = JSX::with_this($O) { $result1.<[[Get]]>() };
	return $result7 if $result7.isa(JSX::ValuePrimitive);
    } # 9
    raise TypeError;
}
multi method <[[DefaultValue]]> ($O: JSX::Number ?$hint) {
    my $result1 = $O.<[[Get]]>('valueOf');
    if $result1.isa(JSX::Value::Object) {
	my $result3 = JSX::with_this($O) { $result1.<[[Get]]>() };
	return $result3 if $result3.isa(JSX::ValuePrimitive);
    } # 5
    my $result5 = $O.<[[Get]]>('toString');
    if $result5.isa(JSX::Value::Object) {
	my $result7 = JSX::with_this($O) { $result1.<[[Get]]>() };
	return $result7 if $result7.isa(JSX::ValuePrimitive);
    } # 9
    raise TypeError;
}


class JSX::Value::Array;
submethod BUILD {
    $.property_attributes{'length'} =
	JSX::PropertyAttributes.new(:DontEnum :DontDelete);
}
method <[[Put]]> ($A: JSX::String $P, JSX::Value $V) { #see 15.4.5.1
    my $result1 = $A.<[[CanPut]]>($P);
    return if $result1 ~~ JSX::false;
    if $A.property_value.exists($P) {
	if $P eq 'length' { # 12-16
	    my $result12 = ToUint32($V);
	    raise RangeError if $result12 != ToNumber($V);
	    my $length = +($A.property_value{'length'});
	    my $k;
	    loop(;$k < $length; $k++) {
		my $key = $k; # Eh. Spec would have us use ToString(k).
		$A.<[[Delete]]>($k) if exists $A.property_value{$key};
		# XXX - need to wrap $k?
	    }
	    $A.property_value{$P} = $result12;
	    return;
	}
	# 5
	$A.property_value($P) = $V;
    } else { # 7
	$A.property_value($P) = $V;
    }
    # 8
    my sub not_an_array_index($P){ !($P ~~ rx:perl5/\A\d+\Z/); };
    return if not_an_array_index($P);
    my $uint = ToUint32($P);
    return if $uint < $A.property_value{'length'};
    $A.property_value{'length'} = $uint + 1; #XXX dont forget + wrappers
    return;
}    


# 9.1 ToPrimitive
multi sub ToPrimitive(JSX::Value::Object $o) {$o.<[[DefaultValue]]>}
multi sub ToPrimitive(JSX::Value $v) {$v}

