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

class JSX::Value;
has $.property_value;
has $.property_attributes;
# 8.6.2.1
method <[[Get]]> ($O: JSX::String $P) {
    my $Result2 = $O.property_value{$P};
    return $Result2 if defined $Result2;
    my $proto = $O.<[[Prototype]]>;
    return JSX::undefined if $proto ~~ JSX::null;
    return $proto.<[[Get]]>($P);
}
# 8.6.2.2
method <[[Put]]> ($O: JSX::String $P, JSX::Value $V) {
    my $Result1 = $O.<[[CanPut]]>($P);
    return if $Result1 ~~ JSX::false;
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
multi method <[[DefaultValue]]> ($O: ?$hint = 'Number') {
    if $hint eq 'String' {
        my $Result1 = $O.<[[Get]]>('toString');
        if $Result1.isa(JSX::Value::Object) {
            my $Result3 = JSX::with_this($O) { $Result1.<[[Get]]>() };
            return $Result3 if $Result3.isa(JSX::ValuePrimitive);
        } # 5
        my $Result5 = $O.<[[Get]]>('valueOf');
        if $Result5.isa(JSX::Value::Object) {
            my $Result7 = JSX::with_this($O) { $Result1.<[[Get]]>() };
            return $Result7 if $Result7.isa(JSX::ValuePrimitive);
        } # 9
        raise TypeError;
    } elsif $hint eq 'Number' {
        my $Result1 = $O.<[[Get]]>('valueOf');
        if $Result1.isa(JSX::Value::Object) {
            my $Result3 = JSX::with_this($O) { $Result1.<[[Get]]>() };
            return $Result3 if $Result3.isa(JSX::ValuePrimitive);
        } # 5
        my $Result5 = $O.<[[Get]]>('toString');
        if $Result5.isa(JSX::Value::Object) {
            my $Result7 = JSX::with_this($O) { $Result1.<[[Get]]>() };
            return $Result7 if $Result7.isa(JSX::ValuePrimitive);
        } # 9
        raise TypeError;
    } else {
        die "bug - invalid hint $hint";
    }
}

# 8.7 The Reference Type
class JSX::InternalValue::Reference {
    has $.base_object;
    has $.property_name;
    method GetBase ($O:) { $.base_object }
    method GetPropertyName ($O:) { $.property_name }
}
# 8.7.1
method GetValue (JSX::PsuedoValue $V) { $V }
method GetValue (JSX::InternalValue::Reference $V) {
    my $Result2 = GetBase($V);
    raise ReferenceError if $Result2 ~~ JSX::null;
    my $Result4 = $Result2.<[[Get]]>(GetPropertyName($V));
    return $Result4;
}
# 8.7.2
method PutValue (JSX::PsuedoValue $V) { raise ReferenceError; }
method PutValue (JSX::InternalValue::Reference $V, $W) {
    my $Result2 = GetBase($V);
    if !($Result2 ~~ JSX::null) {
        $Result2.<[[Put]]>(GetPropertyName($V),$W);
        return;
    } # 6
    JSX::global_object.<[[Put]]>(GetPropertyName($V),$W);
    return;
}

# 8.8 The List Type
class JSX::InternalValue::List is Array;

# 8.9 The Completion Type
class JSX::InternalValue::Completion_type { # XXX - should be enum
   has $.normal;
   has $.break;
   has $.continue;
   has $.return;
   has $.throw;
}
class JSX::InternalValue::Completion {
   submethod BUILD { $.type = JSX::InternalValue::Completion_type.new(); }
   has $.type;
   has $.value;
   has $.target;
   method is_abrupt_completion() { !$.type.normal }
}
# empty is represented by undef.

# 9. Type Conversion

# 9.1
multi sub ToPrimitive(JSX::Value::Object $o) {$o.<[[DefaultValue]]>}
multi sub ToPrimitive(JSX::Value $v) {$v}

# 9.2
multi sub ToBoolean(JSX::Value::Undefined $v) { JSX::false }
multi sub ToBoolean(JSX::Value::Null $v)      { JSX::false }
multi sub ToBoolean(JSX::Value::Boolean $v)   { $v }
multi sub ToBoolean(JSX::Value::Number $v) {
    +$v == (+0|-0|NaN) ?? JSX::false :: JSX::true;
}
multi sub ToBoolean(JSX::Value::String $v) {
    ~$v eq "" ?? JSX::false :: JSX::true;
}
multi sub ToBoolean(JSX::Value::Object $v) { JSX::true }

# 9.3
multi sub ToNumber(JSX::Value::Undefined $v) { NaN }
multi sub ToNumber(JSX::Value::Null $v)      { +0 }
multi sub ToNumber(JSX::Value::Boolean $v)   { ?$v :: JSX::true :: +0 }
multi sub ToNumber(JSX::Value::Number $v)    { $v }
multi sub ToNumber(JSX::Value::String $v) {
    my $s = ~$v;
    return NaN if $s !~ rx/^<TheGrammar.StringNumericLiteral>$/;
    $s ~~ s:g/<TheGrammar.StrWhiteSpaceChar>+//;
    $s ~~ s/^([\+|\-]?)0+(<[0..9]>)/$1$2/;
    return +0 if $s eq "";
    return +$s;
}
multi sub ToNumber(JSX::Value::Object $v) {
    my $Result1 = ToPrimitive($v,'Number');
    my $Result2 = ToNumber($Result1);
    return $Result2;
}

# 9.4
multi sub ToInteger(JSX::Value $v) {
    use Math qw(sign floor abs);
    my $Result1 = ToNumber($v);
    return +0 if $Result1 == NaN;
    return $Result1 if $Result1 == (+0|-0|Inf|-Inf);
    my $Result4 = sign($Result1) * floor(abs($Result1));
    return $Result4;
}

# 9.5
multi sub ToInt32(JSX::Value $v) {
    use Math qw(sign floor abs);
    my $Result1 = ToNumber($v);
    return +0 if $Result1 == (NaN|+0|-0|Inf|-Inf);
    my $Result3 = sign($Result1) * floor(abs($Result1));
    my $n2_32 = 2**32;
    my $n2_31 = 2**31;
    my $Result4 = $Result3 % $n2_32;
    return $Result4-$n2_32 if $Result4 >= $n2_31;
    return $Result4;
}

# 9.6
multi sub ToUint32(JSX::Value $v) {
    use Math qw(sign floor abs);
    my $Result1 = ToNumber($v);
    return +0 if $Result1 == (NaN|+0|-0|Inf|-Inf);
    my $Result3 = sign($Result1) * floor(abs($Result1));
    my $n2_32 = 2**32;
    my $Result4 = $Result3 % $n2_32;
    return $Result4;
}

# 9.7
multi sub ToUint16(JSX::Value $v) {
    use Math qw(sign floor abs);
    my $Result1 = ToNumber($v);
    return +0 if $Result1 == (NaN|+0|-0|Inf|-Inf);
    my $Result3 = sign($Result1) * floor(abs($Result1));
    my $n2_16 = 2**16;
    my $Result4 = $Result3 % $n2_16;
    return $Result4;
}

# 9.8

# 9.9





class JSX::Value::Array;
submethod BUILD {
    $.property_attributes{'length'} =
        JSX::PropertyAttributes.new(:DontEnum :DontDelete);
}
# 15.4.5.1
method <[[Put]]> ($A: JSX::String $P, JSX::Value $V) {
    my $Result1 = $A.<[[CanPut]]>($P);
    return if $Result1 ~~ JSX::false;
    if $A.property_value.exists($P) {
        if $P eq 'length' { # 12-16
            my $Result12 = ToUint32($V);
            raise RangeError if $Result12 != ToNumber($V);
            my $length = +($A.property_value{'length'});
            my $k;
            loop(;$k < $length; $k++) {
                my $key = $k; # Eh. Spec would have us use ToString(k).
                $A.<[[Delete]]>($k) if exists $A.property_value{$key};
                # XXX - need to wrap $k?
            }
            $A.property_value{$P} = $Result12;
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

