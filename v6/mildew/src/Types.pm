use v5.10;
use Emit::Yeast;
use MooseX::Declare;
class TypeInfo {
    has orgin=>(is=>'ro',isa=>'AST::Base');
    has type=>(is=>'rw',builder=>'infer_type');
    has mold=>(is=>'rw');
    method infer_type {
        Type::Unknown->new();
    }
}
class TypeInfo::Phi extends TypeInfo {
}
class TypeInfo::IntegerConstant extends TypeInfo {
}
class TypeInfo::StringConstant extends TypeInfo {
}
class Type {
    has name=>(is=>'rw');
}
class Type::Scope {
}
class Type::Unknown {
    method emit_call($i,$stmt,$value) {
        my $list = sub {
            "(SMOP__Object*[]) {" . join(',',(map {"SMOP_REFERENCE(interpreter,".$value->($_).")"} @_),"NULL") . "}"
        };
        my $capture = $stmt->rvalue->capture;
        "frame->pc = " . ($i+1) . ";\n" 
        . "frame->ret = &" . $value->($stmt->lvalue) . ";\n" 
        . Emit::Yeast::assign($value->($stmt->lvalue),
            "SMOP_DISPATCH(\n" . "interpreter,\nSMOP_RI(" . $value->($capture->invocant) . "),\n"
            . $value->($stmt->rvalue->identifier)
            . ",\nSMOP__NATIVE__capture_create(interpreter," 
            . $list->($capture->invocant,@{$capture->positional})
            . ","
            . $list->(@{$capture->named})
            . ")\n" . ")")
        . "break;\n"
    }
}
