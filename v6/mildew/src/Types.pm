use v5.10;
use Emit::Yeast;
use MooseX::Declare;
class TypeInfo {
    has type=>(is=>'rw',builder=>'infer_type');
    has mold=>(is=>'rw',isa=>'AST::Block::SSA');
    method infer_type {
        Type::Unknown->new();
    }
}
class TypeInfo::FromAssignment extends TypeInfo {
    has orgin=>(is=>'ro',isa=>'AST::Base',required=>1);
    method infer_type {
        my $rvalue = $self->orgin->rvalue;
        if ($rvalue->isa('AST::Call')) {
            $rvalue->capture->invocant->type_info->type->method_call($rvalue);
        } else {
            Type::Unknown->new();
        }
    }
}
class TypeInfo::Phi extends TypeInfo {
}
class TypeInfo::IntegerConstant extends TypeInfo {
}
class TypeInfo::StringConstant extends TypeInfo {
}
class TypeInfo::External extends TypeInfo {
}
class Type {
    sub is_str {
        my ($value,$str) = @_;
        defined $value && $value->isa('AST::StringConstant') && $value->value eq $str;
    }
    method method_call($call) {
        Type::Unknown->new();
    }
    method emit_call($i,$stmt,$value) {
        my $list = sub {
            "(SMOP__Object*[]) {" . join(',',(map {"SMOP_REFERENCE(interpreter,".$value->($_).")"} @_),"NULL") . "}"
        };
        my $capture = $stmt->rvalue->capture;

        Emit::Yeast::measure($stmt->id,"frame->pc = " . ($i+1) . ";\n" 
        . "frame->ret = &" . $value->($stmt->lvalue) . ";\n" 
        . Emit::Yeast::assign($value->($stmt->lvalue),
            "SMOP_DISPATCH(\n" . "interpreter,\nSMOP_RI(" . $value->($capture->invocant) . "),\n"
            . $value->($stmt->rvalue->identifier)
            . ",\nSMOP__NATIVE__capture_create(interpreter," 
            . $list->($capture->invocant,@{$capture->positional})
            . ","
            . $list->(@{$capture->named})
            . ")\n" . ")")
        . "break;\n");
    }
}
class Type::Scope extends Type {
    method method_call($call) {
        if (Type::is_str($call->identifier,'lookup') && Type::is_str($call->capture->positional->[0],'MildewSOLoader')) {
            Type::Scalar->new(content=>Type::MildewSOLoader->new());
        } else {
            Type::Unknown->new();
        }
    }
}
class Type::Scalar extends Type {
    has content=>(is=>'ro',isa=>'Type');
    method method_call($call) {
        if (Type::is_str($call->identifier,'FETCH')) {
            $self->content;
        } else {
            Type::Unknown->new();
        }
    }
}
class Type::Unknown extends Type {
}
class Type::MildewSOLoader extends Type {
}
