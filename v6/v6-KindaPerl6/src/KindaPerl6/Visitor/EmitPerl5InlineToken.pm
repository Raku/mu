use v6-alpha;

# This visitor is a plugin for the perl5 emitter.
# It emits p6-regex as c code.


class KindaPerl6::Visitor::EmitPerl5InlineToken {

    # 'EmitPerl5' predefines all nodes, except 'Token'
    use KindaPerl6::Visitor::EmitPerl5;
    use KindaPerl6::Visitor::EmitTokenC;

    method visit ( $node ) {
        $node.emit_perl5_and_c;
    };
}

class CompUnit {
    method emit_perl5_and_c {
        '{package ' ~ $.name ~ ';use Inline C => Config,STRUCTS=>["match"];use Inline C => `cat "lib/KindaPerl6/Runtime/C/match.h"`.<<\'END\';' ~ Main::newline() ~ self.emit_c ~ Main::newline() ~ 'END' ~ Main::newline() ~ '}' ~ self.emit_perl5;
    }
}
class Token {    
    method emit_perl5 {
        '::DISPATCH(::DISPATCH($::' ~ $KindaPerl6::Visitor::EmitPerl5::current_compunit ~ ',"HOW"),"add_method",::DISPATCH($::Str,"new",\''~$.name~
        '\'),::DISPATCH($::Method,"new",sub {my $match = '~ Main::mangle_ident($KindaPerl6::Visitor::EmitPerl5::current_compunit~'::'~$.name) ~ '(GLOBAL::_str($_[1]),GLOBAL::_int($_[2]));::DISPATCH($::Match,"new",{match_str=>$_[1],bool=>::DISPATCH($::Bit,"new",$match->boolean),from=>::DISPATCH($::Int,"new",$match->from),to=>::DISPATCH($::Int,"new",$match->to)})}))';
    }
}
