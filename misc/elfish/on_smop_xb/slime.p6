class Node {
    has $.identifier;
    has $.responder;
    has $.capture;
    has $.result;
    method to_c() {
        my $responder = 
        my $code = 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new,  SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){';
        my $attrs = {};
        if $.identifier {
            $attrs<identifier> = 'SMOP__NATIVE__idconst_create("'~$.identifier~'")';
        }
        if $.capture {
            $attrs<capture> = $.capture.to_c;
        }
        if $.capture {
            $attrs<responder> = 'SMOP_REFERENCE(interpreter,(SMOP__Object*)SMOP_RI('~$.capture.invocant~'))';
        }
        for $attrs.keys {
            $code = $code ~ 'SMOP__ID__' ~ $_ ~ ',' ~ $attrs{$_} ~ ',';
        }
        $code = $code ~ 'NULL  }))';
    }
}
class Frame {
    has $.nodes;
    method to_c() {
        my $nodes = 'NULL';
        if ($.nodes) {
            $nodes = $.nodes.map(sub($n) {$n.to_c}).join(',') ~ ',NULL';
        }
        'SMOP_DISPATCH(interpreter,SMOP__SLIME__Frame,SMOP__ID__new,SMOP__NATIVE__capture_create(interpreter,SMOP__SLIME__Frame,(SMOP__Object*[]){'~$nodes~'},NULL))'
    }
}
class Capture {
    has $.invocant;
    has $.positional;
    has $.named;
    method to_c() {
        my $invocant = $.invocant;
        'SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,'~$invocant~'),  NULL,  NULL)';
    }
}
my $frame = Frame.new('nodes',[
    Node.new(),
    Node.new('identifier','identifier','capture',Capture.new('invocant','hi_sayer'));
]);

sub headers {
    '#include <stdio.h>
#include <smop.h>
#include <smop_lowlevel.h>
';
}
sub hi_sayer {
    '
static SMOP__Object* say_hi(SMOP__Object* interpreter,
            SMOP__ResponderInterface* self,
            SMOP__Object* identifier,      
            SMOP__Object* capture) {       
    if (identifier == SMOP__ID__identifier) {
        printf("hi\n");
    }
    SMOP_RELEASE(interpreter,capture);
    return NULL;
}
static SMOP__Object* create_hi_sayer(void) {
    SMOP__Object* hi_sayer = smop_lowlevel_alloc(sizeof(SMOP__ResponderInterface));
    SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*)hi_sayer;
    ri->MESSAGE = say_hi;
    ri->REFERENCE = smop_lowlevel_refcnt_inc;
    ri->RELEASE = smop_lowlevel_refcnt_dec;
    return hi_sayer;
}
';
}
say headers();
say hi_sayer();
say 'int main() {
    smop_init();';
say "   SMOP__Object* hi_sayer = create_hi_sayer();\n";
say '   SMOP__Object* interpreter = SMOP_DISPATCH(SMOP__INTPTR__InterpreterInstance, SMOP_RI(SMOP__INTPTR__InterpreterInstance), SMOP__ID__new,SMOP__NATIVE__capture_create(SMOP__INTPTR__InterpreterInstance,SMOP__INTPTR__InterpreterInstance,NULL,NULL));';

say '   SMOP_DISPATCH(interpreter, SMOP_RI(interpreter), SMOP__ID__goto,' ~ $frame.to_c ~');';
say '   SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
    SMOP__ID__loop, SMOP__NATIVE__capture_create(interpreter,
    SMOP_REFERENCE(interpreter,interpreter),NULL, NULL));
    SMOP_RELEASE(interpreter,hi_sayer);
    SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance,interpreter);
    smop_destr();
    return 0;
}';
