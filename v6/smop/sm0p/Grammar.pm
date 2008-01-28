grammar sm0p {
    token frame {
        <ws>* <identifier> <ws>* = <ws>* q:sm0p <ws>* { <ws>* <node>+ <ws>* } <ws>* ;
        { return $<identifier> ~ ' = SMOP_DISPATCH(interpreter, '
          ~ 'SMOP__SLIME__Frame, SMOP__ID__new, SMOP__NATIVE__capture_create('
          ~ 'interpreter, SMOP__SLIME__Frame, (SMOP__Object*[]){ '
          ~ @<node>.join(',') ~ ' }, NULL)); ' #: }
    };
    token node {
        <node_nocapture>
      | <node_invocant_only>
    };

    token node_nocapture {
        <ws>* $invocant := <identifier> . $identifier := <idconst> ( <ws>* ) ;
        { return 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__responder, ' ~ $invocant
          ~ ' SMOP__ID__capture, SMOP__NATIVE__capture_create(interpreter, ' ~ $invocant ~ ', NULL, NULL) '
          ~ ' }))' }
    };

    token node_invocant_only {
        <ws>* $responder := <identifier> . $identifier := <idconst> (
        $invocant := <identifier> <ws>* : <ws>* ) ;
        { return 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__responder, ' ~ $responder
          ~ ' SMOP__ID__capture, SMOP__NATIVE__capture_create(interpreter, ' ~ $invocant ~ ', NULL, NULL) '
          ~ ' }))' }
    };

    token identifier {
        
    }
}
