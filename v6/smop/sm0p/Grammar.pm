grammar sm0p {
    token frame {
        <ws> <identifier> <ws> '=' <ws> 'q:sm0p' <ws> '{' <ws> <nodes> <ws> '}' <ws> ;
        { make $<identifier> ~ ' = SMOP_DISPATCH(interpreter, '
          ~ 'SMOP__SLIME__Frame, SMOP__ID__new, SMOP__NATIVE__capture_create('
          ~ 'interpreter, SMOP__SLIME__Frame, (SMOP__Object*[]){ '
          ~ $<nodes> ~ ' }, NULL)); ' }
    };

    token nodes {
         <node>+ { make @<node>.join(',') + ', NULL' }
       | '' { make 'NULL' }
    };

    token node {
        <node_empty> { make $<node_empty> ~ '' }
      | <node_result> { make $<node_result> ~ '' }
      | <node_capturized> { make $<node_capturized> ~ '' }
      | <node_full> { make $<node_full> ~ '' }
    };

    token node_empty {
      <ws> ';'
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, NULL))' }
    };

    token node_full {
        <ws> $responder := <identifier> '.' <identifier> '('
        [ <invocant> | '' ] <ws> <named> <ws> <positional> <ws> ')' <ws> ; <ws>
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__responder, SMOP_REFERENCE(interpreter,(SMOP__Object*)SMOP_RI(' ~ $responder ~ ')), '
          ~ ' SMOP__ID__identifier, SMOP_REFERENCE(interpreter,' ~ $<identifier> ~ '), '
          ~ ' SMOP__ID__capture, SMOP__NATIVE__capture_create(interpreter, '
          ~ 'SMOP_REFERENCE(interpreter,' ~ ($<invocant> ?? $<invocant> !! $<responder>) ~ '), '~ $<positional> ~', '~ $<named> ~') '
          ~ ' , NULL  }))' }
    };

    token node_capturized {
        <ws> $responder := <identifier> '.' <identifier> '('
        <ws> '|' $capture := <identifier> <ws> ')' <ws> ; <ws>
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__responder, SMOP_REFERENCE(interpreter,(SMOP__Object*)SMOP_RI(' ~ $responder ~ ')), '
          ~ ' SMOP__ID__identifier, SMOP_REFERENCE(interpreter,' ~ $<identifier> ~ '), '
          ~ ' SMOP__ID__capture, ' ~ $capture
          ~ ' , NULL  }))' }
    };

    token node_result {
        <ws> <identifier> <ws> ';'
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__result, SMOP_REFERENCE(interpreter,' ~ $<identifier> ~ ')'
          ~ ' , NULL}))' }
    };

    token invocant {
        <ws> <identifier> <ws> ':' <ws>
        { make $<identifier> ~ '' }
    };

    token positional {
        <identifier> <ws> [ , <ws> <identifier> ]*
        { make '(SMOP__Object*[]){' ~ @<identifier>.join(',') ~ ',NULL}'  }
      | { make 'NULL' }
    };

    token named {
        <pair> [ , <ws> <pair> ]*
        { make '(SMOP__Object*[]){' ~ @<pair>.join(',') ~ ',NULL}'  }
      | { make 'NULL' }
    };

    token pair {
        $key := <identifier> <ws> '=>' <ws> $val := <identifier>
        { make $key ~ ', ' ~ $val }
    };

    token identifier {
        <capturize>
      | '$' <name> { make $<name> ~ '' }
      | <idconst>
      | <name>
      | <nativeint>
    };

    token capturize {
        <ws> SMOP__SLIME__Capturize '.'  new  '(' <ws>
        $invocant := <cint> <ws> ',' <ws> $positional := <cintlist> <ws> ',' <ws>
        $named := <cintlist> <ws> ',' <ws> $target := <cint> ')'
        { make 'SMOP__SLIME__Capturize_create(' ~ $invocant ~ ','
          ~ $positional ~ ',' ~ $named ~ ','
          ~ $target ~ ')' }
    };

    token cint {
        \d+
    };

    token cintlist {
        <cint> [ <ws> ',' <ws> <cint> ]*
        { make '(int[]){' ~ @<cint>.join(',') ~ ',0}' }
    };

    token idconst {
      [ new|lexical|back|capture|continuation|continues|copy|
        current|debug|drop|DESTROYALL|eval|forget|free|goto|
        has_next|identifier|jail|lexical|loop|move_capturize|
        move_identifier|move_responder|new|next|past|push|responder|
        result|setr|outer
      ] { make 'SMOP__ID__' ~ $/ }
    };

    token name {
        [ a..z | A..Z ] [ \w | '_' | \d ]*
    };

    token ws {
        [ \s | \n  | '#' .+? \n ]*
    };

    token nativeint {
        \d+ { make 'SMOP__NATIVE__int_create(' ~ $/ ~ ')' };
    };
}
module main {
    $_ = slurp;
    say sm0p.frame($_);
}
