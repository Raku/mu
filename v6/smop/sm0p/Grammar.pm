grammar sm0p {
    token frame {
        <ws> <identifier> <ws> '=' <ws> 'q:sm0p' <ws> '{' <ws> <node>+ <ws> '}' <ws> ;
        { return $<identifier> ~ ' = SMOP_DISPATCH(interpreter, '
          ~ 'SMOP__SLIME__Frame, SMOP__ID__new, SMOP__NATIVE__capture_create('
          ~ 'interpreter, SMOP__SLIME__Frame, (SMOP__Object*[]){ '
          ~ @<node>.join(',') ~ ' }, NULL)); ' }
    };

    token node {
        <ws> $responder := <identifier> '.' $identifier := <identifier> '('
        [ <ws> $invocant := <identifier> <ws> ':' ]? <ws> <positional> <ws> <named> <ws> ')' ;
        { return 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__responder, SMOP_RI(' ~ $responder ~ '), '
          ~ ' SMOP__ID__identifier, ' ~ $identifier ~ ', '
          ~ ' SMOP__ID__capture, SMOP__NATIVE__capture_create(interpreter, '
          ~ ($invocant ?? $invocant !! $responder) ~ ', '~ $<positional> ~', '~ $<named> ~') '
          ~ ',NULL }))' }
      | <ws> <identifier> <ws> ';'
        { return 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__result, ' ~ $identifier
          ~ ',NULL }))' }
      | <ws> ';'
        { return 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, NULL))' }
    };

    token positional {
        <identifier> <ws> [ , <ws> <identifier> ]*
        { return '(SMOP__Object*[]){' ~ @<identifier>.join(',') ~ ',NULL}'  }
      | { return 'NULL' }
    };

    token named {
        <pair> [ , <ws> <pair> ]*
        { return '(SMOP__Object*[]){' ~ @<pair>.join(',') ~ ',NULL}'  }
      | { return 'NULL' }
    };

    token pair {
        $key := <identifier> <ws> '=>' <ws> $val := <identifier>
        { return $key ~ ', ' ~ $val }
    };

    token identifier {
        '$' <name> { return $<name> }
      | <idconst> { return $<idconst> }
      | <name> { return $<name> }
    };

    token idconst {
        new { return 'SMOP__ID__new' }
      | lexical { return 'SMOP__ID__lexical' }
    };

    token name {
        <alpha> [ <alpha> | <digit> ]+
    };

    token ws {
        [ \s | \n  | '#' .+? \n ]*
    };
}
