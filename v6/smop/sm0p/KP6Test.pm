grammar sm0p is KindaPerl6::Grammar {
    token frame {
        <ws> <identifier> <ws> '=' <ws> 'q:sm0p' <ws> '{' <ws> <node>+ <ws> '}' <ws> [ ';' | '' ]
        { make $<identifier> ~ ' = SMOP_DISPATCH(interpreter, '
          ~ 'SMOP__SLIME__Frame, SMOP__ID__new, SMOP__NATIVE__capture_create('
          ~ 'interpreter, SMOP__SLIME__Frame, (SMOP__Object*[]){ '
          ~ ($$<node>) ~ ' }, NULL)); ' }
    };

    token node {
        <ws> <responder> '.' <identifier> '('
        [ <ws> <invocant> <ws> ':' | '' ] <ws> <positional> <ws> <named> <ws> ')' ;
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__responder, SMOP_RI(' ~ $<responder> ~ '), '
          ~ ' SMOP__ID__identifier, ' ~ $<identifier> ~ ', '
          ~ ' SMOP__ID__capture, SMOP__NATIVE__capture_create(interpreter, '
          ~ ($<invocant> ?? $<invocant> !! $<responder>) ~ ', '~ $<positional> ~', '~ $<named> ~') '
          ~ ',NULL }))' }
      | <ws> <identifier> <ws> ';'
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__result, ' ~ $<identifier>
          ~ ',NULL }))' }
      | <ws> ';'
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, NULL))' }
      | '' { make 'NULL' }
    };

    token invocant {
        <identifier>
    };

    token responder {
        <identifier>
    };

    token positional {
        <positionals>
        { make '(SMOP__Object*[]){' ~ ($$<positionals>) ~ ',NULL}'  }
      | { make 'NULL' }
    };

    token positionals {
        <identifier>
        [
        |   <ws> \, <ws> <positionals>
            { make [ $$<identifier>, ( $$<positionals> ).values ] }
        |   <ws> [ \, <ws> | '' ]
            { make [ $$<identifier> ] }
        ]
    |   { make [ ] }
    }

    token named {
        <pairs>
        { make '(SMOP__Object*[]){' ~ ($$<pairs>) ~ ',NULL}'  }
      | { make 'NULL' }
    };

    token pairs {
        <pair>
        [
        |   <ws> \, <ws> <pairs>
            { make [ $$<pair>, ( $$<pairs> ).values ] }
        |   <ws> [ \, <ws> | '' ]
            { make [ $$<pair> ] }
        ]
    |   { make [ ] }
    }

    token pair {
        <identifier> <ws> '=>' <ws> <identifier2>
        { make $<identifier> ~ ', ' ~ $<identifier2> }
    };

    token identifier2 {
        <identifier>
    };

    token identifier {
        '$' <name> { make $<name> }
      | <idconst> { make $<idconst> }
      | <name> { make $<name> }
    };

    token idconst {
        new { make 'SMOP__ID__new' }
      | lexical { make 'SMOP__ID__lexical' }
    };

    token name :P5 {\\w[\\w\\d]+};

    token ws {
        [ \s | \n  | '#' .+? \n | '' ]+
    };
}
module main {
    $_ = 'node = q:sm0p { obj.method( a ) ; }';
    say sm0p.frame($_);
}
