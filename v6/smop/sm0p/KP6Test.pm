grammar sm0p is KindaPerl6::Grammar {

    token frame {
        <ws> <identifier> <ws> '=' <ws> 'q:sm0p' <ws> '{' <ws> <nodes> <ws> '}' <ws> [ ';' | '' ]
        { make $<identifier> ~ ' = SMOP_DISPATCH(interpreter, '
          ~ 'SMOP__SLIME__Frame, SMOP__ID__new, SMOP__NATIVE__capture_create('
          ~ 'interpreter, SMOP__SLIME__Frame, (SMOP__Object*[]){ '
          ~ ($<nodes>) ~ ' }, NULL)); ' }
    };

    token nodes {
        <node>
        [
        |   <nodes>
            { make $<node> ~ ', ' ~ $<nodes> }
        |   ''
            { make $<node> ~ ', ' }
        ]
        | '' { make 'NULL' }
    }

    token node {
        <node_empty> { make $<node_empty> ~ '' }
      | <node_result> { make $<node_result> ~ '' }
      | <node_full> { make $<node_full> ~ '' }
    };

    token node_empty {
      <ws> ';'
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, NULL))' }
    };

    token node_full {
        <ws> <responder> '.' <identifier> '('
        [ <ws> <invocant> <ws> ':' | '' ] <ws> <positional> <ws> <named> <ws> ')' <ws> ; <ws>
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__responder, SMOP_RI(' ~ $<responder> ~ '), '
          ~ ' SMOP__ID__identifier, ' ~ $<identifier> ~ ', '
          ~ ' SMOP__ID__capture, SMOP__NATIVE__capture_create(interpreter, '
          ~ ($<invocant> ?? $<invocant> !! $<responder>) ~ ', '~ $<positional> ~', '~ $<named> ~') '
          ~ ',NULL }))' }
    };

    token node_result {
        <ws> <identifier> <ws> ';'
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__result, ' ~ $<identifier>
          ~ ',NULL }))' }
    };

    token invocant {
        <identifier>
        { make $<identifier> ~ '' }
    };

    token responder {
        <identifier>
        { make $<identifier> ~ '' }
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
            { make $<identifier> ~ $<positionals>  }
        |   <ws> [ \, <ws> | '' ]
            { make $<identifier> ~ '' }
        ]
        |   { make '' }
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
            { make $<pair> ~ $<pairs> }
        |   <ws> [ \, <ws> | '' ]
            { make $<pair> ~ '' }
        ]
        |   { make '' }
    }

    token pair {
        <identifier> <ws> '=>' <ws> <identifier2>
        { make $<identifier> ~ ', ' ~ $<identifier2> }
    };

    token identifier2 {
        <identifier> { make $<identifier> ~ '' }
    };

    token identifier {
        '$' <name> { make $<name> ~ '' }
      | <idconst> { make $<idconst> ~ ''}
      | <name> { make $<name> ~ '' }
    };

    token idconst {
        new { make 'SMOP__ID__new' }
      | lexical { make 'SMOP__ID__lexical' }
    };

    token name {
        <namep5>
        { make $<namep5> ~ '' }
    };

    token namep5 :P5 {\\w[\\w\\d]*};

    token ws {
        [ \s | \n  | '#' .+? \n | '' ]+
        { make '' }
    };
}
module main {
    $_ =
'node = q:sm0p { $obj.method( a ) ; }';
    say "Starting to parse " ~ $_;
    my $a = sm0p.frame($_);
    say "Parsed.";
    say $a;
}
