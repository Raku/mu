grammar sm0p is KindaPerl6::Grammar {

    token frame {
        <ws> <identifier> <ws> '=' <ws> 'q:sm0p' <ws> '{' <ws> <nodes> <ws> '}' <ws> [ ';' | '' ]
        { make $<identifier> ~ ' = SMOP_DISPATCH(interpreter, '
          ~ 'SMOP__SLIME__Frame, SMOP__ID__new, SMOP__NATIVE__capture_create('
          ~ 'interpreter, SMOP__SLIME__Frame, (SMOP__Object*[]){ '
          ~ $<nodes> ~ ' }, NULL)); ' }
    };

    token nodes {
        <node>
        [
        |   <nodes>
            { make $<node> ~ ', ' ~ $<nodes> }
        |   ''
            { make $<node> ~ ', ' }
        ]
        | '' { make ' NULL ' }
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
        [ <invocant> | '' ] <ws> <named> <ws> <positional> <ws> ')' <ws> ; <ws>
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__responder, SMOP_RI(' ~ $<responder> ~ '), '
          ~ ' SMOP__ID__identifier, ' ~ $<identifier> ~ ', '
          ~ ' SMOP__ID__capture, SMOP__NATIVE__capture_create(interpreter, '
          ~ ($<invocant> ?? $<invocant> !! $<responder>) ~ ', '~ $<positional> ~', '~ $<named> ~') '
          ~ ' , NULL  }))' }
    };

    token node_result {
        <ws> <identifier> <ws> ';'
        { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
          ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
          ~ ' SMOP__ID__result, ' ~ $<identifier>
          ~ ' , NULL}))' }
    };

    token invocant {
        <ws> <identifier> <ws> ':' <ws>
        { make $<identifier> ~ '' }
    };

    token responder {
        <identifier>
        { make $<identifier> ~ '' }
    };

    token positional {
        <positionals>
        { make '(SMOP__Object*[]){' ~ $<positionals> ~ '}'  }
      | { make ' NULL' }
    };

    token positionals {
        <identifier>
        [
        |   <ws> \, <ws> <positionals>
            { make $<identifier> ~ ', ' ~ $<positionals>  }
        |   <ws>
            { make $<identifier> ~ ', NULL' }
        ]
    }

    token named {
        <pairs>
        { make '(SMOP__Object*[]){' ~ $<pairs> ~ '}'  }
      | { make ' NULL' }
    };

    token pairs {
        <pair>
        [
            <ws> \, <ws> <pairs>
            { make $<pair> ~ ', ' ~ $<pairs> }
        |   <ws> [ \, <ws> | '' ]
            { make $<pair> ~ ', NULL' }
        ]
    }

    token pair {
        <identifier> <ws> '=>' <ws> <identifier2>
        { make $<identifier> ~ ', ' ~ $<identifier2> }
    };

    token identifier2 {
        <identifier> { make $<identifier> ~ '' }
    };

    token identifier {
        <capturize> { make $<capturize> ~ '' }
      | '$' <name> { make $<name> ~ '' }
      | <idconst> { make $<idconst> ~ ''}
      | <name> { make $<name> ~ '' }
      | <native_int> { make $<native_int> ~ '' }
    };

    token idconst {
        new { make 'SMOP__ID__new' }
      | lexical { make 'SMOP__ID__lexical' }
    };

    token name {
        <nameP5> { make $/.Str }
    }

    token ws {
        [ \s | \n | '' ]+
        { make '' }
    }; # |

    token capturize {
        <ws> SMOP__SLIME__Capturize '.'  new  '(' <ws>
        <cint1> <ws> ',' <ws> <cintlist1> <ws> ',' <ws>
        <cintlist2> <ws> ',' <ws> <cint2> ')'
        { make 'SMOP__SLIME__Capturize_create(' ~ $<cint1> ~ ','
          ~ $<cintlist1> ~ ',' ~ $<cintlist2> ~ ','
          ~ $<cint2> ~ ')' }
    };

    token cint1 { <cint> { make $<cint> ~ '' } };
    token cint2 { <cint> { make $<cint> ~ '' } };
    token cint {
        <digitsP5> { make $/ ~ '' }
    };

    token cintlist1 { <cintlist> { make $<cintlist> ~ '' } };
    token cintlist2 { <cintlist> { make $<cintlist> ~ '' } };
    token cintlist {
        '(' <cintlistbody>
        ')' {make '(int[]){ '~ $<cintlistbody> ~ ' }' }
       | '' {make 'NULL'}
    };

    token cintlistbody {
        <ws> <cint> <ws>
        [
         ',' <ws> <cintlistbody> <ws>
         { make $<cint> ~ ', ' ~ $<cintlistbody> }
        | '' { make $<cint> ~ ', NULL '}
        ]
    };

    token native_int {
        <digitsP5> { make 'SMOP__NATIVE__int_create(' ~ $/ ~ ')' };
    };

    token native_int_list {
        '(' <native_int_list_body>
        ')' {make '(SMOP__Object*[]){ '~ $<native_int_list_body> ~ ' }' }
       | '' {make 'NULL'}
    };

    token native_int_list_body {
        <ws> <native_int> <ws>
        [
         ',' <ws> <native_int_list_body> <ws>
         { make $<native_int> ~ ', ' ~ $<native_int_list_body> }
        | '' { make $<native_int> ~ ', NULL '}
        ]
    };

    token nameP5 :P5 {[a-zA-Z_][\\w_]*};
    token digitsP5 :P5 {\\d+};
}
module main {
    $_ =
'node = q:sm0p { ; $obj.method(new => $a, $a, SMOP__SLIME__Capturize.new(1,(2,3,4),(5,6,7),8)); }';
    say "Starting to parse " ~ $_;
    my $a = sm0p.frame($_);
    say "Parsed.";
    say $a;
}
