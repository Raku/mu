grammar sm0p;
token frame {
    <ws> <identifier>  <ws> '=' <ws> 'q:sm0p' <ws> '{' <ws> <nodes> <ws> '}' <ws> ';'?
    { make $<identifier> ~ ' = SMOP_DISPATCH(interpreter, '
      ~ 'SMOP__SLIME__Frame, SMOP__ID__new, SMOP__NATIVE__capture_create('
      ~ 'interpreter, SMOP__SLIME__Frame, (SMOP__Object*[]){ '
      ~ $<nodes> ~ ' }, NULL)); ' }
}

token nodes {
    <node>*
    { if ($<node>.elems) { make $<node>.join(', ') ~ ', NULL'} else { make '' }}
}

token node {
    <node_empty> { make $<node_empty> ~ '' }
    || <node_result> { make $<node_result> ~ '' }
    || <node_capturized> { make $<node_capturized> ~ '' }
    || <node_full> { make $<node_full> ~ '' }
}

token node_empty {
    # <ws>
  ';'
    { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
      ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, NULL))' }
}

token node_full {
    <ws> <responder> '.' <identifier> '('
    [ <invocant>||'' ] <ws> <named> <ws> <positional> <ws> ')' <ws> ';' <ws>
    { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
      ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
      ~ ' SMOP__ID__responder, SMOP_REFERENCE(interpreter,(SMOP__Object*)SMOP_RI(' ~ $<responder> ~ ')), '
      ~ ' SMOP__ID__identifier, SMOP_REFERENCE(interpreter,' ~ $<identifier> ~ '), '
      ~ ' SMOP__ID__capture, SMOP__NATIVE__capture_create(interpreter, '
      ~ 'SMOP_REFERENCE(interpreter,' ~ ($<invocant> ?? $<invocant> !! $<responder>) ~ '), '~ $<positional> ~', '~ $<named> ~') '
      ~ ' , NULL  }))' }
}

token node_capturized {
    <ws> <responder> '.' <identifier> '('
    <ws> '|' <identifier2> <ws> ')' <ws> ';' <ws>
    { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
      ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
      ~ ' SMOP__ID__responder, SMOP_REFERENCE(interpreter,(SMOP__Object*)SMOP_RI(' ~ $<responder> ~ ')), '
      ~ ' SMOP__ID__identifier, SMOP_REFERENCE(interpreter,' ~ $<identifier> ~ '), '
      ~ ' SMOP__ID__capture, ' ~ $<identifier2> 
      ~ ' , NULL  }))' }
}

token node_result {
    <ws> <identifier> <ws> ';'
    { make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
      ~ ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
      ~ ' SMOP__ID__result, SMOP_REFERENCE(interpreter,' ~ $<identifier> ~ ')'
      ~ ' , NULL}))' }
}

token invocant {
    <ws> <identifier> <ws> ':' <ws>
    { make $<identifier> ~ '' }
}

token responder {
    <identifier>
    { make $<identifier> ~ '' }
}

token positional {
    <positionals>
    { make '(SMOP__Object*[]){' ~ $<positionals> ~ '}'  }
 ||{ make ' NULL' }
}

token positionals {
    <identifier>
    [
   ||  <ws> \, <ws> <positionals>
        { make $<identifier> ~ ', ' ~ $<positionals>  }
   ||  <ws>
        { make $<identifier> ~ ', NULL' }
    ]
}

token named {
    <pairs>
    { make '(SMOP__Object*[]){' ~ $<pairs> ~ '}'  }
 ||{ make ' NULL' }
}

token pairs {
    <pair>
    [
        <ws> \, <ws> <pairs>
        { make $<pair> ~ ', ' ~ $<pairs> }
   ||  <ws> [ \, <ws> | '' ]
        { make $<pair> ~ ', NULL' }
    ]
}

token pair {
    <identifier> <ws> '=>' <ws> <identifier2>
    { make $<identifier> ~ ', ' ~ $<identifier2> }
}

token identifier2 {
    <identifier> { make $<identifier> ~ '' }
}

token identifier {
    <capturize> { make $<capturize> ~ '' }
 ||'$' <name> { make $<name> ~ '' }
 ||<idconst> { make $<idconst> ~ ''}
 ||<name> { make $<name> ~ '' }
 ||<nativeint> { make $<nativeint> ~ '' }
}

token idconst {
  <idconst_list> { make 'SMOP__ID__' ~ $<idconst_list> ~ '' }
}

token idconst_list {(new|lexical|back|capture|continuation|continues|copy|current|debug|drop|DESTROYALL|eval|forget|free|goto|has_next|identifier|jail|lexical|loop|move_capturize|move_identifier|move_responder|new|next|past|push|responder|result|setr|outer)}

token name {
    <nameP5>
}

token ws  {\s*} 

token capturize {
    <ws> SMOP__SLIME__Capturize '.'  new  '(' <ws>
    <cint1> <ws> ',' <ws> <cintlist1> <ws> ',' <ws>
    <cintlist2> <ws> ',' <ws> <cint2> ')'
    { make 'SMOP__SLIME__Capturize_create(' ~ $<cint1> ~ ','
      ~ $<cintlist1> ~ ',' ~ $<cintlist2> ~ ','
      ~ $<cint2> ~ ')' }
}

token cint1 { <cint> { make $<cint> ~ '' } }
token cint2 { <cint> { make $<cint> ~ '' } }
token cint {
    <digitsP5> { make $<digitsP5> ~ '' }
}

token cintlist1 { <cintlist> { make $<cintlist> ~ '' } }
token cintlist2 { <cintlist> { make $<cintlist> ~ '' } }
token cintlist {
    '('
    [ <cintlistbody> ')' {make '(int[]){ '~ $<cintlistbody> ~ ' }' }
   ||')' {make 'NULL'} ] 
  ||'' {make 'NULL'}
}

token cintlistbody {
    <ws> <cint> <ws>
[
     ',' <ws> <cintlistbody> <ws>
     { make $<cint> ~ ', ' ~ $<cintlistbody> }
   ||'' { make $<cint> ~ ', 0 '}
    ]
}

token nativeint {
    <digitsP5> { make 'SMOP__NATIVE__int_create(' ~ $<digitsP5> ~ ')' }
}

token nativeint_list {
    '(' <nativeint_list_body>
    ')' {make '(SMOP__Object*[]){ '~ $<nativeint_list_body> ~ ' }' }
  ||'' {make 'NULL'}
}

token nativeint_list_body {
    <ws> <nativeint> <ws>
    [
     ',' <ws> <nativeint_list_body> <ws>
     { make $<nativeint> ~ ', ' ~ $<nativeint_list_body> }
   ||'' { make $<nativeint> ~ ', NULL '}
    ]
}

token nameP5 {<[a-zA-Z_]>\w*}
#token nameP5 {<[a-zA-Z_]>\w*}
token digitsP5 {\d+}
