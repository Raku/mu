
use v6-alpha;
use utf8;

# Perl6 implementation of the 'Rule' syntax 
# author: Flavio S. Glock - fglock@gmail.com

=for compiling

    After compiling:
    - remove all references to:
        Data::Bind
    - replace the header with:
        package Pugs::Grammar::Rule;
        use utf8;
        no strict 'refs';
        use Pugs::Runtime::Match;
        use Pugs::Runtime::Regex;
        our %rule_terms;
        our %variables;
        
=cut

grammar Pugs::Grammar::Rule;

#use Pugs::Runtime::Match;

our %rule_terms;
our %variables;

token pod_begin {
    |   \n =end \N*
    |   . \N* <?pod_begin>
}

token pod_other {
    |   \n =cut \N*
    |   . \N* <?pod_other>
}

token ws {
    [
    |    \# \N*
    |    \n [ = [
            |  begin <?ws> END \N* .*
            |  begin  <?pod_begin>
            |  kwid   <?pod_other>
            |  pod    <?pod_other>
            |  for    <?pod_other>
            |  head1  <?pod_other>
            ]?
            ]?
    |    \s
    ]+
}

# regex ident can start with a number
token ident {
    [ <?alnum> | _ | '::' ]+
}

# after '\\'
token special_char {  
        | ( c | C ) \[ ( [<alnum>|\s| ';' | '(' | ')' | '-' ]+) \]
          #  \c[LATIN LETTER A] 
          { return { special_char => '\\' ~ $0 ~ $1 , } } 

        | [ x | X ] <xdigit>+
          #  \x0021    \X0021
          { return { special_char => '\\' ~ $/ , } } 
        | ( x | X ) \[ (<xdigit>+) \]
          #  \x[0021]  \X[0021]
          { return { special_char => '\\' ~ $0 ~ $1 , } } 

        | [ o | O ] \d+
          #  \o0021    \O0021
          { return { special_char => '\\' ~ $/ , } } 
        | ( o | O ) \[ (\d+) \]
          #  \o[0021]  \O[0021]
          { return { special_char => '\\' ~ $0 ~ $1 , } } 

        | .
          #  \e  \E
          { return { special_char => '\\' ~ $/ , } } 
}

token literal {
    [ 
    |  \\ <special_char>
    |  <-[ \' ]> 
    ]*
}

token double_quoted {
    [ 
    |  \\ <special_char>
    |  <%Pugs::Grammar::Rule::variables>
    |  <-[ \" ]> 
    ]*
}

token metasyntax {
    [ 
    |  \\ <special_char>
    |  \'  <?literal>     \'
    |  \"  <?double_quoted>   \"
    |  \{  <?string_code>        \}
    |  \<  <?metasyntax>  \>
    |  <-[ \> ]> 
    ]+ 
}

token char_range {
    [ 
    |  \\ <special_char>
    |  <-[ \] ]> 
    ]+ 
}

token char_class {
    |  <?alpha>+
    |  \[  <?char_range>  \]
}

token string_code {
    # bootstrap "code"
    [ 
    |  \\ <special_char>
    |  \'  <?literal>     \'
    |  \"  <?double_quoted>   \"
    |  \{  [ <?string_code> | '' ]  \}
    |  \(  [ <?string_code> | '' ]  \)
    |  \<  [ <?string_code> | '' ]  \>
    |  [ <?ws> | \> | \= | \- ] \> 
    |  <?ws>
    |  <-[ \} \) \> ]> 
    ]+ 
}

token parsed_code {
    # this subrule is overridden inside the perl6 compiler
    <?string_code>
    { return '{' ~ $/ ~ '}' }
}

token named_capture_body {
    | \(  <rule>        \)  { return { capturing_group => $$<rule> ,} } 
    | \[  <rule>        \]  { return $$<rule> } 
    | \<  <parse_metasyntax>  { return $$<parse_metasyntax> } 
    | \'  <?literal>    \'
        { return { metasyntax => { metasyntax => ~ $$/ ,} } }
    | { die "invalid alias syntax" }
}

token parse_metasyntax {
        $<modifier> := [ '!' | '?' | '' ]
    [
        '{'  <parsed_code>  '}>'
        { return { closure => {
            closure  => $$<parsed_code>,
            modifier => $$<modifier>,
        } } }
    |
        <char_class>
        ( <[+-]> <char_class> )+
        \>
        { 
            if ( $$<modifier> eq '!' ) {
              return { 
                negate => {
                  char_class => [ 
                    '+' ~ $<char_class>,
                    @($/[0]),   # TODO - stringify
              ] } }
            }
            return { 
              char_class => [ 
                '+' ~ $<char_class>,
                @($/[0]),   # TODO - stringify
            ] } 
        }
    |
        <ident>
        [
          <?ws> <rule> \> 
          {
            if  ( $$<ident> eq 'before' 
               || $$<ident> eq 'after'    
                ) {
                return { $$<ident> => { rule => $$<rule>, modifier => $$<modifier> } } 
            }
            return { metasyntax => { 
                metasyntax => $$<ident>, 
                rule       => $$<rule>, 
                modifier   => $$<modifier>,
            } }
          }
        |
          ':' <?ws>?
          $<str> := [
            [ 
            |  \\ <special_char>
            |  <%Pugs::Grammar::Rule::variables>
            |  <-[ \> ]> 
            ]*
          ]
          \>
          {
            if  ( $$<ident> eq 'before' 
               || $$<ident> eq 'after'    
                ) {
                return { $$<ident> => { 
                    rule     => { metasyntax => { 
                        metasyntax => '\'' ~ $$<str> ~ '\'' 
                    } },
                    modifier => $$<modifier>,
                } } 
            }
            return { metasyntax => {
                metasyntax => $$<ident>, 
                string   => $$<str>, 
                modifier => $$<modifier>,
            } }
          }
        |
          \(  <parsed_code>  \) \>
          { return { call => { 
              method   => $$<ident>, 
              params   => $$<parsed_code>, 
              modifier => $$<modifier>,
          } } }
        ]
    |
        <metasyntax>  \>
        { return { metasyntax => {
              metasyntax => ~$$<metasyntax>, 
              modifier   => $$<modifier>,
        } } }
    ]
}


%variables = (

    '$<' => token {
        <ident> \> 
        { return { match_variable => '$' ~ $/<ident> ,} }
    },
    '$' => token { 
        <?digit>+
        { return { match_variable => '$' ~ $/ ,} }
    |
        \^?
        [ <?alnum> | _ | \: \: ]+
        { return { variable => '$' ~ $/ ,} }
    },
    '@' => token { 
        <?digit>+
        { return { match_variable => '@' ~ $/ ,} }
    |
        \^?
        [ <?alnum> | _ | \: \: ]+
        { return { variable => '@' ~ $/ ,} }
    },
    '%' => token { 
        <?digit>+
        { return { match_variable => '%' ~ $/ ,} }
    |
        \^?
        [ <?alnum> | _ | \: \: ]+
        { return { variable => '%' ~ $/ ,} }
    },

); # /%variables
    

%rule_terms = (

    '{*}' => token {
        # placeholder
        { return { metasyntax => { metasyntax => 'null' ,} } }
    },

    '\'' => token {
        <?literal>     \'
        { return { metasyntax => { metasyntax => '\'' ~ $$/ ,} } }
    },
    '(' => token {
        <rule> \)
        { return { capturing_group => $$<rule> ,} }
    },
    '<(' => token {
        <rule>  ')>'
        { return { capture_as_result => $$<rule> ,} }
    },
    '<+' => token {
        <char_class>
        ( <[+-]> <char_class> )*
        \>
        { return { 
            char_class => [ 
                '+' ~ $<char_class>,
                @($/[0]),   # TODO - stringify
            ] } 
        }
    },
    '<-' => token {
        <char_class>
        ( <[+-]> <char_class> )*
        \>
        { return { 
            char_class => [ 
                '-' ~ $<char_class>,
                @($/[0]),   # TODO - stringify
            ] } 
        }
    },
    '<[' => token { 
        <char_range>  \]
        ( <[+-]> <char_class> )*
        \>
        { return { 
            char_class => [ 
                '+[' ~ $<char_range> ~ ']',
                @($/[0]),   # TODO - stringify
            ] } 
        }
    },
    '<' => token { 
        <parse_metasyntax>
        { return $$<parse_metasyntax> }
    },
    '{' => token { 
        <parsed_code>  \}
        { return { closure => {
            closure => $$<parsed_code>,
            modifier => 'plain',
        } } }
    },
    '\\' => token {  
        <special_char>
        { return $$<special_char> }
    },
    '.' => token { 
        { return { 'dot' => 1 ,} }
    },
    '[' => token { 
        <rule> \] 
        { return $$<rule> }
    },
    ':::' => token { { return { colon => ':::' ,} } },
    ':?'  => token { { return { colon => ':?' ,} } },
    ':+'  => token { { return { colon => ':+' ,} } },
    '::'  => token { { return { colon => '::' ,} } },
    ':'   => token { { return { colon => ':'  ,} } },
    '$$'  => token { { return { colon => '$$' ,} } },
    '$'   => token { { return { colon => '$'  ,} } },
    '^^'  => token { { return { colon => '^^' ,} } },
    '^'   => token { { return { colon => '^'  ,} } },
    
    '>>'  => token { { return { colon => '>>' ,} } },
    '»'   => token { { return { colon => '>>' ,} } },

    '<<'  => token { { return { colon => '<<' ,} } },
    '«'   => token { { return { colon => '<<' ,} } },

    ':i'  => token { 
        <?ws> <rule> 
        { return { modifier => { modifier => 'ignorecase', :$$<rule>, } } } },
    ':ignorecase'  => token { 
        <?ws> <rule> 
        { return { modifier => { modifier => 'ignorecase', :$$<rule>, } } } },
    ':s'  => token { 
        <?ws> <rule> 
        { return { modifier => 'sigspace',   :$$<rule>, } } },
    ':sigspace'    => token { 
        <?ws> <rule> 
        { return { modifier => 'sigspace',   :$$<rule>, } } },
    ':P5' => token { 
        <?ws> <rule> 
        { return { modifier => 'Perl5',  :$$<rule>, } } },
    ':Perl5'       => token { 
        <?ws> <rule> 
        { return { modifier => 'Perl5',  :$$<rule>, } } },
    ':bytes'       => token { 
        <?ws> <rule> 
        { return { modifier => 'bytes',  :$$<rule>, } } },
    ':codes'       => token { 
        <?ws> <rule> 
        { return { modifier => 'codes',  :$$<rule>, } } },
    ':graphs'      => token { 
        <?ws> <rule> 
        { return { modifier => 'graphs', :$$<rule>, } } },
    ':langs'       => token { 
        <?ws> <rule> 
        { return { modifier => 'langs',  :$$<rule>, } } },

); # /%rule_terms
    
token term {
    |  <%Pugs::Grammar::Rule::variables>
       [  <?ws>? ':=' <?ws>? <named_capture_body>
          { 
            return { named_capture => {
                rule =>  $$<named_capture_body>,
                ident => $$<Pugs::Grammar::Rule::variables>,
            }, }; 
          }
       |
          { 
            return $$<Pugs::Grammar::Rule::variables> 
          }
       ]
    |  <%Pugs::Grammar::Rule::rule_terms>
        { 
            #print "term: ", Dumper( $_[0]->data );
            return $$<Pugs::Grammar::Rule::rule_terms> 
        }
    |  <-[ \] \} \) \> \: \? \+ \* \| \& ]> 
        { 
            #print "constant: ", Dumper( $_[0]->data );
            return { 'constant' => $$/ ,} 
        }
}

token quant {
    |   '**' <?ws>? \{  <parsed_code>  \}
        { return { closure => $$<parsed_code> ,} }
    |   <[  \? \* \+  ]>?
}

token quantifier {
    $<ws1>   := (<?ws>?)
    <!before  <[   \} \] \)   ]> >
    <term> 
    $<ws2>   := (<?ws>?)
    <quant>
    $<greedy> := (<[  \? \+  ]>?)
    $<ws3>   := (<?ws>?)
    { 
      if 
               $$/{'quant'}   eq ''
            && $$/{'greedy'} eq ''
            && $$/{'ws1'}    eq ''
            && $$/{'ws2'}    eq ''
            && $$/{'ws3'}    eq ''
      {
          return $$/{'term'};
      }
      return { 
        quant => { 
            term    => $$/{'term'},
            quant   => $$/{'quant'},
            greedy  => $$/{'greedy'},
            ws1     => $$/{'ws1'},
            ws2     => $$/{'ws2'},
            ws3     => $$/{'ws3'},
        } }
    }
}

token concat {
    <quantifier>+ 
    {             
      use v5;
        my @a = map {  $_->()  }  @{ $::_V6_MATCH_->{'quantifier'} };
        return { concat => \@a ,}  if scalar @a > 1;
        return $a[0];
      use v6;
    }
}

token conjunctive1 {
    [ <?ws>? \& <!before \& > ]?
    
    <concat>**{1}
    [
        \& <!before \& >  <concat> 
    ]*
    
    {             
      use v5;
        my @a = map {  $$_  }  @{ $::_V6_MATCH_->{'concat'} };
        return { conjunctive1 => \@a ,}  if scalar @a > 1;
        return $a[0];
      use v6;
    }
}

token disjunctive1 {
    [ <?ws>? \| <!before \| > ]?
    
    <conjunctive1>**{1}
    [
        \| <!before \| > <conjunctive1> 
    ]*
    
    {             
      use v5;
        my @a = map {  $$_  }  @{ $::_V6_MATCH_->{'conjunctive1'} };
        return { alt1 => \@a ,}  if scalar @a > 1;
        return $a[0];
      use v6;
    }
}

token conjunctive {
    [ <?ws>? \& \& ]?
    
    <disjunctive1>**{1}
    [
        \& \& <disjunctive1> 
    ]*
    
    {             
      use v5;
        my @a = map {  $$_  }  @{ $::_V6_MATCH_->{'disjunctive1'} };
        return { conjunctive => \@a ,}  if scalar @a > 1;
        return $a[0];
      use v6;
    }
}

token rule {
    [ <?ws>? \| \| ]?
    
    <conjunctive>**{1}
    [
        \| \| <conjunctive> 
    ]*
    
    {             
      use v5;
        my @a = map {  $$_  }  @{ $::_V6_MATCH_->{'conjunctive'} };
        return { alt => \@a ,}  if scalar @a > 1;
        return $a[0];
      use v6;
    }
}
