
use v6-alpha;

# Perl6 implementation of the 'Rule' syntax 
# author: Flavio S. Glock - fglock@gmail.com

=for compiling

    After compiling:
    - remove all references to:
        Data::Bind
    - replace the header with:
        package Pugs::Grammar::Rule;
        no strict 'refs';
        use Pugs::Runtime::Match;
        use Pugs::Runtime::Regex;
        our %rule_terms;
        our %variables;
    - replace:  # emitter bug
            $named{'concat'} = $match;
            $named{'conjunctive'} = $match;

       - with:
            push @{ $named{'concat'} }, $match;
            push @{ $named{'conjunctive'} }, $match;

update: also replace in 'conjunctive1' and 'disjunctive1'

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
    [ <?alnum> | _ | <'::'> ]+
}

token literal {
    [ 
    |  \\ .
    |  <-[ \' ]> 
    ]*
}

token metasyntax {
    [ 
    |  \\ .
    |  \'  <?literal>     \'
    |  \{  <?string_code>        \}
    |  \<  <?metasyntax>  \>
    |  <-[ \> ]> 
    ]+ 
    { return { metasyntax => $$/ ,} }
}

token char_range {
    [ 
    |  \\ .
    |  <-[ \] ]> 
    ]+ 
}

token char_class {
    |  <?ident>
    |  \[  <?char_range>  \]
}

token string_code {
    # bootstrap "code"
    [ 
    |  \\ .
    |  \'  <?literal>     \'
    |  \{  <?string_code> \}
    |  <-[ \} ]> 
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
    | \<  <metasyntax>  \>  { return $$<metasyntax> } 
    | { die "invalid alias syntax" }
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

    '(' => token {
        <rule> \)
        { return { capturing_group => $$<rule> ,} }
    },
    '<(' => token {
        <rule>  <')>'>
        { return { capture_as_result => $$<rule> ,} }
    },
    '<after' => token {
        <?ws> <rule> \> 
        { return { after => :$$<rule>, } }
    },
    '<before' => token {
        <?ws> <rule> \> 
        { return { before => :$$<rule>, } }
    },
    '<!before' => token {
        <?ws> <rule> \> 
        { return { not_before => :$$<rule>, } }
    },
    '<!' => token {
        <metasyntax> \> 
        { return { negate  => $$<metasyntax>, } }
    },
    '<+' => token {
        $<c0> := <char_class>

        [
           \+  $<c1> := <char_class> 
           \>

           { return  

# [<before <?alpha>>|<before <?digit>>].
# print "Ast: ", Dumper( Pugs::Grammar::Rule->rule( '[<before <?alpha>>|<before <?digit>>].' )->() );

{
  'concat' => [
    {
      'quant' => {
        'ws2' => '',
        'greedy' => '',
        'quant' => '',
        'ws1' => '',
        'ws3' => '',
        'term' => {
          'alt' => [
            {
              'quant' => {
                'ws2' => '',
                'greedy' => '',
                'quant' => '',
                'ws1' => '',
                'ws3' => '',
                'term' => {
                  'before' => {
                    'rule' => {
                      'quant' => {
                        'ws2' => '',
                        'greedy' => '',
                        'quant' => '',
                        'ws1' => '',
                        'ws3' => '',
                        'term' => {
                          'metasyntax' => '+' ~ $<c0>
                        }
                      }
                    }
                  }
                }
              }
            },
            {
              'quant' => {
                'ws2' => '',
                'greedy' => '',
                'quant' => '',
                'ws1' => '',
                'ws3' => '',
                'term' => {
                  'before' => {
                    'rule' => {
                      'quant' => {
                        'ws2' => '',
                        'greedy' => '',
                        'quant' => '',
                        'ws1' => '',
                        'ws3' => '',
                        'term' => {
                          'metasyntax' => '+' ~ $<c1>
                        }
                      }
                    }
                  }
                }
              }
            }
          ]
        }
      }
    },
    {
      'quant' => {
        'ws2' => '',
        'greedy' => '',
        'quant' => '',
        'ws1' => '',
        'ws3' => '',
        'term' => {
          'dot' => 1
        }
      }
    }
  ]
}

           }

        |  \> 
           { return { metasyntax => ~ $<c0> } }
        ]
    },
    '<-' => token {
        <char_class> \>
        { return { metasyntax => '-' ~ $<char_class> } }
    },
    '<' => token { 
        <metasyntax>  \>
        { return $$<metasyntax> }
    },
    '{' => token { 
        <parsed_code>  \}
        { return { closure => $$<parsed_code> ,} }
    },
    '\\' => token {  
        | [ x | X | o | O ] \d+
          #  \x0021    \X0021
          { return { special_char => '\\' ~ $/ , } } 
        | ( x | X | o | O ) \[ (\d+) \]
          #  \x[0021]  \X[0021]
          { return { special_char => '\\' ~ $0 ~ $1 , } } 
        | .
          #  \e  \E
          { return { special_char => '\\' ~ $/ , } } 
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
        { return { modifier => 'ignorecase', :$$<rule>, } } },
    ':ignorecase'  => token { 
        <?ws> <rule> 
        { return { modifier => 'ignorecase', :$$<rule>, } } },
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
       [  <?ws>? <':='> <?ws>? <named_capture_body>
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
    |   <'**'> <?ws>? \{  <parsed_code>  \}
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
    { return { 
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
    [ <?ws>? \& ]?
    
    <concat>
    [
        \&  <concat> 
    ]*
    
    {             
      use v5;
        my @a = map {  $$_  }  @{ $::_V6_MATCH_->{'concat'} };
        return { conjunctive1 => \@a ,}  if scalar @a > 1;
        return $a[0];
      use v6;
    }
}

token conjunctive {
    [ <?ws>? \& \& ]?
    
    <conjunctive1>
    [
        \& \& <conjunctive1> 
    ]*
    
    {             
      use v5;
        my @a = map {  $$_  }  @{ $::_V6_MATCH_->{'conjunctive1'} };
        return { conjunctive => \@a ,}  if scalar @a > 1;
        return $a[0];
      use v6;
    }
}

token disjunctive1 {
    [ <?ws>? \| ]?
    
    <conjunctive>
    [
        \|  <conjunctive> 
    ]*
    
    {             
      use v5;
        my @a = map {  $$_  }  @{ $::_V6_MATCH_->{'conjunctive'} };
        return { alt1 => \@a ,}  if scalar @a > 1;
        return $a[0];
      use v6;
    }
}

token rule {
    [ <?ws>? \| \| ]?
    
    <disjunctive1>
    [
        \| \| <disjunctive1> 
    ]*
    
    {             
      use v5;
        my @a = map {  $$_  }  @{ $::_V6_MATCH_->{'disjunctive1'} };
        return { alt => \@a ,}  if scalar @a > 1;
        return $a[0];
      use v6;
    }
}
