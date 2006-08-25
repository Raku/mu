# Perl6 implementation of the 'Term' syntax category
# author: Flavio S. Glock - fglock@gmail.com

use v6-alpha;

grammar Pugs::Grammar::P6Rule
    does Pugs::Grammar::BaseCategory;
# this file was extracted from the P6 version in Pugs-Compiler-Rule

use Pugs::Compiler::Rule;
use Pugs::Compiler::Token;
use Pugs::Compiler::Regex;
use Pugs::Grammar::Perl6;
use Pugs::Runtime::Match;

our @rule_terms;

# TODO - reuse 'ident' from other modules
regex ident {
    [ <alnum> | _ | <'::'> ]+
}

token capturing_group {
    \( <rule> \)
    { return { capturing_group => $/{rule}() ,} }
}

token non_capturing_group {
    \[ <rule> \] 
    { return $/{rule}() }
}

regex metasyntax {
    \< ([ <metasyntax> | . ]+?) \>
    { return { metasyntax => $/[0]() ,} }
}

token named_capture_body {
    | <capturing_group>     { return { rule => $/{capturing_group}(), } } 
    | <non_capturing_group> { return { rule => $/{non_capturing_group}(),} } 
    | <metasyntax>          { return { rule => $/{metasyntax}(), } } 
    | { die "invalid alias syntax" }
}

@Pugs::Grammar::P6Rule::rule_terms = (

  #*capturing_group = 
  token {
        \( <rule> \)
        { return { capturing_group => $/{rule}() ,} }
    },
  #*after = 
  token {
        \< after <?ws> <rule> \> 
        { return { after => {
                rule  => $/{rule}(),
            }, } 
        }
    },
  #*before = 
  token {
        \< before <?ws> <rule> \> 
        { return { before => {
                rule  => $/{rule}(),
            }, } 
        }
    },
  #*negate = 
  token {
        \< \! <rule> \> 
        { return { negate => {
                rule  => $/{rule}(),
            }, } 
        }
    },
  regex { 
    #metasyntax => 
        \< ([ <metasyntax> | \\ . | . ]+?) \>
        { return { metasyntax => $/[0]() ,} }
    },
  #*named_capture = 
  token {
        \$ \< <ident> \> <?ws>? \:\= <?ws>? <named_capture_body>
        { my $body = $/{named_capture_body}();
          $body->{ident} = $/{ident}();
          return { named_capture => $body, } 
        }
    },
  regex { 
    #match_variable 
        [ \$ | \@ | \% ] <digit>+
        { return { match_variable => $/() ,} }
    },
  regex { 
    #variable_rule => 
        [ \$ | \@ | \% ]
        \^?
        [ <alnum> | _ | \: \: ]+
        { return { variable => $() ,} }
    },
  regex { 
    # closure_rule => 
        # callback perl6 compiler
        \{ <Pugs::Grammar::Perl6.parse> \}
        { return { closure => $/{'Pugs::Grammar::Perl6.parse'}() ,} }
    },
  regex {  
    #special_char => 
        \\ .
        { return { special_char => $(), } } 
    },
  Pugs::Compiler::Token->compile( 
    #dot => 
        \.    
        { return { 'dot' => 1 ,} }
    },
  Pugs::Compiler::Token->compile( 
    #non_capturing_group => 
        \[ <rule> \] 
        { return $/{rule}() }
    },
  #*colon = 
  token {
        (  <':::'>  
        |  \:\?     
        |  \:\+     
        |  \:\:  |  \: 
        |  \$\$  |  \$ 
        |  \^\^  |  \^
        )  
        { return { colon => $/() ,} }
    },
); # /@rule_terms
    
token term {
    |  <before \} > { $::_V6_SUCCEED = 0 } 
    |  <before \] > { $::_V6_SUCCEED = 0 } 
    |  <before \) > { $::_V6_SUCCEED = 0 } 
    |  <before \> > { $::_V6_SUCCEED = 0 } 
    |  <@Pugs::Grammar::P6Rule::rule_terms>
        { 
            #print "term: ", Dumper( $_[0]->data );
            return $/{'Pugs::Grammar::P6Rule::rule_terms'}() 
        }
    |  ( <-[ \] \} \) \> \: \? \+ \* \| \& ]> )
        { 
            #print "constant: ", Dumper( $_[0]->data );
            return { 'constant' => $/[0]->() ,} 
        }
}

token quantifier {
    $<ws1>   := (<?ws>?)
    <term> 
    $<ws2>   := (<?ws>?)
    $<quant> := (
        |  <'??'>  
        |  <'*?'>  
        |  <'+?'> 
        |  <'?'>  
        |  <'*'>  
        |  <'+'> 
        |  <null>   )
    $<ws3>   := (<?ws>?)
    { return {  
            term  => $/{term}(),
            quant => $/{quant}(),
            ws1   => $/{ws1}(),
            ws2   => $/{ws2}(),
            ws3   => $/{ws3}(),
        } 
    }
}

token concat {
    $<q1> := <quantifier> 
    [   $<q2> := <concat> 
        { return { concat => [ 
                { quant => $/{q1}() ,}, 
                $/{q2}(),
            ] ,} 
        } 
    |   { return { quant => $/{q1}() ,} } 
    ]
}

token rule {
    [ <?ws>? \| ]?
    $<q1> := <concat> 
    [   \| $<q2> := <rule> 
        { return { alt => [ 
                $/{q1}(), 
                $/{q2}(),
            ] ,} 
        }
    |   { return $/{q1}() } 
    ]
}
