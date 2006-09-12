# this file was extracted from the P6 version in Pugs-Compiler-Rule

package  Pugs::Grammar::P6Rule;
use strict;
use warnings;

use Pugs::Compiler::Rule;
use Pugs::Compiler::Token;
use Pugs::Compiler::Regex;
#use Pugs::Grammar::Perl6;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Runtime::Match; # overload doesn't work without this ???

our @rule_terms;

# TODO - reuse 'ident' from other modules
Pugs::Compiler::Regex->install( 
    ident => 
    q(
        [ <alnum> | _ | \\: \\: ]+
    ));

Pugs::Compiler::Token->install(
    capturing_group =>
    q(
        \\( <rule> \\)
        { return { capturing_group => $/{rule}() ,} }
    ));

Pugs::Compiler::Token->install( 
    non_capturing_group => 
    q(
        \\[ <rule> \\] 
        { return $/{rule}() }
    ));

Pugs::Compiler::Regex->install( 
    metasyntax => 
    q(
        \\< ([ <metasyntax> | . ]+?) \\>
        { return { metasyntax => $/[0]() ,} }
    ));

Pugs::Compiler::Token->install( 
    named_capture_body => 
    q(
    | <capturing_group>     { return { rule => $/{capturing_group}(), } } 
    | <non_capturing_group> { return { rule => $/{non_capturing_group}(),} } 
    | <metasyntax>          { return { rule => $/{metasyntax}(), } } 
    | { die "invalid alias syntax" }
    ));

@Pugs::Grammar::P6Rule::rule_terms = (

  #*capturing_group = 
  Pugs::Compiler::Token->compile(q(
        \\( <rule> \\)
        { return { capturing_group => $/{rule}() ,} }
    )),
  #*after = 
  Pugs::Compiler::Token->compile(q(
        \\< after <?ws> <rule> \\> 
        { return { after => {
                rule  => $/{rule}(),
            }, } 
        }
    )),
  #*before = 
  Pugs::Compiler::Token->compile(q(
        \\< before <?ws> <rule> \\> 
        { return { before => {
                rule  => $/{rule}(),
            }, } 
        }
    )),
  Pugs::Compiler::Token->compile(q(
        \\< \\! before <?ws> <rule> \\> 
        { return { not_before => { 
                rule  => $/{rule}(),
            }, } 
        }
    )),
  #*negate = 
  Pugs::Compiler::Token->compile(q(
        \\< \\! <rule> \\> 
        { return { negate => {
                rule  => $/{rule}(),
            }, } 
        }
    )),
  Pugs::Compiler::Regex->compile( 
    #metasyntax => 
    q(  \\< ([ <metasyntax> | \\\\ . | . ]+?) \\>
        { return { metasyntax => $/[0]() ,} }
    )),
  #*named_capture = 
  Pugs::Compiler::Token->compile(q(
        \\$ \\< <ident> \\> <?ws>? \\:\\= <?ws>? <named_capture_body>
        { my $body = $/{named_capture_body}();
          $body->{ident} = $/{ident}();
          return { named_capture => $body, } 
        }
    )),
  Pugs::Compiler::Regex->compile( 
    #match_variable => 
    q(
        [ \\$ | \\@ | \\% ] <digit>+
        { return { match_variable => $/() ,} }
    )),
  Pugs::Compiler::Regex->compile( 
    #variable_rule => 
    q(
        [ \\$ | \\@ | \\% ]
        \\^?
        [ <alnum> | _ | \\: \\: ]+
        { return { variable => $() ,} }
    )),
  Pugs::Compiler::Regex->compile( 
    # closure_rule => 
    q(
        # callback perl6 compiler
        \\{ <Pugs::Grammar::Perl6.parse> \\}
        { return { closure => 
           { bare_block => $/{'Pugs::Grammar::Perl6.parse'}() },
        } }
    )),
  Pugs::Compiler::Regex->compile(  
    #special_char => 
    q(
        \\\\ .
        { return { special_char => $(), } } 
    )),
  Pugs::Compiler::Token->compile( 
    #dot => 
    q(
        \\.    
        { return { 'dot' => 1 ,} }
    )),
  Pugs::Compiler::Token->compile( 
    #non_capturing_group => 
    q(
        \\[ <rule> \\] 
        { return $/{rule}() }
    )),
  #*colon = 
  Pugs::Compiler::Token->compile(q(
        (  <':::'>  
        |  \\:\\?     
        |  \\:\\+     
        |  \\:\\:  |  \\: 
        |  \\$\\$  |  \\$ 
        |  \\^\\^  |  \\^
        )  
        { return { colon => $/() ,} }
    )),
); # /@rule_terms
    
*term = Pugs::Compiler::Token->compile(q^
    |  <before \\} > { $::_V6_SUCCEED = 0 } 
    |  <before \\] > { $::_V6_SUCCEED = 0 } 
    |  <before \\) > { $::_V6_SUCCEED = 0 } 
    |  <before \\> > { $::_V6_SUCCEED = 0 } 
    |  <@Pugs::Grammar::P6Rule::rule_terms>
        { 
            #print "term: ", Dumper( $_[0]->data );
            return $/{'Pugs::Grammar::P6Rule::rule_terms'}() 
        }
    |  ( <-[ \\] \\} \\\) \\> \\: \\? \\+ \\* \\| \\& ]> )
        { 
            #print "constant: ", Dumper( $_[0]->data );
            return { 'constant' => $/[0]->() ,} 
        }
    ^ )->code;

*quantifier = Pugs::Compiler::Token->compile(q(
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
))->code;
*concat = Pugs::Compiler::Token->compile(q(
    $<q1> := <quantifier> 
    [   $<q2> := <concat> 
        { return { concat => [ 
                { quant => $/{q1}() ,}, 
                $/{q2}(),
            ] ,} 
        } 
    |   { return { quant => $/{q1}() ,} } 
    ]
))->code;
*rule = Pugs::Compiler::Token->compile(q(
    [ <?ws>? \\| ]?
    $<q1> := <concat> 
    [   \\| $<q2> := <rule> 
        { return { alt => [ 
                $/{q1}(), 
                $/{q2}(),
            ] ,} 
        }
    |   { return $/{q1}() } 
    ]
))->code;

1;

__END__


package  Pugs::Grammar::P6Rule;
use strict;
use warnings;

use base qw(Pugs::Grammar::Rule);
use Pugs::Runtime::Match; # overload doesn't work without this ???

*code = Pugs::Compiler::Token->compile( q(
    \\{ <Pugs::Grammar::Perl6.parse> \\}
    { return $/{'Pugs::Grammar::Perl6.parse'}() }
))->code;
    
1;
