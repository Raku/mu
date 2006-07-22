# this file was extracted from the P6 version in Pugs-Compiler-Rule

package  Pugs::Grammar::P6Rule;
use strict;
use warnings;

use Pugs::Compiler::Rule;
use Pugs::Compiler::Token;
use Pugs::Compiler::Regex;
use Pugs::Grammar::Perl6;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Runtime::Match::Ratchet; # overload doesn't work without this ???

our @rule_terms;

# TODO - reuse 'ident' from other modules
Pugs::Compiler::Regex->install( 
    ident => q(
        [ <alnum> | _ | \\: \\: ]+
    ));

Pugs::Compiler::Regex->install( 
    closure_rule => q(
        # callback perl6 compiler
        \\{ <Pugs::Grammar::Perl6.parse> \\}
        { return { closure => $/{'Pugs::Grammar::Perl6.parse'}() ,} }
    ));
Pugs::Compiler::Regex->install( 
    variable_rule => q(
        [ \\$ | \\@ | \\% ]
        \\^?
        [ <alnum> | _ | \\: \\: ]+
        { return { variable => $() ,} }
    ));
Pugs::Compiler::Regex->install( 
    match_variable => q(
        [ \\$ | \\@ | \\% ] <digit>+
        { return { match_variable => $/() ,} }
    ));
Pugs::Compiler::Regex->install( 
    metasyntax => q(
        \\< ([ <alnum> | <ws> ]+) \\>
        { return { metasyntax => $/[0]() ,} }
    ));
Pugs::Compiler::Regex->install( 
    dot => q(
        \\.    
        { return { 'dot' => 1 ,} }
    ));
Pugs::Compiler::Regex->install( 
    special_char => q(
        \\\\ .
        { return { special_char => $(), } } 
    ));
Pugs::Compiler::Regex->install( 
    non_capturing_group => q(
        \\[ : <rule> \\] 
        { return $/{rule}() }
    ));
*named_capture_body = Pugs::Compiler::Regex->compile(q(
        |  \\( : <rule> \\) { return { rule => $/{rule}(),       } } 
        |  \\[ : <rule> \\] { return { rule => $/{rule}(),       } } 
        |    <metasyntax>   { return { rule => $/{metasyntax}(), } } 
    ))->code;
*named_capture = Pugs::Compiler::Regex->compile(q(
        \\$ \\< <ident> \\> <?ws>? \\:\\= <?ws>? <named_capture_body>
        { my $body = $/{named_capture_body}();
          $body->{ident} = $/{ident}();
          return { named_capture => $body, } 
        }
    ))->code;
*before = Pugs::Compiler::Regex->compile(q(
        \\< before <?ws> : <rule> \\> 
        { return { before => {
                rule  => $/{rule}(),
            }, } 
        }
    ))->code;
*after = Pugs::Compiler::Regex->compile(q(
        \\< after <?ws> : <rule> \\> 
        { return { after => {
                rule  => $/{rule}(),
            }, } 
        }
    ))->code;
*capturing_group = Pugs::Compiler::Regex->compile(q(
        \\( : <rule> \\)
        { return { capturing_group => $/{rule}() ,} }
    ))->code;
*colon = Pugs::Compiler::Regex->compile(q(
        (  \\:\\:\\:  
        |  \\:\\?     
        |  \\:\\+     
        |  \\:\\:  |  \\: 
        |  \\$\\$  |  \\$ 
        |  \\^\\^  |  \\^
        )  
        { return { colon => $/() ,} }
    ))->code;
    
*term = Pugs::Compiler::Regex->compile(q(
    |  $<term>  := <@Pugs::Grammar::P6Rule::rule_terms>
          { return $/{term}() }
    |  .  { return { 'constant' => $() ,} }
    ))->code;

*quantifier = Pugs::Compiler::Regex->compile(q(
    $<ws1>   := (<?ws>?)
    <term>
    $<ws2>   := (<?ws>?)
    $<quant> := (
        |  \\?\\?  
        |  \\*\\?  
        |  \\+\\? 
        |  \\?       
        |  \\*       
        |  \\+
        |  <''>   )
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
*concat = Pugs::Compiler::Regex->compile(q(
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
*rule = Pugs::Compiler::Regex->compile(q(
    [ <?ws> \\| ]?
    $<q1> := <concat> 
    [   \\| : $<q2> := <rule> 
        { return { alt => [ 
                $/{q1}(), 
                $/{q2}(),
            ] ,} 
        }
    |   { return $/{q1}() } 
    ]
))->code;


    # XXX - currying should be made automatically by <@xxx> runtime
    # curry @rule_terms with Grammar
    @rule_terms = map { 
        my $method = $_;
        sub{ 
            #warn "Trying $method\n";
            # $str, $state, $_[2], $_[3]{match}, @_[4,5,6,7]
            my $match = Pugs::Grammar::P6Rule->$method($_[0], { p => 0 } );
            #warn "Match $method ".Dumper($match) if $match->{bool};
            return $match;
        }
    }
    qw(  capturing_group 
         after before metasyntax 
         named_capture match_variable
         variable_rule closure_rule special_char dot
         non_capturing_group colon 
    );

1;

