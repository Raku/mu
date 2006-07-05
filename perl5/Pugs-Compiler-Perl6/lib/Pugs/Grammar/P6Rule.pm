# this file was extracted from the P6 version in Pugs-Compiler-Rule

package  Pugs::Grammar::P6Rule;
use strict;
use warnings;

use Pugs::Compiler::Rule;
use Pugs::Compiler::Token;
use Pugs::Compiler::Regex;
use base 'Pugs::Grammar::Base';
use Pugs::Runtime::Match::Ratchet; # overload doesn't work without this ???

our @rule_terms;

# reuse some subs
  use Pugs::Grammar::Rule; 
  # XXX - this doesn't work:
  #       "Can't call method "no_match" on an undefined value"
  #*code       = &Pugs::Grammar::Rule::code;
  #*literal    = &Pugs::Grammar::Rule::literal;
  #*metasyntax = &Pugs::Grammar::Rule::metasyntax;
  sub code       { Pugs::Grammar::Rule::code(@_) }
  sub literal    { Pugs::Grammar::Rule::literal(@_) }
  sub metasyntax { Pugs::Grammar::Rule::metasyntax(@_) }
  push @rule_terms, 'metasyntax';

*ws = Pugs::Compiler::RegexPerl5->compile(q(^((?:\\s|\\#(?-s:.)*)+)), { P5 => 0 })->code;
*variable = Pugs::Compiler::RegexPerl5->compile(q(^([\\$\\%\\@](?:(?:\\:\\:)?[_[:alnum:]]+)+)), { P5 => 0 })->code;
*positional_variable = Pugs::Compiler::RegexPerl5->compile(q(^([\\$\\%\\@]\\^(?:[_[:alnum:]]+))), { P5 => 0 })->code;
*ident = Pugs::Compiler::RegexPerl5->compile(q(^((?:(?:\\:\\:)?[_[:alnum:]]+)+)), { P5 => 0 })->code;
*num_variable = Pugs::Compiler::RegexPerl5->compile(q(^(?:\\$[[:digit:]]+)), { P5 => 0 })->code;
*dot = Pugs::Compiler::Regex->compile(q(
        \\.    
            
        { return { 'dot' => 1 ,} }
    ))->code;
*plain_text = Pugs::Compiler::Regex->compile(q(
        <alnum> | \\, | \\; | \\_ | \\/ | \\~ | \\" | \\' | \\=

        { return { 'constant' => $() ,} }
    ))->code;
*special_char = Pugs::Compiler::Regex->compile(q(
        \\\\ .

        { return { special_char => $(), } } 
    ))->code;
*non_capturing_group = Pugs::Compiler::Regex->compile(q(
        \\[ <rule> \\] 
         
        { return $_[0]{rule}() }
    ))->code;
*closure_rule = Pugs::Compiler::Regex->compile(q(
        <code>
            
        { return { closure => $_[0]{code}() ,} }
    ))->code;
*variable_rule = Pugs::Compiler::Regex->compile(q(
        <variable> | <positional_variable>
            
        { return { variable => $() ,} }
    ))->code;
*match_variable = Pugs::Compiler::Regex->compile(q(
        <num_variable>    
            
        { return { match_variable => $_[0]{num_variable}() ,} }
    ))->code;
*named_capture_body = Pugs::Compiler::Regex->compile(q(
          [ \\( <rule> \\) { return { rule => $_[0]{rule}(), } } ]
        | [ \\[ <rule> \\] { return { rule => $_[0]{rule}(), } } ]
        | [ <metasyntax> { return { rule => $_[0]{metasyntax}(), } } ]
    ))->code;
*named_capture = Pugs::Compiler::Regex->compile(q(
        \\$ \\< <ident> \\> <?ws>? \\:\\= <?ws>? <named_capture_body>
        
        { my $body = $_[0]{named_capture_body}();
          $body->{ident} = $_[0]{ident}();
          return { named_capture => $body, } 
        }
    ))->code;
*before = Pugs::Compiler::Regex->compile(q(
        \\< before <?ws> <rule> \\> 
        
        { return { before => {
                rule  => $_[0]{rule}(),
            }, } 
        }
    ))->code;
*after = Pugs::Compiler::Regex->compile(q(
        \\< after <?ws> <rule> \\> 
        
        { return { after => {
                rule  => $_[0]{rule}(),
            }, } 
        }
    ))->code;
*capturing_group = Pugs::Compiler::Regex->compile(q(
        \\( <rule> \\)
            
        { return { capturing_group => $_[0]{rule}() ,} }
    ))->code;
*colon = Pugs::Compiler::Regex->compile(q(
        ( 
            [ \\:\\:\\: ] | 
            [ \\:\\? ]   | 
            [ \\:\\+ ]   | 
            [ \\:\\: ]   | \\: |
            [ \\$\\$ ]   | \\$ |
            [ \\^\\^ ]   | \\^
        )
            
        { return { colon => $_[0]->() ,} }
    ))->code;
*quantifier = Pugs::Compiler::Regex->compile(q(
    $<ws1>   := (<?ws>?)
    $<term>  := (<@Pugs::Grammar::P6Rule::rule_terms>)
    $<ws2>   := (<?ws>?)
    $<quant> := (
        [ 
            [ \\?\\? ] |
            [ \\*\\? ] |
            [ \\+\\? ] |
            \\?       |
            \\*       |
            \\+
        ]?
    )
    $<ws3>   := (<?ws>?)
    
    { return {  
            term  => $_[0]{term}(),
            quant => $_[0]{quant}(),
            ws1   => $_[0]{ws1}(),
            ws2   => $_[0]{ws2}(),
            ws3   => $_[0]{ws3}(),
        } 
    }
))->code;
*concat = Pugs::Compiler::Regex->compile(q(
    $<q1> := (<quantifier>) 
    [
        $<q2> := (<concat>) 
        
        { return { concat => [ 
                { quant => $_[0]{q1}() ,}, 
                $_[0]{q2}(),
            ] ,} 
        } 
    |    
        { return { quant => $_[0]{q1}() ,} } 
    ]
))->code;
*rule = Pugs::Compiler::Regex->compile(q(
    $<q1> := (<concat>) 
    [
        $<q2> := (<rule>) 

        { return { alt => [ 
                $_[0]{q1}(), 
                $_[0]{q2}(),
            ] ,} 
        }
    |           
        { return $_[0]{q1}() } 
    ]
))->code;

unshift @rule_terms, 'dot';
unshift @rule_terms, 'plain_text';
unshift @rule_terms, 'special_char';
push @rule_terms, 'non_capturing_group';
unshift @rule_terms, 'closure_rule';
unshift @rule_terms, 'variable_rule';
unshift @rule_terms, 'match_variable';
unshift @rule_terms, 'named_capture';
unshift @rule_terms, 'before';
unshift @rule_terms, 'after';
unshift @rule_terms, 'capturing_group';
push @rule_terms, 'colon';

    # XXX - currying should be made automatically by <@xxx> runtime
    # curry @rule_terms with Grammar
    @rule_terms = map { 
        my $method = $_;
        sub{ 
            # warn "Trying $method\n";
            my $match = Pugs::Grammar::Rule->$method(@_);
            #warn "Match $method ".Dumper($match) if $match->{bool};
            return $match;
        }
    }
    @rule_terms;

1;
