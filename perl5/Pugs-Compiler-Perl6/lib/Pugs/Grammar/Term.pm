package Pugs::Grammar::Term;
use utf8;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Runtime::Match;
use Pugs::Compiler::Token;

our %hash;

# <audreyt>
# $infix:<plus>     plus => $infix<plus>
# :$<foo> is a special case of that
# :$/<foo> is not valid
# $/:<foo> would be the currently specced equiv
# another thought is to make :%h<foo> parse as foo=>%h<foo>
# not (h=>%h)<foo> which is likely nonsensical


*cpan_bareword = Pugs::Compiler::Token->compile( '
    [ _ | <?alnum> | \: ]+ 
    \- 
    [ _ | <?alnum> | \- | \. | \* ]+ 
    <before \( | \; | \s | $ > 
',
    { grammar => __PACKAGE__ }
)->code;

*perl5source = Pugs::Compiler::Token->compile( q(
    ( [ <!before [ \} | ; | <?ws> ] use <?ws> v6 > . ]+ )
    #<-[ ;\}\)\] ]>* 
        { return { 
            perl5source => $_[0][0]->() 
        } }
),
    { grammar => __PACKAGE__ }
)->code;

sub substitution {
    my $grammar = shift;
    return $grammar->no_match(@_) unless $_[0];
    my $pos = $_[1]{p} || 0;
    my $s = substr( $_[0], $pos );
    my $options;
    while ($s =~ s/^:(\w+)//) {
	$options->{lc($1)} = 1;
    }
    return $grammar->no_match(@_) unless substr($s, 0 , 1) eq '/';
    substr($s, 0, 1, '');
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( "/" . $s, "/" );
    return $grammar->no_match(@_) unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    my $extracted2;
    ($extracted2,$remainder) = Text::Balanced::extract_delimited( "/" . $remainder, "/" );
    return $grammar->no_match(@_) unless length($extracted2) > 0;
    $extracted2 = substr( $extracted2, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => \1,
        str     => \$_[0],
        match   => [],
        from    => \$pos,
        to      => \( length($_[0]) - length($remainder) ),
        capture => \{ options => $options, substitution => [$extracted, $extracted2] },
    } );
};

my %openmatch = ( '/' => '/',
                  '{' => '}',
                  '[' => ']',
                  '!' => '!',
                  '\'' => '\'');

sub rx {
    my $grammar = shift;
    return $grammar->no_match(@_) unless $_[0];
    my $options;
    my $pos = $_[1]{p} || 0;
    while ( substr( $_[0], $pos ) =~ m/^:(\w+)/ ) {
        $options->{lc($1)} = 1;
        $pos += 1 + length($1);
    }
    $pos++ while substr($_[0], $pos) =~ /^\s/;
    my $open = substr($_[0], $pos , 1);
    #print "rx options ", keys( %$options ), ", open $open \n";
    my $ret = rx_body($grammar, $_[0], { p => $pos+1, args => { open => $open } });
    #print "rx match: ", Dumper($ret->data->{capture} );
    ${ $ret->data->{capture} }->{options} = $options if $ret;
    return $ret;
}

sub rx_body {
    my $grammar = shift;
    use Data::Dumper;
    my $open = $_[1]->{args}{open};
    return $grammar->no_match(@_) unless exists $openmatch{$open};
    my $pos = $_[1]{p} || 0;
    my $s = substr( $_[0], $pos );
    my ($extracted,$remainder) = $open eq $openmatch{$open}
        ? Text::Balanced::extract_delimited( $open . $s, $openmatch{$open} )
        : Text::Balanced::extract_bracketed( $open . $s, $open.$openmatch{$open} );
    #print "rx_body at $s got $extracted\n";
    return $grammar->no_match(@_) unless length($extracted) > 0;
    $extracted = substr( $extracted, 1, -1 );
    return Pugs::Runtime::Match->new( { 
        bool    => \1,
        str     => \$_[0],
        match   => [],
        from    => \$pos,
        to      => \( length($_[0]) - length($remainder) ),
        capture => \{ rx => $extracted },
    } );
};

*ident = Pugs::Compiler::Token->compile( q(
        <[ \? \* \: ]>?     # $?CALLER  $*x  $:x
        [
            [ '::' | <null> ]
            [ _ | <?alpha> ]
            [ _ | <?alnum> ]*
        ]+
) )->code;

*bare_ident = Pugs::Compiler::Token->compile( q(
        [
            [ '::' | <null> ]
            [ _ | <?alpha> ]
            [ _ | <?alnum> ]*
        ]+
) )->code;

*parenthesis = Pugs::Compiler::Token->compile( q^
                <?ws>? $<invocant> := <Pugs::Grammar::Term.parse> <?ws>? \:
                [
                    <?ws> 
                    <Pugs::Grammar::Expression.parse('allow_semicolon', 1)> <?ws>? 
                    ')'
                    { return {
                        op1      => "(",
                        op2      => ")",
                        fixity   => "circumfix",
                        self     => $_[0]{'invocant'}->(),
                        exp1     => $_[0]{'Pugs::Grammar::Expression.parse'}->() 
                    } }
                |
                    <?ws>? 
                    ')'
                    { return {
                        op1      => "(",
                        op2      => ")",
                        fixity   => "circumfix",
                        self     => $_[0]{'invocant'}->(),
                    } }
                ]
            |
                <?ws>? <Pugs::Grammar::Perl6.block> <?ws>? \:  
                <?ws>?
                ')'
                { return {
                    op1      => "(",
                    op2      => ")",
                    fixity   => "circumfix",
                    self     => $_[0]{'Pugs::Grammar::Perl6.block'}->() 
                } }
            |

                <?ws>? <Pugs::Grammar::Expression.parse('allow_semicolon', 1)> <?ws>? 
                ')'
                { return {
                    op1 => "(",
                    op2 => ")",
                    fixity => "circumfix",
                    exp1 => $_[0]{'Pugs::Grammar::Expression.parse'}->() 
                } }
            |
                <?ws>? <Pugs::Grammar::Perl6.block> <?ws>? 
                ')'
                { return {
                    op1 => "(",
                    op2 => ")",
                    fixity => "circumfix",
                    exp1 => $_[0]{'Pugs::Grammar::Perl6.block'}->() 
                } }
            |
                <?ws>? 
                ')'
                { return {
                    op1 => "(",
                    op2 => ")",
                    fixity => "circumfix",
                } }
^ )->code;

*brackets = Pugs::Compiler::Token->compile( q(
                <Pugs::Grammar::Infix.parse> 
                ']'
                { return {
                    op => $_[0]{'Pugs::Grammar::Infix.parse'}->(),
                    reduce => 1, 
                } }
            |
                <?ws>? <Pugs::Grammar::Expression.parse> <?ws>? 
                ']'
                { return {
                    op1 => "[",
                    op2 => "]",
                    fixity => "circumfix",
                    exp1 => $_[0]{'Pugs::Grammar::Expression.parse'}->() 
                } }
            |
                <?ws>? <Pugs::Grammar::Perl6.block> <?ws>? 
                ']'
                { return {
                    op1 => "[",
                    op2 => "]",
                    fixity => "circumfix",
                    exp1 => $_[0]{'Pugs::Grammar::Perl6.block'}->() 
                } }
            |
                <?ws>? 
                ']'
                { return {
                    op1 => "[",
                    op2 => "]",
                    fixity => "circumfix",
                } }
) )->code;

sub is_hash_or_pair {
    # XXX - does %hash, {anon_hash}, hash() interpolate?
    my $elem = $_[0];
    ref( $elem )
    &&    (  exists $elem->{pair}          #  :a<b>  :a  :!a
          || exists $elem->{hash}          #  %hash
          || exists $elem->{anon_hash}     #  {}  { 1 => 2 }
          || (  exists $elem->{fixity}     #  a => 'b'
             && $elem->{fixity} eq 'infix'
             && $elem->{op1} eq '=>'
             )
          || (  exists $elem->{fixity}     #  %( 1, 2 )
             && $elem->{fixity} eq 'prefix'
             && $elem->{op1} eq '%'
             )
          || (  exists $elem->{sub}        #  pair( 1, 2 )  
             && $elem->{sub}{bareword} eq 'pair'
             && $elem->{op1} eq 'call'
             )
          || (  exists $elem->{sub}        #  hash( 1, 2 )
             && $elem->{sub}{bareword} eq 'hash'
             && $elem->{op1} eq 'call'
             )
          )
    ? 1 : 0;
}

sub recompile {
    my $class = shift;
    %hash = (
        '$' => q(
                | <before <[  \{ \[ \< \« ]> >
                  { return { scalar => '$/' ,} }
                | \^ <?Pugs::Grammar::Term.ident>
                  { return { scalar => '$' . $_[0] ,} }
                | <?Pugs::Grammar::Term.ident>
                  { return { scalar => '$' . $_[0] ,} }
                | (\d+)
                  { return {                          
                              'exp1' => {
                                'scalar' => '$/'
                              },
                              'exp2' => {
                                'int' => $/[0]()
                              },
                              'fixity' => 'postcircumfix',
                              'op1' => '[',
                              'op2' => ']',                    
                        },   
                  } 
            ),
        '$.' => q(
                <?Pugs::Grammar::Term.ident>
                { return { scalar => '$.' . $_[0]->() ,} }
            ),
        # XXX - Cheat - @.foo is turned into $.foo
        '@.' => q(
                <?Pugs::Grammar::Term.ident>
                { return { scalar => '$.' . $_[0]->() ,} }
            ),
        '%.' => q(
                <?Pugs::Grammar::Term.ident>
                { return { scalar => '$.' . $_[0]->() ,} }
            ),
        '$/' => q(
                { return { scalar => '$/' ,} } 
            ),
        '$!' => q(
                { return { scalar => '$!' ,} } 
            ),
        '$()' => q(
                { return 
                    {
                      'exp1' => {
                        'pos' => 2,
                        'scalar' => '$/'
                      },
                      'fixity' => 'prefix',
                      'op1' => '$',
                    }
                }
            ),
        '@' => q(
                # XXX t/subroutines/multidimensional_arglists.t
                \; <?Pugs::Grammar::Term.ident>
                { return { die => "not implemented" } }
            |
                <?Pugs::Grammar::Term.ident>
                { return { array => "\@" . $_[0]->() ,} }
            ),
        '::' => q(
                <?Pugs::Grammar::Term.ident>
                { return { type => "\::" . $_[0]->() ,} }
            ),
        '@()' => q(
                { return 
                    {
                      'exp1' => {
                        'pos' => 2,
                        'scalar' => '$/'
                      },
                      'fixity' => 'prefix',
                      'op1' => '@',
                    }
                }
            ),
        '%' => q(
                <?Pugs::Grammar::Term.ident>
                { return { hash  => "\%" . $_[0]->() ,} }
            ),
        '%()' => q(
                { return 
                    {
                      'exp1' => {
                        'pos' => 2,
                        'scalar' => '$/'
                      },
                      'fixity' => 'prefix',
                      'op1' => '%',
                    }
                }
            ),
        '&' => q(
                <?Pugs::Grammar::Term.ident>
                { return { code  => "\&" . $_[0]->() ,} }
            ),
        '(' => q(
                <Pugs::Grammar::Term.parenthesis>
                { return $_[0]{'Pugs::Grammar::Term.parenthesis'}->() }
            ),
        '[' => q(
                <Pugs::Grammar::Term.brackets>
                { return $_[0]{'Pugs::Grammar::Term.brackets'}->() }
            ),
        '{' => q(
                # S06 - Anonymous hashes vs blocks
                # if it is completely empty 
                <?ws>? '}'
                { 
                  return { 
                    anon_hash => { null => 1, },
                } }
            |
                # consists of a single list, first element is either a hash or a pair
                <?ws>? <Pugs::Grammar::Perl6.statements> <?ws>? '}'
                { 
                    #print "Term block\n";
                    my $stmt = $_[0]{'Pugs::Grammar::Perl6.statements'}->();
                    
                    #print "Statements: ", Dumper($stmt);
                    
                    if ( scalar @{$stmt->{statements}} == 1 ) {
                        my $list = $stmt->{statements}[0];
                        if (  exists $list->{list} 
                           && $list->{op1} eq ','
                           ) {
                            my $elem = $list->{list}[0];
                            if ( Pugs::Grammar::Term::is_hash_or_pair( $elem ) ) {
                                return { 
                                    anon_hash => $list,
                                }                                
                            }
                        }
                        if ( Pugs::Grammar::Term::is_hash_or_pair( $list ) ) {
                            return { 
                                anon_hash => {
                                    list => [ $list ],
                                    assoc => 'list',
                                    op1 => ',',
                                }
                            }                                
                        }
                    }
                    
                    return { 
                        bare_block => $stmt,
                } }
            ),
        '->' => q( 
        [
            <?ws>? <Pugs::Grammar::Perl6.signature_no_invocant> <?ws>? 
            \{ <?ws>? <Pugs::Grammar::Perl6.statements> <?ws>? \}
            { return { 
                pointy_block => $_[0]{'Pugs::Grammar::Perl6.statements'}->(),
                signature    => $_[0]{'Pugs::Grammar::Perl6.signature_no_invocant'}->(),
            } }
        |
            <?ws>?
            \{ <?ws>? <Pugs::Grammar::Perl6.statements> <?ws>? \}
            { return { 
                pointy_block => $_[0]{'Pugs::Grammar::Perl6.statements'}->(),
                signature    => undef,
            } }
        ]
            ),
        '.' => q(
                # .method op
                <?Pugs::Grammar::Term.ident>
                { return { dot_bareword  => $_[0]->() ,} }
            ),
        '...' => q(
            { return { term => "yada" } }
            ),
        '???' => q(
            { return { term => "???" } }
            ),
        '!!!' => q(
            { return { term => "!!!" } }
            ),
        'Inf' => q(
            { return { num => "Inf" } }
            ),
        'NaN' => q(
            { return { num => "NaN" } }
            ),
        'self' => q(
            { return { term => "self" } }
            ),
        'undef' => q(
            { return { term => "undef" } }
            ),
        'my' => q(
            <?ws> <Pugs::Grammar::Perl6.signature_term_type>
            <?ws>? <Pugs::Grammar::Term.parse>
            <?ws>? <Pugs::Grammar::Perl6.attribute>
            { 
                return { 
                    exp1 => $/{'Pugs::Grammar::Term.parse'}->(),
                    attribute  => $/{'Pugs::Grammar::Perl6.attribute'}->(),
                    variable_declarator => "my",
                    type  => $/{'Pugs::Grammar::Perl6.signature_term_type'}->(),
                } 
            }
            ),
        'our' => q(
            <?ws> <Pugs::Grammar::Perl6.signature_term_type>
            <?ws>? <Pugs::Grammar::Term.parse>
            <?ws>? <Pugs::Grammar::Perl6.attribute>
            { 
                return { 
                    exp1 => $/{'Pugs::Grammar::Term.parse'}->(),
                    attribute  => $/{'Pugs::Grammar::Perl6.attribute'}->(),
                    variable_declarator => "our",
                    type  => $/{'Pugs::Grammar::Perl6.signature_term_type'}->(),
                } 
            }
            ),
        'has' => q(
            <?ws> <Pugs::Grammar::Perl6.signature_term_type>
            <?ws>? <Pugs::Grammar::Term.parse>
            <?ws>? <Pugs::Grammar::Perl6.attribute>
            { 
                return { 
                    exp1 => $/{'Pugs::Grammar::Term.parse'}->(),
                    attribute  => $/{'Pugs::Grammar::Perl6.attribute'}->(),
                    variable_declarator => "has",
                    type  => $/{'Pugs::Grammar::Perl6.signature_term_type'}->(),
                } 
            }
            ),
        'state' => q(
            <?ws> <Pugs::Grammar::Perl6.signature_term_type>
            <?ws>? <Pugs::Grammar::Term.parse>
            <?ws>? <Pugs::Grammar::Perl6.attribute>
            { 
                return { 
                    exp1 => $/{'Pugs::Grammar::Term.parse'}->(),
                    attribute  => $/{'Pugs::Grammar::Perl6.attribute'}->(),
                    variable_declarator => "state",
                    type  => $/{'Pugs::Grammar::Perl6.signature_term_type'}->(),
                } 
            }
            ),
        'constant' => q(
            <?ws> <Pugs::Grammar::Perl6.signature_term_type>
            <?ws>? <Pugs::Grammar::Term.parse>
            <?ws>? <Pugs::Grammar::Perl6.attribute>
            { 
                return { 
                    exp1 => $/{'Pugs::Grammar::Term.parse'}->(),
                    attribute  => $/{'Pugs::Grammar::Perl6.attribute'}->(),
                    variable_declarator => "constant",
                    type  => $/{'Pugs::Grammar::Perl6.signature_term_type'}->(),
                } 
            }
            ),
        q(s) => q(
            <Pugs::Grammar::Term.substitution>
            { return { 
                    substitution => $/{'Pugs::Grammar::Term.substitution'}->(),
                } 
            }
            ),
        q(rx) => q(
            <Pugs::Grammar::Term.rx>
            { return { 
                    rx => $/{'Pugs::Grammar::Term.rx'}->(),
                } 
            }
            ),
        q(m) => q(
            <Pugs::Grammar::Term.rx>
            { return { 
                    rx => $/{'Pugs::Grammar::Term.rx'}->(),
                } 
            }
            ),
        q(/) => q(
            <Pugs::Grammar::Term.rx_body('open','/')>
            { return { 
                    rx => $/{'Pugs::Grammar::Term.rx_body'}->(),
                } 
            }
            ),
        q(perl5:) => q(
                ### perl5:Test::More
                <Pugs::Grammar::Term.bare_ident> 
                { return { 
                        bareword => $/{'Pugs::Grammar::Term.bare_ident'}->(),
                        lang => 'perl5',
                } }
            ),
        q(use) => q(
                # "use v5"
                <?ws> v5 <?ws>?; <perl5source> 
                { return $_[0]{perl5source}->() 
                }
            |
                # default
                <?ws> 
                { return { bareword => 'use' } }
            ),
        q(do) =>  q( 
                # { print "statement do \n"; }
                <?ws> 
                $<exp1> := <Pugs::Grammar::Perl6.statement>        
                { return { 
                        statement => 'do',
                        exp1 => $_[0]{exp1}->(),
                } }
            ),
        q(:) => Pugs::Compiler::Token->compile( q^
            ### pair - long:<name> 
                # :foo<bar>
                ([_|\w]+) \< <Pugs::Grammar::Quote.angle_quoted>
                { return {
                    pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value => $/{'Pugs::Grammar::Quote.angle_quoted'}->(), 
                } } }
            |
                # :foo(exp)
                ([_|\w]+) \(  
                    <?ws>? <Pugs::Grammar::Expression.parse> <?ws>? 
                \)
                { 
                    return {
                      pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value => $/{'Pugs::Grammar::Expression.parse'}->(), 
                } } }
            |
                # :$foo 
                \$ ((_|\w)+)
                { return {
                    pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value => { scalar  => '$' . $/[0]() }, 
                } } }
            |
                # :$<foo>
                '$<' ((_|\w)+) \>
                { return {
                    pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value => {                         
                              'exp1' => {
                                'scalar' => '$/'
                              },
                              'exp2' => {
                                'angle_quoted' => $/[0]()
                              },
                              'fixity' => 'postcircumfix',
                              'op1' => '<',
                              'op2' => '>',                    
                        },                        
                } } }
            |
                # :$$<foo>
                '$$<' ((_|\w)+) \>
                { return {
                    pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value =>     
                          {
                          'exp1' => {
                            'exp1' => {
                              'scalar' => '$/'
                            },
                            'exp2' => {
                              'angle_quoted' => $/[0]()
                            },
                            'fixity' => 'postcircumfix',
                            'op1' => '<',
                            'op2' => '>',
                          },
                          'fixity' => 'prefix',
                          'op1' => '$',
                        },                        
                } } }
            |
                # :foo 
                ((_|\w)+)
                { return {
                    pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value => { num => 1 }, 
                } } }
            |
                # :!foo 
                '!' ((_|\w)+)
                { return {
                    pair => { 
                        key   => { single_quoted => $/[0]() }, 
                        value => { num => 0 }, 
                } } }
            ^ ),
        q(q) => Pugs::Compiler::Token->compile( q^
                <Pugs::Grammar::Quote.q>
                { return $/{'Pugs::Grammar::Quote.q'}->() }
            ^ ),
        q() => Pugs::Compiler::Token->compile( q^
                ### num/int
                \d+ 
                [
                    \.\d+
                    [ <[Ee]> <[+-]>? \d+ ]?
                    { return { num => $() ,} } 
                |
                    <[Ee]> <[+-]>? \d+ 
                    { return { num => $() ,} } 
                |
                    { return { int => $() ,} } 
                ]
            |
                <Pugs::Grammar::Perl6.sub_decl>
                    { return $_[0]{'Pugs::Grammar::Perl6.sub_decl'}->();
                    }
            |
                <Pugs::Grammar::Perl6.proto_rule_decl>
                    { return $_[0]{'Pugs::Grammar::Perl6.proto_rule_decl'}->();
                    }
            |
                <Pugs::Grammar::Perl6.rule_decl>
                    { return $_[0]{'Pugs::Grammar::Perl6.rule_decl'}->();
                    }
            |
                <Pugs::Grammar::Perl6.class_decl>
                    { return $_[0]{'Pugs::Grammar::Perl6.class_decl'}->();
                    }
            |
                ### Test-0.0.6
                <Pugs::Grammar::Term.cpan_bareword> 
                { return { cpan_bareword => $/{'Pugs::Grammar::Term.cpan_bareword'}->() } }
            |
                ### Test => ... - autoquote before '=>'
                <Pugs::Grammar::Term.bare_ident> 
                [
                    <before <?ws>? \=\> >
                    { return { single_quoted => $/{'Pugs::Grammar::Term.bare_ident'}->() } } 
                
                |
                ### Test::More
                { return { bareword => $/{'Pugs::Grammar::Term.bare_ident'}->() } }
                ]
            ^ ),
        );
        
    for my $trait ( qw(
       BEGIN 
     | CHECK 
     | INIT 
     | START
     | FIRST
     | ENTER
    ) ) {
    __PACKAGE__->add_rule(
        $trait =>  qq( 
        <?ws>? <Pugs::Grammar::Perl6.block>        
            { return { 
                trait  => '$trait',
                \%{ \$_[0]{'Pugs::Grammar::Perl6.block'}->() },
            } }
        ) );
    }
    
    $class->SUPER::recompile;
}

BEGIN {
    __PACKAGE__->recompile;
}

1;
