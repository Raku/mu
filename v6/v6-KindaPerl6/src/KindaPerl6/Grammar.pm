use v6-alpha;

grammar KindaPerl6::Grammar {

use KindaPerl6::Grammar::Regex;
use KindaPerl6::Grammar::Mapping;
use KindaPerl6::Grammar::Control;
use KindaPerl6::Grammar::Parameters;
use KindaPerl6::Grammar::Signature;
use KindaPerl6::Grammar::Term;
use KindaPerl6::Grammar::Statements;
use KindaPerl6::Grammar::Quote;
use KindaPerl6::Grammar::Sub;
use KindaPerl6::Grammar::Token;
use KindaPerl6::Grammar::Expression;

my $Class_name;  # for diagnostic messages
sub get_class_name { $Class_name }; 

token ident_digit {
    [ [ <.word> | _ | <.digit> ] <.ident_digit>
    |   ''
    ]    
};

token ident {
    | [ <!before \d> <.word> | _ ] <.ident_digit>
        [ ':<'   <angle_quoted> '>' | '' ]
    | ¢
};

token full_ident {
    <.ident>
    [   '::' <.full_ident>
    |   ''
    ]    
};

token namespace {
    |   <ident> '::'
        [
        |   <namespace> 
            { make [ $$<ident>, ( $$<namespace> ).values ] }
        |   
            { make [ $$<ident> ] }
        ]
    |
        { make [ ] }
};

token to_line_end {
    |  \N <.to_line_end>
    |  ''
};

token pod_begin {
    |   \n '=end' <.to_line_end>
    |   . <.to_line_end> <.pod_begin>
};

token pod_other {
    |   \n '=cut' <.to_line_end>   # XXX is "cut" deprecated?
    |   \n '=end' <.to_line_end>
    |   . <.to_line_end> <.pod_other>
};

token ws {
    [
    |    '#' <.to_line_end>
    |    \n [
            |  '=begin'  <.pod_begin>
            |  '=kwid'   <.pod_other>
            |  '=pod'    <.pod_other>
            |  '=for'    <.pod_other>
            |  '=head1'  <.pod_other>
            |  ''
            ]
    |    \s
    ]
    [ <.ws> | '' ]
};

token opt_ws  {  <.ws> | ''  };
token opt_ws2 {  <.ws> | ''  };
token opt_ws3 {  <.ws> | ''  };

token dot {
    | \.
    | \\ <.opt_ws> \.
}

token parse {
    | <comp_unit>
        [
        |   <parse>
            { make [ $$<comp_unit>, ( $$<parse> ).values ] }
        |   { make [ $$<comp_unit> ] }
        ]
    | { make [] }
};

token unit_type {
    'class' | 'grammar' | 'role' | 'module'
};

token trait_auxiliary {
    is | does | meta
};

token class_trait {
    <trait_auxiliary> <.ws> <full_ident> 
        { make [ $$<trait_auxiliary>, $$<full_ident> ] }
};

token class_traits {
    | <class_trait>
        [
        |   <.ws> <class_traits>
            { make [ $$<class_trait>, ( $$<class_traits> ).values ] }
        |   { make [ $$<class_trait> ] }
        ]
    | { make [] }
};

token comp_unit {
    <.opt_ws> [\; <.opt_ws> | '' ]
    [ 'use' <.ws> 'v6-' <ident> <.opt_ws> \; <.ws>  |  '' ]
    
    [
    <unit_type> <.opt_ws> <full_ident> <.opt_ws>
    <class_traits> <.opt_ws>
    '{'
        { $Class_name := ~$<full_ident> }
        <.opt_ws>
        { 
            COMPILER::add_pad( $Class_name );
        }
        <exp_stmts>
        <.opt_ws>
    '}'
    <.opt_ws> [\; <.opt_ws> | '' ]
    {
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        make CompUnit.new(
            'unit_type'   => $$<unit_type>,
            'name'        => $$<full_ident>,
            'traits'      => $$<class_traits>,
            'attributes'  => { },
            'methods'     => { },
            'body'        => Lit::Code.new(
                pad   => $env,
                state => { },
                sig   => Sig.new( 'invocant' => undef, 'positional' => [ ] ),
                body  => $$<exp_stmts>,
            ),
        )
    }
    ] | [
    <.opt_ws>
    {
        $Class_name := 'Main';
        COMPILER::add_pad( $Class_name );
    }
    <exp_stmts2>
    {
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        make CompUnit.new(
            'unit_type'   => 'module',
            'name'        => 'Main',
            'traits'      => [],
            'attributes'  => { },
            'methods'     => { },
            'body'        => Lit::Code.new(
                pad   => $env,
                state => { },
                sig   => Sig.new( 'invocant' => undef, 'positional' => [ ] ),
                body  => $$<exp_stmts2>,
            ),
        )
    }
    ]
};

token declarator {
     'my' | 'state' | 'has' | 'our'
};
token opt_declarator {
    <declarator> <.ws> { make $$<declarator>;} | { make '';}
};

token opt_ident {  
    | <ident>  { make $$<ident> }
    | ''     { make 'postcircumfix:<( )>' }
};

token sub_or_method_name {
    <full_ident> [ <.dot> <ident> | '' ]
};

token opt_type {
    |   [ '::' | '' ]  <full_ident>   { make $$<full_ident> }
    |   ''                            { make '' }
};

token use_from_perl5 {
    ':from<perl5>' { make 1 } | { make 0 }
}

token sigil { \$ |\% |\@ |\& };

token twigil { [ \. | \! | \^ | \* ] | '' };

# used in Term.pm
token undeclared_var {
    <sigil> <twigil> <namespace> <ident>
    {
        # no pre-declaration checks
        make Var.new(
            sigil     => ~$<sigil>,
            twigil    => ~$<twigil>,
            name      => ~$<ident>,
            namespace => $$<namespace>,
        )
    }
};

token var {
    <sigil> '/'
    {
        make 
            Var.new(
                    sigil     => ~$<sigil>,
                    twigil    => '',
                    name      => '/',
                    namespace => [ ],
                )
    }
  |
    <sigil> <twigil> <namespace> <ident>
    {
        # check for pre-declaration
        make COMPILER::get_var(
            Var.new(
                    sigil     => ~$<sigil>,
                    twigil    => ~$<twigil>,
                    name      => ~$<ident>,
                    namespace => $$<namespace>,
                )
        )
    }
};

token val {
    | <val_undef>  { make $$<val_undef> }  # undef
    # | $<exp> := <val_object>   # (not exposed to the outside)
    | <val_int>    { make $$<val_int>   }  # 123
    | <val_bit>    { make $$<val_bit>   }  # True, False
    | <val_num>    { make $$<val_num>   }  # 123.456
    | <val_buf>    { make $$<val_buf>   }  # 'moose'
};

token val_bit {
    | True  { make Val::Bit.new( 'bit' => 1 ) }
    | False { make Val::Bit.new( 'bit' => 0 ) }
};




token val_undef {
    undef <!before \w >
    { make Val::Undef.new( ) }
};

token val_num {  
    XXX { make 'TODO: val_num' } 
};


token digits {  \d  [ <digits> | '' ]  };


token val_int {
    <digits>
    { make Val::Int.new( 'int' => ~$/ ) }
};


# XXX obsolete?
token exp_seq {
    | <exp>
        # { say 'exp_seq: matched <exp>' }
        [
        |   <.opt_ws> \, <.opt_ws> <exp_seq> 
            <.opt_ws> [ \, <.opt_ws> | '' ]
            { make [ $$<exp>, ( $$<exp_seq> ).values ] }
        |   <.opt_ws> [ \, <.opt_ws> | '' ]
            { make [ $$<exp> ] }
        ]
    | 
        # { say 'exp_seq: end of match' }
        { make [] }
};

token apply {
    <namespace> <ident>
    [
        [ \( <.opt_ws> <exp_parameter_list> <.opt_ws> \)
        | <.ws> <exp_parameter_list> <.opt_ws>
        ]
        {
            make Apply.new(
                'code'      => COMPILER::get_var( 
                    Var.new(
                            sigil     => '&',
                            twigil    => '',
                            name      => $$<ident>,
                            namespace => $$<namespace>,
                        ) ),
                'arguments' => $$<exp_parameter_list>,
            )
        }
    |
        {
            make Apply.new(
                'code'      => COMPILER::get_var( 
                    Var.new(
                            sigil     => '&',
                            twigil    => '',
                            name      => $$<ident>,
                            namespace => $$<namespace>,
                        ) ),
                'arguments' => [],
            )
        }
    ]
};

token opt_name {  <ident> | ''  };


token invocant {
    |  <var> \:    { make $$<var> }
    |  { make undef }
};

token capture {
    # TODO - exp_seq / exp_mapping == positional / named 
    |  <exp>\:  <.opt_ws> <exp_parameter_list> 
        { make Lit::Capture.new( 'invocant' => $$<exp>, 'array' => $$<exp_parameter_list>, 'hash' => [ ] ); }
    |  <exp_mapping> 
        { make Lit::Capture.new( 'invocant' => undef, 'array' => [ ], 'hash' => $$<exp_mapping> ); }
};

token base_class { <full_ident> }

token subset {
    # example:  subset Not_x of Str where { $_ ne 'x' }
    subset  <.ws> 
    <full_ident> <.ws> 
    of      <.ws>
    <base_class> <.ws> 
    where   
    <.opt_ws> \{ <.opt_ws>  
        # { say ' parsing statement list ' }
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <.opt_ws> 
    [   \}     | { say '*** Syntax Error in subset \'', get_class_name(), '.', $$<name>, '\' near pos=', $/.to; die 'error in Block'; } ]
    {
        # say ' block: ', ($$<exp_stmts>).perl;
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        make Lit::Subset.new( 
            'name'  => $$<full_ident>, 
            'base_class' => 
                Proto.new( name => $$<base_class> ), 
            'block' => 
                Sub.new( 
                    'name'  => undef, 
                    'block' => Lit::Code.new(
                        pad   => $env,
                        state => { },
                        sig   => Sig.new( 'invocant' => undef, 'positional' => [ ] ),
                        body  => $$<exp_stmts>,
                ),
            ),
        );
    }
}


token begin_block {
    BEGIN
    <.opt_ws> \{ <.opt_ws>  

        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <.opt_ws> 
    [   \}     | { say '*** Syntax Error in BEGIN near pos=', $/.to; die 'error in Block'; } ]
    {
        # say ' block: ', ($$<exp_stmts>).perl;
        my $env := COMPILER::current_pad();
        #print "  grammar: dropping pad\n";
        COMPILER::drop_pad();
        #say "BEGIN block";
        #print "  grammar: entering begin block\n";
        make COMPILER::begin_block( 
            # $env, 
            Lit::Code.new(
                pad   => $env,
                state => { },
                sig   => Sig.new( 'invocant' => undef, 'positional' => [ ] ),
                body  => $$<exp_stmts>,
            ),
        );
    }
};

token check_block {
    CHECK
    <.opt_ws> \{ <.opt_ws>  
          <exp_stmts> <.opt_ws> 
    [   \}     | { say '*** Syntax Error in CHECK block'; die 'error in Block'; } ]
    { 
        #say "CHECK block";
        make COMPILER::check_block( $$<exp_stmts> );
    }
};

}

=begin

=head1 NAME 

KindaPerl6::Grammar - Grammar for KindaPerl6

=head1 SYNOPSIS

    my $match := $source.parse;
    ($$match).perl;    # generated KindaPerl6 AST

=head1 DESCRIPTION

This module generates a syntax tree for the KindaPerl6 compiler.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2007 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
