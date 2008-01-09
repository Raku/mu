
use v6-alpha;

grammar KindaPerl6::Grammar {

    sub declare_parameters( $env, $vars ) {
        # declare the variables in the signature as 'my'
        # TODO - declare the named parameters
        #say "#declaring parameters";

        #my $vars := [
        #        Var.new( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
        #        $sig.invocant,
        #        @($sig.positional).>>key,
        #    ];

        my $decl;
        my $var;
        for @($vars) -> $var {
            if $var.isa( 'Var' ) {
                #say "#var ", $var.name;
                push @($decl),
                    Decl.new(
                        decl  => 'my',
                        var   => $var,
                        type  => '', # TODO
                    );
            }
        };
        $env.add_lexicals( $decl );
    }

    token exp_parameter_named {
        |   <ident>                             #  key => value
            <.opt_ws> '=>' <.opt_ws>
            <exp>
            { make [ Val::Buf.new( 'buf' => ~$<ident> ), $$<exp> ] }
        |   \: <ident> \< <angle_quoted> \>     #  :key<value>
            {
                make [
                    Val::Buf.new( 'buf' => ~$<ident> ),
                    Val::Buf.new( 'buf' => ~$<angle_quoted> ) ]
            }
        |   \: <ident> \( <.opt_ws> <exp> <.opt_ws> \)   #  :key(value)
            {
                make [
                    Val::Buf.new( 'buf' => ~$<ident> ),
                    $$<exp> ]
            }
        |   \: <ident>                          #  :key
            {
                make [
                    Val::Buf.new( 'buf' => ~$<ident> ),
                    Val::Bit.new( 'bit' => 1 ) ]
            }
        |   \: <sigil> <ident>                  #  :$var
            {
                make [
                    Val::Buf.new( 'buf' => ~$<ident> ),
                    Var.new( 'sigil' => ~$$<sigil>, 'twigil' => '', 'name' => $$<ident>, namespace => [ ] ) ]
            }
    };


    token exp_parameter_item {
        |   <exp_parameter_named>
            { make Lit::NamedArgument.new(
                    key           => ($$<exp_parameter_named>)[0],
                    value         => ($$<exp_parameter_named>)[1],
                ) }
        |   <pair>  { make Lit::Pair.new( key => ($$<pair>)[0], value => ($$<pair>)[1] ) }
        |   <exp>   { make $$<exp>  }
    }

    token exp_parameter_list {
        |   <exp_parameter_item>
            [
            |   <.opt_ws> \, <.opt_ws> <exp_parameter_list>
                { make [ $$<exp_parameter_item>, ( $$<exp_parameter_list> ).values ] }
            |   <.opt_ws> [ \, <.opt_ws> | '' ]
                { make [ $$<exp_parameter_item> ] }
            ]
        |
            { make [ ] }
    };



}

=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
