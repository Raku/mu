
use v6-alpha;

grammar KindaPerl6::Grammar {

    sub declare_parameters( $env, $vars ) {
        # declare the variables in the signature as 'my'
        # TODO - declare the named parameters
        #say "#declaring parameters";

        #my $vars := [
        #        ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
        #        $sig.invocant,
        #        @($sig.positional).>>key,
        #    ];

        my $decl;
        my $var;
        for @($vars) -> $var {
            if $var.isa( 'Var' ) {
                #say "#var ", $var.name;
                push @($decl),
                    ::Decl(
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
            { return [ ::Val::Buf( 'buf' => ~$<ident> ), $$<exp> ] }
        |   \: <ident> \< <angle_quoted> \>     #  :key<value>
            {
                return [
                    ::Val::Buf( 'buf' => ~$<ident> ),
                    ::Val::Buf( 'buf' => ~$<angle_quoted> ) ]
            }
        |   \: <ident> \( <.opt_ws> <exp> <.opt_ws> \)   #  :key(value)
            {
                return [
                    ::Val::Buf( 'buf' => ~$<ident> ),
                    $$<exp> ]
            }
        |   \: <ident>                          #  :key
            {
                return [
                    ::Val::Buf( 'buf' => ~$<ident> ),
                    ::Val::Bit( 'bit' => 1 ) ]
            }
        |   \: <sigil> <ident>                  #  :$var
            {
                return [
                    ::Val::Buf( 'buf' => ~$<ident> ),
                    ::Var( 'sigil' => ~$$<sigil>, 'twigil' => '', 'name' => $$<ident>, namespace => [ ] ) ]
            }
    };


    token exp_parameter_item {
        |   <exp_parameter_named>
            { return ::Lit::NamedArgument(
                    key           => ($$<exp_parameter_named>)[0],
                    value         => ($$<exp_parameter_named>)[1],
                ) }
        |   <pair>  { return ::Lit::Pair( key => ($$<pair>)[0], value => ($$<pair>)[1] ) }
        |   <exp>   { return $$<exp>  }
    }

    token exp_parameter_list {
        |   <exp_parameter_item>
            [
            |   <.opt_ws> \, <.opt_ws> <exp_parameter_list>
                { return [ $$<exp_parameter_item>, @( $$<exp_parameter_list> ) ] }
            |   <.opt_ws> [ \, <.opt_ws> | '' ]
                { return [ $$<exp_parameter_item> ] }
            ]
        |
            { return [ ] }
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
