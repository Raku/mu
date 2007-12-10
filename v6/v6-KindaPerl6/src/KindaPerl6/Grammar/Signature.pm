
use v6-alpha;

grammar KindaPerl6::Grammar {

    # has $.is_longname; ???
    token sig_type {
        |   [ '::' | '' ]  <full_ident> <.ws>  { make $$<full_ident> }
        |   ''                                 { make '' }
    }
    token sig_default_value {
        |   <.opt_ws> '=' <.opt_ws> <exp> <.opt_ws> { make { has_default => 1, default => $$<exp>, } }
        |   ''                                      { make { has_default => 0, default => ::Val::Undef( ), } }
    }
    token sig_named_only       { ':' { make 1 } | { make 0 } }
    token sig_optional         {
        |   '?'            { make 1 }
        |   [ '!' | '' ]   { make 0 }
    }
    token sig_slurpy           { '*' { make 1 } | { make 0 } }
    token sig_multidimensional { '@' { make 1 } | { make 0 } }
    token sig_rw               { <.ws> 'is' <.ws> 'rw'   { make 1 } | { make 0 } }
    token sig_copy             { <.ws> 'is' <.ws> 'copy' { make 1 } | { make 0 } }

    token exp_sig_item {
            <sig_type>
            <sig_named_only> <sig_slurpy> <sig_multidimensional>

            <sigil> <ident>

            <sig_optional>

            <sig_default_value>
            <sig_rw> <sig_copy>   # XXX no order !!!

            { make ::Lit::SigArgument(
                    key           => ::Var(
                            sigil     => ~$<sigil>,
                            twigil    => '',
                            name      => ~$<ident>,
                            namespace => [],
                        ),
                    value         => ($$<sig_default_value>){'default'},
                    type          => $$<sig_type>,
                    has_default   => ::Val::Bit( bit => ($$<sig_default_value>){'has_default'} ),
                    is_named_only => ::Val::Bit( bit => $$<sig_named_only>  ),
                    is_optional   => ::Val::Bit( bit => $$<sig_optional>    ),
                    is_slurpy     => ::Val::Bit( bit => $$<sig_slurpy>      ),
                    is_multidimensional =>
                                     ::Val::Bit( bit => $$<sig_multidimensional> ),
                    is_rw         => ::Val::Bit( bit => $$<sig_rw>          ),
                    is_copy       => ::Val::Bit( bit => $$<sig_copy>        ),
                ) }
    }

    token exp_sig_list {
        |   <exp_sig_item>
            [
            |   <.opt_ws> \, <.opt_ws> <exp_sig_list>
                { make [ $$<exp_sig_item>, ( $$<exp_sig_list> ).values ] }
            |   <.opt_ws> [ \, <.opt_ws> | '' ]
                { make [ $$<exp_sig_item> ] }
            ]
        |
            { make [ ] }
    };

    token sig {
        <invocant>
        <.opt_ws>
        <exp_sig_list>
        {
            # say ' invocant: ', ($$<invocant>).perl;
            # say ' positional: ', ($$<exp_seq>).perl;
            make ::Sig( 'invocant' => $$<invocant>, 'positional' => $$<exp_sig_list>, );
        }
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
