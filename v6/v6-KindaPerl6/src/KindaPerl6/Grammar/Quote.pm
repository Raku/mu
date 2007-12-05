
use v6-alpha;

grammar KindaPerl6::Grammar {

token double_quoted {
    |  <!before \\ | \' | \" | \$ | \@ | \% > . <double_quoted>
    |  ''
};

token single_quoted {
    |  <!before \\ | \' > . <single_quoted>
    |  ''
};

token quoted_any { . }

token quoted_array {
    <before \@ > <var> \[ <.opt_ws> \]
        {
            return ::Apply(
                'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'prefix:<~>', namespace => [ ] ),
                'arguments' => [ $$<var> ],
            );
        }
}

token quoted_hash {
    <before \% > <var> \{ <.opt_ws> \}
        {
            return ::Apply(
                'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'prefix:<~>', namespace => [ ] ),
                'arguments' => [ $$<var> ],
            );
        }
}

token quoted_scalar {
    <before \$ > <var>
        {
            return ::Apply(
                'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'prefix:<~>', namespace => [ ] ),
                'arguments' => [ $$<var> ],
            );
        }
}

token quoted_exp {
    |  <quoted_array>  { return $$<quoted_array>  }
    |  <quoted_hash>   { return $$<quoted_hash>   }
    |  <quoted_scalar> { return $$<quoted_scalar> }
    |  \' { return ::Val::Char( char => 39 ) }
    |  \\
        [  # see S02
        |   a  { return ::Val::Char( char =>  7 ) }
        |   b  { return ::Val::Char( char =>  8 ) }
        |   t  { return ::Val::Char( char =>  9 ) }
        |   n  { return ::Val::Char( char => 10 ) }
        |   f  { return ::Val::Char( char => 12 ) }
        |   r  { return ::Val::Char( char => 13 ) }
        |   e  { return ::Val::Char( char => 27 ) }
        |   \" { return ::Val::Char( char => 34 ) }
        |   \' { return ::Val::Char( char => 39 ) }
        |   \\ { return ::Val::Char( char => 92 ) }
        |   <quoted_any> { return ::Val::Buf( 'buf' => $$<quoted_any> ) }
        ]
    |  [ \$ | \@ | \% | '' ] <double_quoted> { return ::Val::Buf( 'buf' => ~$/ ) }
}

token single_quoted_exp {
    |   \\  \'   { return ::Val::Char( char => 39 ) }
    |   \\  \\   { return ::Val::Char( char => 92 ) }
    |   \\       { return ::Val::Char( char => 92 ) }
    |  <single_quoted> { return ::Val::Buf( 'buf' => ~$/ ) }
}

token quoted_exp_seq {
    <quoted_exp>
    [
    |  <before \" >     { return $$<quoted_exp>;}
    |
        <quoted_exp_seq>
        {
            return ::Apply(
                'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'infix:<~>', namespace => [ ] ),
                'arguments' => [ $$<quoted_exp>, $$<quoted_exp_seq> ],
            );
        }
    ]
}

token single_quoted_exp_seq {
    <single_quoted_exp>
    [
    |  <before \' >     { return $$<single_quoted_exp>;}
    |
        <single_quoted_exp_seq>
        {
            return ::Apply(
                'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'infix:<~>', namespace => [ ] ),
                'arguments' => [ $$<single_quoted_exp>, $$<single_quoted_exp_seq> ],
            );
        }
    ]
}

token angle_quoted {
    |  \\ .  <angle_quoted>
    |  <!before \> > . <angle_quoted>
    |  ''
};

token french_quoted {
    |  \\ .  <french_quoted>
    |  <!before \» > . <french_quoted>
    |  ''
};

token val_buf {
    | \" <quoted_exp_seq> \" { return $$<quoted_exp_seq> }
    | \' <single_quoted_exp_seq> \' { return $$<single_quoted_exp_seq> }
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
