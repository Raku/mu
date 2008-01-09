
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
            make Apply.new(
                'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'prefix:<~>', namespace => [ ] ),
                'arguments' => [ $$<var> ],
            );
        }
}

token quoted_hash {
    <before \% > <var> \{ <.opt_ws> \}
        {
            make Apply.new(
                'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'prefix:<~>', namespace => [ ] ),
                'arguments' => [ $$<var> ],
            );
        }
}

token quoted_scalar {
    <before \$ > <var>
        {
            make Apply.new(
                'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'prefix:<~>', namespace => [ ] ),
                'arguments' => [ $$<var> ],
            );
        }
}

token quoted_exp {
    |  <quoted_array>  { make $$<quoted_array>  }
    |  <quoted_hash>   { make $$<quoted_hash>   }
    |  <quoted_scalar> { make $$<quoted_scalar> }
    |  \' { make Val::Char.new( char => 39 ) }
    |  \\
        [  # see S02
        |   a  { make Val::Char.new( char =>  7 ) }
        |   b  { make Val::Char.new( char =>  8 ) }
        |   t  { make Val::Char.new( char =>  9 ) }
        |   n  { make Val::Char.new( char => 10 ) }
        |   f  { make Val::Char.new( char => 12 ) }
        |   r  { make Val::Char.new( char => 13 ) }
        |   e  { make Val::Char.new( char => 27 ) }
        |   \" { make Val::Char.new( char => 34 ) }
        |   \' { make Val::Char.new( char => 39 ) }
        |   \\ { make Val::Char.new( char => 92 ) }
        |   <quoted_any> { make Val::Buf.new( 'buf' => $$<quoted_any> ) }
        ]
    |  [ \$ | \@ | \% | '' ] <double_quoted> { make Val::Buf.new( 'buf' => ~$/ ) }
}

token single_quoted_exp {
    |   \\  \'   { make Val::Char.new( char => 39 ) }
    |   \\  \\   { make Val::Char.new( char => 92 ) }
    |   \\       { make Val::Char.new( char => 92 ) }
    |  <single_quoted> { make Val::Buf.new( 'buf' => ~$/ ) }
}

token quoted_exp_seq {
    <quoted_exp>
    [
    |  <before \" >     { make $$<quoted_exp>;}
    |
        <quoted_exp_seq>
        {
            make Apply.new(
                'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'infix:<~>', namespace => [ ] ),
                'arguments' => [ $$<quoted_exp>, $$<quoted_exp_seq> ],
            );
        }
    ]
}

token single_quoted_exp_seq {
    <single_quoted_exp>
    [
    |  <before \' >     { make $$<single_quoted_exp>;}
    |
        <single_quoted_exp_seq>
        {
            make Apply.new(
                'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'infix:<~>', namespace => [ ] ),
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
    | \" <quoted_exp_seq> \" { make $$<quoted_exp_seq> }
    | \' <single_quoted_exp_seq> \' { make $$<single_quoted_exp_seq> }
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
