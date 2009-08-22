
use v6-alpha;

grammar MiniPerl6::Grammar {

token key { 
    |  <ident> <before <'=>'> | <.ws> > 
       { make ::Val::Buf( 'buf' => ~$<ident> ) }  # autoquote
    |  <exp>   
       { make $$<exp> } 
};

token pair {
    |   <key> 
        <.opt_ws> <'=>'> <.opt_ws>
        <exp>
        { make [ $$<key>, $$<exp> ] }
    |   \: <sigil> <ident>                  #  :$var
        { 
            make [ 
                ::Val::Buf( 'buf' => ~$<ident> ), 
                ::Var( 'sigil' => ~$$<sigil>, 'twigil' => '', 'name' => $$<ident> ) ] 
        } 
};

token exp_mapping {
    |   <pair> 
        [
        |   <.opt_ws> \, <.opt_ws> <exp_mapping> 
            { make [ $$<pair>, @( $$<exp_mapping> ) ] }
        |   <.opt_ws> [ \, <.opt_ws> | '' ]
            { make [ $$<pair> ] }
        ]
    |
        { make [ ] }
};

}


=begin

=head1 NAME 

MiniPerl6::Grammar - Grammar for MiniPerl6

=head1 SYNOPSIS

    my $match := $source.parse;
    ($$match).perl;    # generated MiniPerl6 AST

=head1 DESCRIPTION

This module generates a syntax tree for the MiniPerl6 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
