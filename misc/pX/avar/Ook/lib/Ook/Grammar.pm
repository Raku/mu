module Ook::Grammar-0.01;

token hexstafur {
    <[ 0..9 a..f A..F ]>
}

token tölustafur { <[ 0..9 ]> }

token staffasti {
                '\''
    $<stafur> := [
                 |       <[-\\]>
                 | \\    <[-\$]>
                 | \\ \$ <hexstafur>**{2}
                 ]
                 '\''
}

token fjöldatala {
    [
    | \$ $<hextala> := ( <hexstafur>+ )
    |    $<tala>    := ( <tölustafur>+ )
    ]
}

token heiltala {
    $<sigil> := [ \-? ]
    <fjöldatala>
}

regex fleytitala {
    \-?
    <tölustafur>+
    \.
    <tölustafur>*
    [
    | <null>
    | <[eE]> <[+-]>? <tölustafur>+
    ]
}

=kwid

= NAME

Ook::Grammar - The ook grammar

= AUTHOR

Ævar Arnfjörð Bjarmason <avar@cpan.org>

= COPYRIGHT

Copyright (c) 2007. Ævar Arnfjörð Bjarmason

= LICENSE

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
