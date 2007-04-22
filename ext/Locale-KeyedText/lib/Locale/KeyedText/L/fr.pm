use v6-alpha;

###########################################################################
###########################################################################

# Constant values used by packages in this file:
my Str %TEXT_STRINGS is readonly = (
    # This group of strings is generic and can be used by any package:

    'LKT_ARG_UNDEF'
        => q[<CLASS>.<METH>(): l'argument <ARG> n'est pas définis (ou est]
            ~ q[ manquant).],
    'LKT_ARG_NO_ARY'
        => q[<CLASS>.<METH>(): argument <ARG> n'est pas un Array',]
           ~ q[ mais contient '<VAL>'.],
    'LKT_ARG_NO_HASH'
        => q[<CLASS>.<METH>(): argument <ARG> n'est pas un Hash',]
           ~ q[ mais contient '<VAL>'.],
    'LKT_ARG_NO_EXP_TYPE'
        => q[<CLASS>.<METH>(): l'argument <ARG> n'est pas <EXP_TYPE>,]
           ~ q[ mais contient '<VAL>'.],

    'LKT_ARG_ARY_ELEM_UNDEF'
        => q[<CLASS>.<METH>(): argument <ARG> est un Array comme]
           ~ q[ attendu, mais un de ces éléments est indéfinis.],
    'LKT_ARG_ARY_ELEM_NO_ARY'
        => q[<CLASS>.<METH>(): argument <ARG> est un Array comme]
           ~ q[ attendu, mais un de ces éléments n'est pas un ]
           ~ q[ Array, mais contient '<VAL>'.],
    'LKT_ARG_ARY_ELEM_NO_HASH'
        => q[<CLASS>.<METH>(): argument <ARG> est un Array comme]
           ~ q[ attendu,mais un de ces éléments n'est pas un Hash,]
           ~ q[ mais contient '<VAL>'.],
    'LKT_ARG_ARY_ELEM_NO_EXP_TYPE'
        => q[<CLASS>.<METH>(): argument <ARG> est un Array comme]
           ~ q[ attendu, mais un de ces éléments n'est pas <EXP_TYPE>,]
           ~ q[ mais contient '<VAL>'.],

    'LKT_ARG_HASH_VAL_UNDEF'
        => q[<CLASS>.<METH>(): argument <ARG> est un Hash,]
           ~ q[ mais la valeur pour cette clef '<KEY>' est indéfinies.],
    'LKT_ARG_HASH_VAL_NO_ARY'
        => q[<CLASS>.<METH>(): argument <ARG> est un Hash comme attendu,]
           ~ q[ mais la valeur pour sa clef '<KEY>' n'est pas un Array,]
           ~ q[ mais contient '<VAL>'.],
    'LKT_ARG_HASH_VAL_NO_HASH'
        => q[<CLASS>.<METH>(): argument <ARG> est un Hash comme attendu,]
           ~ q[ mais la valeur pour sa clef '<KEY>' n'est pas un Hash,]
           ~ q[ mais contient '<VAL>'.],
    'LKT_ARG_HASH_VAL_NO_EXP_TYPE'
        => q[<CLASS>.<METH>(): argument <ARG> est un Hash comme attendu,]
           ~ q[ mais la valeur pour sa clef '<KEY>' n'est pas un]
           ~ q[ <EXP_TYPE>, mais contient '<VAL>'.],

    'LKT_ARG_ARY_NO_ELEMS'
        => q[<CLASS>.<METH>(): argument <ARG> est un Array comme]
           ~ q[ attendu, mais ne contient aucun élément.],
    'LKT_ARG_HASH_NO_ELEMS'
        => q[<CLASS>.<METH>(): argument <ARG> est un Hash comme attendu,]
           ~ q[ mais ne contient aucun élément.],

    'LKT_ARG_EMP_STR'
        => q[<CLASS>.<METH>(): l'argument <ARG> est une chaine vide.],
    'LKT_ARG_ARY_ELEM_EMP_STR'
        => q[<CLASS>.<METH>(): argument <ARG> est un Array comme]
           ~ q[ attendu, mais un de ces éléments est une chaine vide.],
    'LKT_ARG_HASH_KEY_EMP_STR'
        => q[<CLASS>.<METH>(): argument <ARG> est un Hash comme attendu,]
           ~ q[ mais une de ces chaines est une chaine vide.],
    'LKT_ARG_HASH_VAL_EMP_STR'
        => q[<CLASS>.<METH>(): argument <ARG> est un Hash comme attendu,]
           ~ q[ mais la valeur pour la clef '<KEY>' est une chaine vide.],

    # This group of strings is specific to Locale::KeyedText itself:

    'LKT_T_FAIL_LOAD_TMPL_MOD'
        => q[<CLASS>.<METH>(): impossible de localiser Locale::KeyedText]
           ~ q[ Template module '<TMPL_MOD_NAME>': <REASON>],
    'LKT_T_FAIL_GET_TMPL_TEXT'
        => q[<CLASS>.<METH>(): impossible d'invoquer get_text_by_key() sur]
           ~ q[ Locale::KeyedText Template module '<TMPL_MOD_NAME>':]
           ~ q[ <REASON>],
);

###########################################################################
###########################################################################

module Locale::KeyedText::L::fr-1.0.0 {
    sub get_text_by_key (Str $msg_key!) returns Str {
        return %TEXT_STRINGS{$msg_key};
    }
} # module Locale::KeyedText::L::fr

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Locale::KeyedText::L::fr -
Localization of Locale::KeyedText for French

=head1 VERSION

This document describes Locale::KeyedText::L::fr version 1.0.0 for Perl 6.

=head1 SYNOPSIS

I<This documentation is pending.>

=head1 DESCRIPTION

I<This documentation is pending.>

=head1 INTERFACE

I<This documentation is pending; this section may also be split into
several.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

Go to L<Locale::KeyedText> for the majority of references.

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHOR

Franck Cuny (C<franck.cuny@gmail.com>)

=head1 LICENCE AND COPYRIGHT

This file is part of the Locale::KeyedText library.

Locale::KeyedText is Copyright © 2003-2007, Darren Duncan.

See the LICENCE AND COPYRIGHT of L<Locale::KeyedText> for details.

=head1 ACKNOWLEDGEMENTS

The ACKNOWLEDGEMENTS in L<Locale::KeyedText> apply to this file too.

=cut
