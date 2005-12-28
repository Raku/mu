#!/usr/bin/pugs
use v6;

###########################################################################
###########################################################################

# Constant values used by packages in this file:
my Str %text_strings is readonly = (
    'LKT_ARG_UNDEF'
        => q[<CLASS>.<METH>(): argument <ARG> is undefined (or missing).],

    'LKT_ARG_EMP_STR'
        => q[<CLASS>.<METH>(): argument <ARG> is an empty string.],

    'LKT_ARG_NO_ARY'
        => q[<CLASS>.<METH>(): argument <ARG> is not an Array ref,]
           ~ q[ but rather contains '<VAL>'.],
    'LKT_ARG_ARY_ELEM_UNDEF'
        => q[<CLASS>.<METH>(): argument <ARG> is an Array ref as expected,]
           ~ q[ but one of its elements is undefined.],
    'LKT_ARG_ARY_ELEM_EMP_STR'
        => q[<CLASS>.<METH>(): argument <ARG> is an Array ref as expected,]
           ~ q[ but one of its elements is an empty string.],

    'LKT_ARG_NO_HASH'
        => q[<CLASS>.<METH>(): argument <ARG> is not a Hash ref,]
           ~ q[ but rather contains '<VAL>'.],
    'LKT_ARG_HASH_KEY_EMP_STR'
        => q[<CLASS>.<METH>(): argument <ARG> is a Hash ref as expected,]
           ~ q[ but one of its keys is an empty string.],
    'LKT_ARG_HASH_VAL_UNDEF'
        => q[<CLASS>.<METH>(): argument <ARG> is a Hash ref as expected,]
           ~ q[ but the value for its '<KEY>' key is undefined.],
    'LKT_ARG_HASH_VAL_EMP_STR'
        => q[<CLASS>.<METH>(): argument <ARG> is a Hash ref as expected,]
           ~ q[ but the value for its '<KEY>' key is an empty string.],

    'LKT_ARG_NO_EXP_TYPE'
        => q[<CLASS>.<METH>(): argument <ARG> is not a <EXP_TYPE>,]
           ~ q[ but rather contains '<VAL>'.],
);

###########################################################################
###########################################################################

module Locale::KeyedText::L::en-1.0.0 {
    sub get_text_by_key (Str $msg_key!) returns Str {
        return %text_strings{$msg_key};
    }
} # module Locale::KeyedText::L::en

###########################################################################
###########################################################################

=pod

=head1 NAME

Locale::KeyedText::L::en -
Localization of Locale::KeyedText for English

=head1 VERSION

This document describes Locale::KeyedText::L::en version 1.0.0.

=head1 SYNOPSIS

I<This documentation is pending.>

=head1 DESCRIPTION

I<This documentation is pending.>

=head1 INTERFACE

I<This documentation is pending; this section may also be split into several.>

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 6.x.y that is at least 6.0.0.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

I<This documentation is pending.>

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHOR

Darren R. Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the Locale::KeyedText library.

Locale::KeyedText is Copyright (c) 2002-2005, Darren R. Duncan.

See the LICENCE AND COPYRIGHT of L<Locale::KeyedText> for details.

=head1 ACKNOWLEDGEMENTS

The ACKNOWLEDGEMENTS in L<Locale::KeyedText> apply to this file too.

=cut
