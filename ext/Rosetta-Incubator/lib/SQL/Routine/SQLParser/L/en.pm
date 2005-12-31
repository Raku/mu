#!/usr/bin/pugs
use v6;

###########################################################################
###########################################################################

# Constant values used by packages in this file:
my Str %TEXT_STRINGS is readonly = (
);

###########################################################################
###########################################################################

module SQL::Routine::SQLParser::L::en-0.30.0 {
    sub get_text_by_key (Str $msg_key!) returns Str {
        return %TEXT_STRINGS{$msg_key};
    }
} # module SQL::Routine::SQLParser::L::en

###########################################################################
###########################################################################

=pod

=head1 NAME

SQL::Routine::SQLParser::L::en -
Localization of SQL::Routine::SQLParser for English

=head1 VERSION

This document describes SQL::Routine::SQLParser::L::en version 0.30.0.

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

This file is part of the SQL::Routine::SQLParser reference implementation
of a SQL:2003 string parser that uses the SQL::Routine database portability
library.

SQL::Routine::SQLParser is Copyright (c) 2002-2005, Darren R. Duncan.

See the LICENCE AND COPYRIGHT of L<SQL::Routine::SQLParser> for details.

=head1 ACKNOWLEDGEMENTS

The ACKNOWLEDGEMENTS in L<SQL::Routine::SQLParser> apply to this file too.

=cut
