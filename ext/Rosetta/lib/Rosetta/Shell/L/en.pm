#!/usr/bin/pugs
use v6;

###########################################################################
###########################################################################

# Constant values used by packages in this file:
my Str %TEXT_STRINGS is readonly = (
    'ROS_S_HELLO' => q[Welcome to the Rosetta DBMS!],
    'ROS_S_GOODBYE' => q[Goodbye!],
    'ROS_S_DBMS_INIT_FAIL'
        => q[Initialization of the Engine "<ENGINE_NAME>" has failed.],
    'ROS_S_DBMS_INIT_SUCCESS'
        => q[Initialization of the Engine "<ENGINE_NAME>" is successful.],
    'ROS_S_PROMPT'
        => q[Enter a Rosetta D command to execute (all on one line),]
           ~ q[ or press ENTER to quit.],
    'ROS_S_TODO_RESULT' => q[TODO: prepare and execute that command.],
);

###########################################################################
###########################################################################

module Rosetta::Shell::L::en-0.1.0 {
    sub get_text_by_key (Str $msg_key!) returns Str {
        return %TEXT_STRINGS{$msg_key};
    }
} # module Rosetta::Shell::L::en

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Rosetta::Shell::L::en -
Localization of Rosetta::Shell for English

=head1 VERSION

This document describes Rosetta::Shell::L::en version 0.1.0.

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

This file is part of the Rosetta DBMS framework.

Rosetta is Copyright (c) 2002-2006, Darren R. Duncan.

See the LICENCE AND COPYRIGHT of L<Rosetta> for details.

=head1 ACKNOWLEDGEMENTS

The ACKNOWLEDGEMENTS in L<Rosetta> apply to this file too.

=cut
