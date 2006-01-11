#!/usr/bin/pugs
use v6;

# External packages used by packages in this file, that don't export symbols:
use Locale::KeyedText-(1.73.0...);
use Rosetta-(0.710.0...);
use Rosetta::Utility::SQLBuilder-(0.220.0...);
use Rosetta::Utility::SQLParser-(0.30.0...);

###########################################################################
###########################################################################

# Constant values used by packages in this file:
# (None Yet)

###########################################################################
###########################################################################

class Rosetta::Engine::Generic-0.220.0 {

    # External packages used by the Rosetta::Engine::Generic class, that do export symbols:
    # (None Yet)

    # Attributes of every Rosetta::Engine::Generic object:
    # (None Yet)

###########################################################################



###########################################################################

} # class Rosetta::Engine::Generic

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Rosetta::Engine::Generic -
A catch-all Engine for any DBI-supported SQL database

=head1 VERSION

This document describes Rosetta::Engine::Generic version 0.220.0.

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

It also requires these Perl 6 classes that are on CPAN:
L<Locale::KeyedText-(1.73.0...)|Locale::KeyedText> (for error messages),
L<Rosetta-(0.710.0...)|Rosetta>,
L<Rosetta::Utility::SQLBuilder-(0.220.0...)|Rosetta::Utility::SQLBuilder>,
L<Rosetta::Utility::SQLParser-(0.30.0...)|Rosetta::Utility::SQLParser>.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

I<This documentation is pending.>

=head1 BUGS AND LIMITATIONS

I<This documentation is pending.>

=head1 AUTHOR

Darren R. Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the Rosetta::Engine::Generic feature reference
implementation of the Rosetta database portability library.

Rosetta::Engine::Generic is Copyright (c) 2002-2006, Darren R. Duncan.  All
rights reserved.  Address comments, suggestions, and bug reports to
C<perl@DarrenDuncan.net>, or visit L<http://www.DarrenDuncan.net/> for more
information.

Rosetta::Engine::Generic is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License (GPL) as
published by the Free Software Foundation (L<http://www.fsf.org/>); either
version 2 of the License, or (at your option) any later version.  You
should have received a copy of the GPL as part of the
Rosetta::Engine::Generic distribution, in the file named "GPL"; if not,
write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA  02110-1301, USA.

Linking Rosetta::Engine::Generic statically or dynamically with other
files is making a combined work based on Rosetta::Engine::Generic.  Thus,
the terms and conditions of the GPL cover the whole combination.  As a
special exception, the copyright holders of Rosetta::Engine::Generic give
you permission to link Rosetta::Engine::Generic with independent files,
regardless of the license terms of these independent files, and to copy
and distribute the resulting combined work under terms of your choice,
provided that every copy of the combined work is accompanied by a complete
copy of the source code of Rosetta::Engine::Generic (the version of
Rosetta::Engine::Generic used to produce the combined work), being
distributed under the terms of the GPL plus this exception.  An independent
file is a file which is not derived from or based on
Rosetta::Engine::Generic, and which is fully useable when not linked to
Rosetta::Engine::Generic in any form.

Any versions of Rosetta::Engine::Generic that you modify and distribute
must carry prominent notices stating that you changed the files and the
date of any changes, in addition to preserving this original copyright
notice and other credits.  Rosetta::Engine::Generic is distributed in the
hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

While it is by no means required, the copyright holders of
Rosetta::Engine::Generic would appreciate being informed any time you
create a modified version of Rosetta::Engine::Generic that you are willing
to distribute, because that is a practical way of suggesting improvements
to the standard version.

=head1 ACKNOWLEDGEMENTS

None yet.

=cut
