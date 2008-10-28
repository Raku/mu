package SMOP;

use 5.010000;
use strict;
use warnings;
use Carp;

our $VERSION = '0.01';

require XSLoader;
XSLoader::load('SMOP', $VERSION);

1;
__END__

=head1 NAME

SMOP - Perl extension for SMOP

=head1 SYNOPSIS

  use SMOP;
  my $int = SMOP::NATIVE::int->create(10);
  my $true = SMOP::NATIVE::bool->true;
  my $fals = SMOP::NATIVE::bool->false;
  my $idconst = SMOP::NATIVE::idconst->create('Hello World');
  my $rootns = SMOP::S1P->RootNamespace;
  my $prelud = SMOP::S1P->LexicalPrelude;
  my $mold = SMOP::Mold->create(12,[$rootns, $int, $true, $fals],
                                45,[1,2,3,1,2,3,1,2,3]);
  my $frame = SMOP::MoldFrame->create($mold);
  my $result = SMOP::Interpreter->run($frame);

=head1 DESCRIPTION

This module provides access to the SMOP runtime from Perl 5. It still
doesn't interoperate in a way to provide callbacks, therefore you
cannot exchange continuations from SMOP to p5.

=head1 SEE ALSO

http://www.perlfoundation.org/perl6/index.cgi?smop

=head1 AUTHOR

Daniel Ruoso, E<lt>daniel at ruoso.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2008 by Daniel Ruoso and others

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.0 or,
at your option, any later version of Perl 5 you may have available.

=cut
