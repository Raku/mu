use Stream 'node';

sub iterator_to_stream {
  my $it = shift;
  my $v = $it->();
  return unless defined $v;
  node($v, sub { iterator_to_stream($it) });
}

1;


# COPYRIGHT NOTICE:
# The contents of this file are Copyright (c) 2008, Matthew Wilson
# and any other contributors whose commits are recorded by the
# "pugscode" subversion source control repository.  The contributors'
# names/handles of are listed in the "pugsroot/AUTHORS" file).
# See licenses/Artistic2.txt for the 'Artistic License 2.0',
# under which this code is distributed and which may be found
# at http://www.opensource.org/licenses/artistic-license-2.0.php
# or http://www.perlfoundation.org/artistic_license_2_0

# ORIGIN:
# The code in this file originates directly
#       from Higher-Order Perl by Mark Dominus,
#       published by Morgan Kaufmann Publishers,
#       Copyright 2005 by Elsevier Inc
# Because of the origin, this file is also subject to the license
# agreement at http://hop.perl.plover.com/LICENSE.txt