#!/usr/bin/perl6

use Test;
BEGIN { plan 39 }
use File::Basename <fileparse basename dirname>;

# import correctly?
ok &basename;

# set fstype -- should replace non-null default
ok length File::Basename::fileparse_fstype = "unix";

# Unix syntax tests
my ($base, $path, $type) = fileparse '/virgil/aeneid/draft.book7', rx'\.book\d+';
ok $base eq 'draft' && $path eq '/virgil/aeneid/' && $type eq '.book7';
is basename('/arma/virumque.cano'), 'virumque.cano';
is dirname('/arma/virumque.cano'),  '/arma';
is dirname('arma/'),                ".";
is dirname("/"),                    "/";


# set fstype -- should replace non-null default
ok length File::Basename::fileparse_fstype = "VMS";

# VMS syntax tests
($base, $path, $type) = fileparse 'virgil:[aeneid]draft.book7', rx{\.book\d+};
ok $base eq 'draft' && $path eq 'virgil:[aeneid]' && $type eq '.book7';
is basename('arma:[virumque]cano.trojae'), 'cano.trojae';
is dirname('arma:[virumque]cano.trojae'),  'arma:[virumque]';
is dirname('arma:<virumque>cano.trojae'),  'arma:<virumque>';
is dirname('arma:virumque.cano'),          'arma:';
$*ENV{DEFAULT} //= '';
is dirname('virumque.cano'),               $ENV{DEFAULT};
is dirname('arma/'),                       '.';


# set fstype -- should replace non-null default
ok length File::Basename::fileparse_fstype = 'MSDOS';

# MSDOS syntax tests
($base, $path, $type) = fileparse 'C:\\virgil\\aeneid\\draft.book7', rx'\.book\d+';
ok $base eq 'draft' && $path eq 'C:\\virgil\\aeneid\\' && $type eq '.book7';
is basename('A:virumque\\cano.trojae'),  'cano.trojae';
is dirname('A:\\virumque\\cano.trojae'), 'A:\\virumque';
is dirname('A:\\'),                      'A:\\';
is dirname('arma\\'),                    '.';

# Yes "/" is a legal path separator under MSDOS
is basename("lib/File/Basename.pm"), "Basename.pm";


# set fstype -- should replace non-null default
ok length File::Basename::fileparse_fstype = 'MacOS';

# MacOS syntax tests
($base, $path, $type) = fileparse 'virgil:aeneid:draft.book7', rx'\.book\d+';
is $base eq 'draft' && $path eq 'virgil:aeneid:' && $type eq '.book7';
is basename(':arma:virumque:cano.trojae'), 'cano.trojae';
is dirname(':arma:virumque:cano.trojae'),  ':arma:virumque:';
is dirname(':arma:virumque:'),             ':arma:';
is dirname(':arma:virumque'),              ':arma:';
is dirname(':arma:'),                      ':';
is dirname(':arma'),                       ':';
is dirname('arma:'),                       'arma:';
is dirname('arma'),                        ':';
is dirname(':'),                           ':';

# Check quoting of metacharacters in suffix arg by basename()
is basename(':arma:virumque:cano.trojae', '.trojae'), 'cano';
is basename(':arma:virumque:cano_trojae', '.trojae'), 'cano_trojae';


# extra tests for a few specific bugs

File::Basename::fileparse_fstype = 'MSDOS';
# perl5.003_18 gives C:/perl/.\
is (fileparse 'C:/perl/lib')[1], 'C:/perl/';
# perl5.003_18 gives C:\perl\
is dirname('C:\\perl\\lib\\'),   'C:\\perl';

File::Basename::fileparse_fstype = 'UNIX';
# perl5.003_18 gives '.'
is dirname('/perl/'),      '/';
# perl5.003_18 gives '/perl/lib'
is dirname('/perl/lib//'), '/perl';
