#!/usr/bin/pugs

use v6;

require Test;

plan 2;

# L<S02/"Names" /Which line am I in/>
is($?LINE, 10, '$?LINE works');

# L<S02/"Names" /Which file am I in/>
ok($?FILE eq ('t/magicals/file_line.t' | 't\\magicals\\file_line.t'), '$?FILE works');

# NOTE:
# above is a junction hack for Unix and Win32 file 
# paths until the FileSpec hack is working - Stevan