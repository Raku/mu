#!usr/bin/perl

use strict;
use warnings;

use Test::More tests => 29;

=pod

This is the perl5 version of the perl6 test. 
It is here to make sure we are in sync with 
the perl5 version.

=cut

use File::Spec::Win32;

is(File::Spec::Win32->curdir(),  '.',         '... got the right curdir');
is(File::Spec::Win32->updir(),   '..',        '... got the right updir');
is(File::Spec::Win32->rootdir(), '/',         '... got the right rootdir');
is(File::Spec::Win32->devnull(), 'nul', '... got the right devnull');

ok(File::Spec::Win32->case_tolerant(), '... Win32 is case tolerant');

{
    my $path = "\\path\\to\\a\\dir";

    my @path = File::Spec::Win32->splitdir($path);
    is(scalar @path, 5, '... we have 5 elements in the path');
    is($path[0], '', '... our first element is ""');    
    is($path[1], 'path', '... our second element is "path"');    
    is($path[2], 'to', '... our third element is "to"');    
    is($path[3], 'a', '... our fourth element is "a"');    
    is($path[4], 'dir', '... our fifth element is "dir"');      
    
    is(File::Spec::Win32->catdir(@path), $path, '... got the right catdir string');                 
}

{
    my $path = "path\\to\\a\\dir";

    my @path = File::Spec::Win32->splitdir($path);
    is(scalar @path, 4, '... we have 4 elements in the path');
    is($path[0], 'path', '... our third element is "path"');    
    is($path[1], 'to', '... our fourth element is "to"');    
    is($path[2], 'a', '... our fifth element is "a"');      
    is($path[3], 'dir', '... our second element is "dir"');    
    
    is(File::Spec::Win32->catdir(@path), $path, '... got the right catdir string');                 
}

{
    my $path = "\\path\\to\\a\\file.txt";

    my @path = File::Spec::Win32->splitdir($path);
    is(scalar @path, 5, '... we have 5 elements in the path');
    is($path[0], '', '... our first element is ""');    
    is($path[1], 'path', '... our second element is "path"');    
    is($path[2], 'to', '... our third element is "to"');    
    is($path[3], 'a', '... our fourth element is "a"');    
    is($path[4], 'file.txt', '... our fifth element is "file.txt"');      
    
    is(File::Spec::Win32->catfile(@path), $path, '... got the right catfile string');                 
}

ok(File::Spec::Win32->file_name_is_absolute("C:\\\\path\\from\\root"), '... checking if path is absolute (yes)');
ok(!File::Spec::Win32->file_name_is_absolute("path\\from\\root"), '... checking if path is absolute (no)');
ok(!File::Spec::Win32->file_name_is_absolute("\nC:\\\\path\\from\\root"), '... checking if path is absolute (no)');

is(File::Spec::Win32->catpath('C:\\\\', 'dir', 'file'), "C:\\\\dir\\file", 
   '... got the right catpath string (volume is ignored)'); 
   
   
   