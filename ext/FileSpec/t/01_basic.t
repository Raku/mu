#!/usr/bin/pugs

use v6;
require Test;

=pod

This is a *very* basic test of the FileSpecUnix module.

=cut

require FileSpecUnix;

is(curdir(),  '.',         '... got the right curdir');
is(updir(),   '..',        '... got the right updir');
is(rootdir(), '/',         '... got the right rootdir');
is(devnull(), '/dev/null', '... got the right devnull');

ok(!case_tolerant(), '... unix is not case tolerant');

{
    my $path = '/path/to/a/dir';

    my @path = splitdir($path);
    is(+@path, 5, '... we have 5 elements in the path');
    is(@path[0], '', '... our first element is ""');    
    is(@path[1], 'path', '... our second element is "path"');    
    is(@path[2], 'to', '... our third element is "to"');    
    is(@path[3], 'a', '... our fourth element is "a"');    
    is(@path[4], 'dir', '... our fifth element is "dir"');      
    
    todo_is(catdir(@path), $path, '... got the right catdir string');                 
}

{
    my $path = '/path/to/a/file.txt';

    my @path = splitdir($path);
    is(+@path, 5, '... we have 5 elements in the path');
    is(@path[0], '', '... our first element is ""');    
    is(@path[1], 'path', '... our second element is "path"');    
    is(@path[2], 'to', '... our third element is "to"');    
    is(@path[3], 'a', '... our fourth element is "a"');    
    is(@path[4], 'file.txt', '... our fifth element is "file.txt"');      
    
    is(catfile(@path), $path, '... got the right catfile string');                 
}

is(catpath('vol', 'dir', 'file'), 'dir/file', 
   '... got the right catpath string (volume is ignored)'); 
   
{
    my @upwards = ('path/to/file', '..', '.', ".\n/path");
    my @no_upwards = no_upwards(@upwards);
    is(+@no_upwards, 2, '... got one element');
    is(@no_upwards[0], 'path/to/file', '... got the right element');  
    is(@no_upwards[1], ".\n/path", '... got the right element');        
}   

ok(file_name_is_absolute('/path/from/root'), '... checking if path is absolute (yes)');
ok(!file_name_is_absolute('path/from/root'), '... checking if path is absolute (no)');
ok(!file_name_is_absolute("\n/path/from/root"), '... checking if path is absolute (no)');
