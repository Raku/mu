#!/usr/bin/pugs

use v6;
require Test;

plan 35;

=pod

This is a *very* basic test of the FileSpecUnix module.

=cut

require File::Spec::Unix;

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

{
    my @path = path();
    ok(+@path, '... we have elements in the path'); 

#     my $orig_path = %*ENV{'PATH'};
#     
#     %*ENV{'PATH'} = 'path/to/bin:path/to/some/other/bin:other/path:';
#     
#     my @path = path();
#     is(+@path, 4, '... we have 4 elements in the path'); 
#     is(@path[0], 'path/to/bin', '... correct first element in the path'); 
#     is(@path[1], 'path/to/some/other/bin', '... correct second element in the path'); 
#     is(@path[2], 'other/path', '... correct third element in the path'); 
#     is(@path[3], '.', '... correct fourth element in the path');             
#     
#     %*ENV{'PATH'} = $orig_path;
}

{
    my @path = splitpath('/path/to/file');
    is(+@path, 3, '... got back the expected elements');
    is(@path[0], '', '... got the right first element');    
    todo_is(@path[1], '/path/to/', '... got the right second element');
    todo_is(@path[2], 'file', '... got the right third element');    
}

{
    my @path = splitpath('/path/to/file', 1);
    is(+@path, 3, '... got back the expected elements');
    is(@path[0], '', '... got the right first element');    
    is(@path[1], '/path/to/file', '... got the right second element');
    is(@path[2], '', '... got the right third element');    
}
