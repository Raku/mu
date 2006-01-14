#!/usr/bin/pugs

use v6;
use Test;

plan 106;

=pod

This is test of the File::Spec::Unix module.
For the perl5 equivalent see the t/11_unix_test_p5.t
file.

=cut

use File::Spec::Unix; pass "(dummy instead of broken use_ok)";

is(curdir(),  '.',         '... got the right curdir');
is(updir(),   '..',        '... got the right updir');
is(rootdir(), '/',         '... got the right rootdir');
is(devnull(), '/dev/null', '... got the right devnull');

ok(!case_tolerant(), '... unix is not case tolerant');

{
    my @paths = (
        "path///to//a///////dir/", 
        "path/./to/././a/./././dir/",
        "./path/to/a/dir/",
        "././path/to/a/dir/"              
        );
    
    for @paths -> $path {
        is(canonpath($path), 'path/to/a/dir', '... cannonpath works for ' ~ $path);
    }
    
    my @paths2 = (       
        "/../path/to/a/dir/",
        "/../../path/to/a/dir/"               
        );
    
    for @paths2 -> $path {
        is(canonpath($path), '/path/to/a/dir', '... cannonpath works for ' ~ $path);
    }    
}

{
    my $path = '/path/to/a/dir';

    my @path = splitdir($path);
    is(+@path, 5, '... we have 5 elements in the path');
    is(@path[0], '', '... our first element is ""');    
    is(@path[1], 'path', '... our second element is "path"');    
    is(@path[2], 'to', '... our third element is "to"');    
    is(@path[3], 'a', '... our fourth element is "a"');    
    is(@path[4], 'dir', '... our fifth element is "dir"');      
    
    is(catdir(@path), $path, '... got the right catdir string');                 
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
   
is(catpath('', 'dir', 'file'), 'dir/file', 
   '... got the right catpath string (volume is ignored)'); 

is(catpath('', 'dir/', 'file'), 'dir/file', 
   '... got the right catpath string (volume is ignored)'); 

is(catpath('', '', 'file'), 'file', 
   '... got the right catpath string (volume is ignored)'); 

is(catpath('', '', ''), '', 
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

     my $orig_path = %*ENV{'PATH'};
     
     %*ENV{'PATH'} = 'path/to/bin:path/to/some/other/bin:other/path:';
     
     my @path = path();
     is(+@path, 4, '... we have 4 elements in the path'); 
     is(@path[0], 'path/to/bin', '... correct first element in the path'); 
     is(@path[1], 'path/to/some/other/bin', '... correct second element in the path'); 
     is(@path[2], 'other/path', '... correct third element in the path'); 
     is(@path[3], '.', '... correct fourth element in the path');             
     
     %*ENV{'PATH'} = $orig_path;
}

{
    my ($vol, $dir, $file) = splitpath('/path/to/file');
    is($vol, '', '... got the right volume');    
    is($dir, '/path/to/', '... got the right directory');
    is($file, 'file', '... got the right file');    
}

{
    my ($vol, $dir, $file) = splitpath('/path/to/file', 1);
    is($vol, '', '... got the right volume');    
    is($dir, '/path/to/file', '... got the right directory');
    is($file, '', '... got the right file');       
}

# these are adapted from the t/Spec.t in the perl5 File::Spec tests

# abs2rel/rel2abs

is(abs2rel('/t1/t2/t3','/t1/t2/t3'),    '',                  'checking abs2real');
is(abs2rel('/t1/t2/t4','/t1/t2/t3'),    '../t4',             'checking abs2real'); 
is(abs2rel('/t1/t2','/t1/t2/t3'),       '..',                'checking abs2real'); 
is(abs2rel('/t1/t2/t3/t4','/t1/t2/t3'), 't4',                'checking abs2real'); 
is(abs2rel('/t4/t5/t6','/t1/t2/t3'),    '../../../t4/t5/t6', 'checking abs2real'); 
is(abs2rel('/','/t1/t2/t3'),            '../../..',          'checking abs2real'); 
is(abs2rel('///','/t1/t2/t3'),          '../../..',          'checking abs2real'); 
is(abs2rel('/.','/t1/t2/t3'),           '../../..',          'checking abs2real'); 
is(abs2rel('/./','/t1/t2/t3'),          '../../..',          'checking abs2real'); 

is(rel2abs('t4', '/t1/t2/t3'),    '/t1/t2/t3/t4',    'checking rel2abs');
is(rel2abs('t4/t5', '/t1/t2/t3'), '/t1/t2/t3/t4/t5', 'checking rel2abs');
is(rel2abs('.', '/t1/t2/t3'),     '/t1/t2/t3',       'checking rel2abs');
is(rel2abs('..', '/t1/t2/t3'),    '/t1/t2/t3/..',    'checking rel2abs');
is(rel2abs('../t4', '/t1/t2/t3'), '/t1/t2/t3/../t4', 'checking rel2abs');
is(rel2abs('/t1', '/t1/t2/t3'),   '/t1',             'checking rel2abs');

# concatenating

is(catfile('a','b','c'),   'a/b/c', 'checking catfile');
is(catfile('a','b','./c'), 'a/b/c', 'checking catfile');
is(catfile('./a','b','c'), 'a/b/c', 'checking catfile');
is(catfile('c'),           'c',     'checking catfile');
is(catfile('./c'),         'c',     'checking catfile');

is(catpath('','','file'),            'file',            'checking catpath');
is(catpath('','/d1/d2/d3/',''),      '/d1/d2/d3/',      'checking catpath');
is(catpath('','d1/d2/d3/',''),       'd1/d2/d3/',       'checking catpath');
is(catpath('','/d1/d2/d3/.',''),     '/d1/d2/d3/.',     'checking catpath');
is(catpath('','/d1/d2/d3/..',''),    '/d1/d2/d3/..',    'checking catpath');
is(catpath('','/d1/d2/d3/','.file'), '/d1/d2/d3/.file', 'checking catpath');
is(catpath('','d1/d2/d3/','file'),   'd1/d2/d3/file',   'checking catpath');
is(catpath('','/../../d1/',''),      '/../../d1/',      'checking catpath');
is(catpath('','/././d1/',''),        '/././d1/',        'checking catpath');
is(catpath('d1','d2/d3/',''),        'd2/d3/',          'checking catpath');
is(catpath('d1','d2','d3/'),         'd2/d3/',          'checking catpath');

is(catdir(),                     '',          'checking catdir');
is(catdir('/'),                  '/',         'checking catdir');
is(catdir('','d1','d2','d3',''), '/d1/d2/d3', 'checking catdir');
is(catdir('d1','d2','d3',''),    'd1/d2/d3',  'checking catdir');
is(catdir('','d1','d2','d3'),    '/d1/d2/d3', 'checking catdir');
is(catdir('d1','d2','d3'),       'd1/d2/d3',  'checking catdir');

# splitting

is(join(',', splitpath('file')),            ',,file',            'checking splitpath');
is(join(',', splitpath('/d1/d2/d3/')),      ',/d1/d2/d3/,',      'checking splitpath');
is(join(',', splitpath('d1/d2/d3/')),       ',d1/d2/d3/,',       'checking splitpath');
is(join(',', splitpath('/d1/d2/d3/.')),     ',/d1/d2/d3/.,',     'checking splitpath');
is(join(',', splitpath('/d1/d2/d3/..')),    ',/d1/d2/d3/..,',    'checking splitpath');
is(join(',', splitpath('/d1/d2/d3/.file')), ',/d1/d2/d3/,.file', 'checking splitpath');
is(join(',', splitpath('d1/d2/d3/file')),   ',d1/d2/d3/,file',   'checking splitpath');
is(join(',', splitpath('/../../d1/')),      ',/../../d1/,',      'checking splitpath');
is(join(',', splitpath('/././d1/')),        ',/././d1/,',        'checking splitpath');

is(join(',', splitdir('')),           '',           'checking splitdir');
is(join(',', splitdir('/d1/d2/d3/')), ',d1,d2,d3,', 'checking splitdir');
is(join(',', splitdir('d1/d2/d3/')),  'd1,d2,d3,',  'checking splitdir');
is(join(',', splitdir('/d1/d2/d3')),  ',d1,d2,d3',  'checking splitdir');
is(join(',', splitdir('d1/d2/d3')),   'd1,d2,d3',   'checking splitdir');

# cannonpath

is(canonpath(''),                                      '',          'checking canonpath');
is(canonpath('///../../..//./././a//b/.././c/././'),   '/a/b/../c', 'checking canonpath');
is(canonpath('/.'),                                    '/',         'checking canonpath');
is(canonpath('/./'),                                   '/',         'checking canonpath');
is(canonpath('/a/./'),                                 '/a',        'checking canonpath');
is(canonpath('/a/.'),                                  '/a',        'checking canonpath');
