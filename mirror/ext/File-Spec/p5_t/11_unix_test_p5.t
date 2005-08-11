#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 100;

=pod

This is the perl5 version of the perl6 test. 
It is here to make sure we are in sync with 
the perl5 version.

=cut

use File::Spec::Unix;

is(File::Spec::Unix->curdir(),  '.',         '... got the right curdir');
is(File::Spec::Unix->updir(),   '..',        '... got the right updir');
is(File::Spec::Unix->rootdir(), '/',         '... got the right rootdir');
is(File::Spec::Unix->devnull(), '/dev/null', '... got the right devnull');

ok(!File::Spec::Unix->case_tolerant(), '... unix is not case tolerant');

{
    my @paths = (
        "path///to//a///////dir/", 
        "path/./to/././a/./././dir/",
        "./path/to/a/dir/",
        "././path/to/a/dir/"              
        );
    
    for my $path (@paths) {
        is(File::Spec::Unix->canonpath($path), 'path/to/a/dir', '... cannonpath works for ' . $path);
    }
    
    my @paths2 = (       
        "/../path/to/a/dir/",
        "/../../path/to/a/dir/"               
        );
    
    for my $path (@paths2) {
        is(File::Spec::Unix->canonpath($path), '/path/to/a/dir', '... cannonpath works for ' . $path);
    }    
}

{
    my $path = '/path/to/a/dir';

    my @path = File::Spec::Unix->splitdir($path);
    is(scalar @path, 5, '... we have 5 elements in the path');
    is($path[0], '', '... our first element is ""');    
    is($path[1], 'path', '... our second element is "path"');    
    is($path[2], 'to', '... our third element is "to"');    
    is($path[3], 'a', '... our fourth element is "a"');    
    is($path[4], 'dir', '... our fifth element is "dir"');      
    
    is(File::Spec::Unix->catdir(@path), $path, '... got the right catdir string');                 
}

{
    my $path = '/path/to/a/file.txt';

    my @path = File::Spec::Unix->splitdir($path);
    is(scalar @path, 5, '... we have 5 elements in the path');
    is($path[0], '', '... our first element is ""');    
    is($path[1], 'path', '... our second element is "path"');    
    is($path[2], 'to', '... our third element is "to"');    
    is($path[3], 'a', '... our fourth element is "a"');    
    is($path[4], 'file.txt', '... our fifth element is "file.txt"');      
    
    is(File::Spec::Unix->catfile(@path), $path, '... got the right catfile string');                 
}

is(File::Spec::Unix->catpath('vol', 'dir', 'file'), 'dir/file', 
   '... got the right catpath string (volume is ignored)'); 

is(File::Spec::Unix->catpath('', 'dir', 'file'), 'dir/file', 
   '... got the right catpath string (volume is ignored)'); 

is(File::Spec::Unix->catpath('', 'dir/', 'file'), 'dir/file', 
   '... got the right catpath string (volume is ignored)'); 

is(File::Spec::Unix->catpath('', '', 'file'), 'file', 
   '... got the right catpath string (volume is ignored)'); 

is(File::Spec::Unix->catpath('', '', ''), '', 
   '... got the right catpath string (volume is ignored)'); 
   
{
    my @upwards = ('path/to/file', '..', '.', ".\n/path");
    my @no_upwards = File::Spec::Unix->no_upwards(@upwards);
    is(scalar @no_upwards, 2, '... got one element');
    is($no_upwards[0], 'path/to/file', '... got the right element');  
    is($no_upwards[1], ".\n/path", '... got the right element');        
}   

ok(File::Spec::Unix->file_name_is_absolute('/path/from/root'), '... checking if path is absolute (yes)');
ok(!File::Spec::Unix->file_name_is_absolute('path/from/root'), '... checking if path is absolute (no)');
ok(!File::Spec::Unix->file_name_is_absolute("\n/path/from/root"), '... checking if path is absolute (no)');

{
    my @path = File::Spec::Unix->path();
    ok(scalar @path, '... we have elements in the path'); 

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
    my ($vol, $dir, $file) = File::Spec::Unix->splitpath('/path/to/file');
    is($vol, '', '... got the right volume');    
    is($dir, '/path/to/', '... got the right directory');
    is($file, 'file', '... got the right file');    
}

{
    my ($vol, $dir, $file) = File::Spec::Unix->splitpath('/path/to/file', 1);
    is($vol, '', '... got the right volume');    
    is($dir, '/path/to/file', '... got the right directory');
    is($file, '', '... got the right file');       
}

{
    is(File::Spec::Unix->abs2rel('/t1/t2/t3','/t1/t2/t3'), '', 'checking abs2real');
    is(File::Spec::Unix->abs2rel('/t1/t2/t4','/t1/t2/t3'), '../t4', 'checking abs2real'); 
    is(File::Spec::Unix->abs2rel('/t1/t2','/t1/t2/t3'), '..', 'checking abs2real'); 
    is(File::Spec::Unix->abs2rel('/t1/t2/t3/t4','/t1/t2/t3'), 't4', 'checking abs2real'); 
    is(File::Spec::Unix->abs2rel('/t4/t5/t6','/t1/t2/t3'), '../../../t4/t5/t6', 'checking abs2real'); 
    is(File::Spec::Unix->abs2rel('/','/t1/t2/t3'), '../../..', 'checking abs2real'); 
    is(File::Spec::Unix->abs2rel('///','/t1/t2/t3'), '../../..', 'checking abs2real'); 
    is(File::Spec::Unix->abs2rel('/.','/t1/t2/t3'), '../../..', 'checking abs2real'); 
    is(File::Spec::Unix->abs2rel('/./','/t1/t2/t3'), '../../..', 'checking abs2real');  
}

{
    is(File::Spec::Unix->rel2abs('t4', '/t1/t2/t3'),    '/t1/t2/t3/t4',    'checking rel2abs');
    is(File::Spec::Unix->rel2abs('t4/t5', '/t1/t2/t3'), '/t1/t2/t3/t4/t5', 'checking rel2abs');
    is(File::Spec::Unix->rel2abs('.', '/t1/t2/t3'),     '/t1/t2/t3',       'checking rel2abs');
    is(File::Spec::Unix->rel2abs('..', '/t1/t2/t3'),    '/t1/t2/t3/..',    'checking rel2abs');
    is(File::Spec::Unix->rel2abs('../t4', '/t1/t2/t3'), '/t1/t2/t3/../t4', 'checking rel2abs');
    is(File::Spec::Unix->rel2abs('/t1', '/t1/t2/t3'),   '/t1',             'checking rel2abs');
    
    # concatenating
    
    is(File::Spec::Unix->catfile('a','b','c'),   'a/b/c', 'checking catfile');
    is(File::Spec::Unix->catfile('a','b','./c'), 'a/b/c', 'checking catfile');
    is(File::Spec::Unix->catfile('./a','b','c'), 'a/b/c', 'checking catfile');
    is(File::Spec::Unix->catfile('c'),           'c',     'checking catfile');
    is(File::Spec::Unix->catfile('./c'),         'c',     'checking catfile');
    
    is(File::Spec::Unix->catpath('','','file'),            'file',            'checking catpath');
    is(File::Spec::Unix->catpath('','/d1/d2/d3/',''),      '/d1/d2/d3/',      'checking catpath');
    is(File::Spec::Unix->catpath('','d1/d2/d3/',''),       'd1/d2/d3/',       'checking catpath');
    is(File::Spec::Unix->catpath('','/d1/d2/d3/.',''),     '/d1/d2/d3/.',     'checking catpath');
    is(File::Spec::Unix->catpath('','/d1/d2/d3/..',''),    '/d1/d2/d3/..',    'checking catpath');
    is(File::Spec::Unix->catpath('','/d1/d2/d3/','.file'), '/d1/d2/d3/.file', 'checking catpath');
    is(File::Spec::Unix->catpath('','d1/d2/d3/','file'),   'd1/d2/d3/file',   'checking catpath');
    is(File::Spec::Unix->catpath('','/../../d1/',''),      '/../../d1/',      'checking catpath');
    is(File::Spec::Unix->catpath('','/././d1/',''),        '/././d1/',        'checking catpath');
    is(File::Spec::Unix->catpath('d1','d2/d3/',''),        'd2/d3/',          'checking catpath');
    is(File::Spec::Unix->catpath('d1','d2','d3/'),         'd2/d3/',          'checking catpath');
    
    is(File::Spec::Unix->catdir(),                     '',          'checking catdir');
    is(File::Spec::Unix->catdir('/'),                  '/',         'checking catdir');
    is(File::Spec::Unix->catdir('','d1','d2','d3',''), '/d1/d2/d3', 'checking catdir');
    is(File::Spec::Unix->catdir('d1','d2','d3',''),    'd1/d2/d3',  'checking catdir');
    is(File::Spec::Unix->catdir('','d1','d2','d3'),    '/d1/d2/d3', 'checking catdir');
    is(File::Spec::Unix->catdir('d1','d2','d3'),       'd1/d2/d3',  'checking catdir');
    
    # splitting
    
    is(join(',', File::Spec::Unix->splitpath('file')),            ',,file',            'checking splitpath');
    is(join(',', File::Spec::Unix->splitpath('/d1/d2/d3/')),      ',/d1/d2/d3/,',      'checking splitpath');
    is(join(',', File::Spec::Unix->splitpath('d1/d2/d3/')),       ',d1/d2/d3/,',       'checking splitpath');
    is(join(',', File::Spec::Unix->splitpath('/d1/d2/d3/.')),     ',/d1/d2/d3/.,',     'checking splitpath');
    is(join(',', File::Spec::Unix->splitpath('/d1/d2/d3/..')),    ',/d1/d2/d3/..,',    'checking splitpath');
    is(join(',', File::Spec::Unix->splitpath('/d1/d2/d3/.file')), ',/d1/d2/d3/,.file', 'checking splitpath');
    is(join(',', File::Spec::Unix->splitpath('d1/d2/d3/file')),   ',d1/d2/d3/,file',   'checking splitpath');
    is(join(',', File::Spec::Unix->splitpath('/../../d1/')),      ',/../../d1/,',      'checking splitpath');
    is(join(',', File::Spec::Unix->splitpath('/././d1/')),        ',/././d1/,',        'checking splitpath');
    
    is(join(',', File::Spec::Unix->splitdir('')),           '',           'checking splitdir');
    is(join(',', File::Spec::Unix->splitdir('/d1/d2/d3/')), ',d1,d2,d3,', 'checking splitdir');
    is(join(',', File::Spec::Unix->splitdir('d1/d2/d3/')),  'd1,d2,d3,',  'checking splitdir');
    is(join(',', File::Spec::Unix->splitdir('/d1/d2/d3')),  ',d1,d2,d3',  'checking splitdir');
    is(join(',', File::Spec::Unix->splitdir('d1/d2/d3')),   'd1,d2,d3',   'checking splitdir');
    
    # cannonpath
    
    is(File::Spec::Unix->canonpath(''),                                      '',          'checking canonpath');
    is(File::Spec::Unix->canonpath('///../../..//./././a//b/.././c/././'),   '/a/b/../c', 'checking canonpath');
    is(File::Spec::Unix->canonpath('/.'),                                    '/',         'checking canonpath');
    is(File::Spec::Unix->canonpath('/./'),                                   '/',         'checking canonpath');
    is(File::Spec::Unix->canonpath('/a/./'),                                 '/a',        'checking canonpath');
    is(File::Spec::Unix->canonpath('/a/.'),                                  '/a',        'checking canonpath');

}


