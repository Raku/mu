#!usr/bin/perl

use strict;
use warnings;

use Test::More tests => 164;

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
   
{
    my @upwards = ('path/to/file', '..', '.', ".\n/path");
    my @no_upwards = File::Spec::Win32->no_upwards(@upwards);
    is(scalar @no_upwards, 2, '... got one element');
    is($no_upwards[0], 'path/to/file', '... got the right element');  
    is($no_upwards[1], ".\n/path", '... got the right element');        
}  

{
    my @path = File::Spec::Win32->path();
    ok(scalar @path, '... we have elements in the path'); 
} 

{
    my ($vol, $dir, $file) = File::Spec::Win32->splitpath("C:\\path\\to\\file");
    is($vol, "C:", '... got the right volume');    
    is($dir, "\\path\\to\\", '... got the right directory');
    is($file, 'file', '... got the right file');    
}

{
    my ($vol, $dir, $file) = File::Spec::Win32->splitpath("C:\\path\\to\\dir", 1);
    is($vol, "C:", '... got the right volume');    
    is($dir, "\\path\\to\\dir", '... got the right directory');
    is($file, '', '... got the right file');    
}

# perl5 File::Spec tests

is(File::Spec::Win32->canonpath(''),               '',             'checking canonpath');
is(File::Spec::Win32->canonpath('a:'),             'A:',           'checking canonpath');
is(File::Spec::Win32->canonpath('A:f'),            'A:f',          'checking canonpath');
is(File::Spec::Win32->canonpath('A:/'),            'A:\\',         'checking canonpath');
is(File::Spec::Win32->canonpath('//a\\b//c'),      '\\\\a\\b\\c',  'checking canonpath');
is(File::Spec::Win32->canonpath('/a/..../c'),      '\\a\\....\\c', 'checking canonpath');
is(File::Spec::Win32->canonpath('//a/b\\c'),       '\\\\a\\b\\c',  'checking canonpath');
is(File::Spec::Win32->canonpath('////'),           '\\\\\\',       'checking canonpath');
is(File::Spec::Win32->canonpath('//'),             '\\',           'checking canonpath');
is(File::Spec::Win32->canonpath('/.'),             '\\.',          'checking canonpath');
is(File::Spec::Win32->canonpath('//a/b/../../c'),  '\\\\a\\b\\c',  'checking canonpath');
is(File::Spec::Win32->canonpath('//a/b/c/../d'),   '\\\\a\\b\\d',  'checking canonpath');
is(File::Spec::Win32->canonpath('//a/b/c/../../d'),'\\\\a\\b\\d',  'checking canonpath');
is(File::Spec::Win32->canonpath('//a/b/c/.../d'),  '\\\\a\\b\\d',  'checking canonpath');
is(File::Spec::Win32->canonpath('/a/b/c/../../d'), '\\a\\d',       'checking canonpath');
is(File::Spec::Win32->canonpath('/a/b/c/.../d'),   '\\a\\d',       'checking canonpath');
is(File::Spec::Win32->canonpath('\\../temp\\'),    '\\temp',       'checking canonpath');
is(File::Spec::Win32->canonpath('\\../'),          '\\',           'checking canonpath');
is(File::Spec::Win32->canonpath('\\..\\'),         '\\',           'checking canonpath');
is(File::Spec::Win32->canonpath('/../'),           '\\',           'checking canonpath');
is(File::Spec::Win32->canonpath('/..\\'),          '\\',           'checking canonpath');

is(join(',', File::Spec::Win32->splitpath('file')),                            ",,file",                            'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\d1/d2\\d3/")),                    ",\\d1/d2\\d3/,",                    'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("d1/d2\\d3/")),                      ",d1/d2\\d3/,",                      'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\d1/d2\\d3/.")),                   ",\\d1/d2\\d3/.,",                   'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\d1/d2\\d3/..")),                  ",\\d1/d2\\d3/..,",                  'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\d1/d2\\d3/.file")),               ",\\d1/d2\\d3/,.file",               'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\d1/d2\\d3/file")),                ",\\d1/d2\\d3/,file",                'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("d1/d2\\d3/file")),                  ",d1/d2\\d3/,file",                  'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("C:\\d1/d2\\d3/")),                  "C:,\\d1/d2\\d3/,",                  'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("C:d1/d2\\d3/")),                    "C:,d1/d2\\d3/,",                    'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("C:\\d1/d2\\d3/file")),              "C:,\\d1/d2\\d3/,file",              'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("C:d1/d2\\d3/file")),                "C:,d1/d2\\d3/,file",                'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("C:\\../d2\\d3/file")),              "C:,\\../d2\\d3/,file",              'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("C:../d2\\d3/file")),                "C:,../d2\\d3/,file",                'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\../..\\d1/")),                    ",\\../..\\d1/,",                    'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\./.\\d1/")),                      ",\\./.\\d1/,",                      'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\\\node\\share\\d1/d2\\d3/")),     "\\\\node\\share,\\d1/d2\\d3/,",     'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\\\node\\share\\d1/d2\\d3/file")), "\\\\node\\share,\\d1/d2\\d3/,file", 'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\\\node\\share\\d1/d2\\file")),    "\\\\node\\share,\\d1/d2\\,file",    'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("file", 1)),                         ",file,",                            'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\d1/d2\\d3/", 1)),                 ",\\d1/d2\\d3/,",                    'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("d1/d2\\d3/", 1)),                   ",d1/d2\\d3/,",                      'checking splitpath');
is(join(',', File::Spec::Win32->splitpath("\\\\node\\share\\d1/d2\\d3/", 1)),  "\\\\node\\share,\\d1/d2\\d3/,",     'checking splitpath');

is(join(',', File::Spec::Win32->splitdir('')),             ''           ,'checking splitdir');
is(join(',', File::Spec::Win32->splitdir("\\d1/d2\\d3/")), ',d1,d2,d3,' ,'checking splitdir');
is(join(',', File::Spec::Win32->splitdir("d1/d2\\d3/")),   'd1,d2,d3,'  ,'checking splitdir');
is(join(',', File::Spec::Win32->splitdir("\\d1/d2\\d3")),  ',d1,d2,d3'  ,'checking splitdir');
is(join(',', File::Spec::Win32->splitdir("d1/d2\\d3")),    'd1,d2,d3'   ,'checking splitdir');

is(File::Spec::Win32->catpath('', '', 'file'),                            'file'                            ,'checking catpath');
is(File::Spec::Win32->catpath('', "\\d1/d2\\d3/", ''),                    "\\d1/d2\\d3/"                    ,'checking catpath');
is(File::Spec::Win32->catpath('', "d1/d2\\d3/", ''),                      "d1/d2\\d3/"                      ,'checking catpath');
is(File::Spec::Win32->catpath('', "\\d1/d2\\d3/.", ''),                   "\\d1/d2\\d3/."                   ,'checking catpath');
is(File::Spec::Win32->catpath('', "\\d1/d2\\d3/..",''),                   "\\d1/d2\\d3/.."                  ,'checking catpath');
is(File::Spec::Win32->catpath('', "\\d1/d2\\d3/", '.file'),               "\\d1/d2\\d3/.file"               ,'checking catpath');
is(File::Spec::Win32->catpath('', "\\d1/d2\\d3/", 'file'),                "\\d1/d2\\d3/file"                ,'checking catpath');
is(File::Spec::Win32->catpath('', "d1/d2\\d3/", 'file'),                  "d1/d2\\d3/file"                  ,'checking catpath');
is(File::Spec::Win32->catpath('C:', "\\d1/d2\\d3/", ''),                  "C:\\d1/d2\\d3/"                  ,'checking catpath');
is(File::Spec::Win32->catpath('C:', "d1/d2\\d3/", ''),                    "C:d1/d2\\d3/"                    ,'checking catpath');
is(File::Spec::Win32->catpath('C:', "\\d1/d2\\d3/", 'file'),              "C:\\d1/d2\\d3/file"              ,'checking catpath');
is(File::Spec::Win32->catpath('C:', "d1/d2\\d3/", 'file'),                "C:d1/d2\\d3/file"                ,'checking catpath');
is(File::Spec::Win32->catpath('C:', "\\../d2\\d3/", 'file'),              "C:\\../d2\\d3/file"              ,'checking catpath');
is(File::Spec::Win32->catpath('C:', "../d2\\d3/", 'file'),                "C:../d2\\d3/file"                ,'checking catpath');
is(File::Spec::Win32->catpath('', "\\../..\\d1/", ''),                    "\\../..\\d1/"                    ,'checking catpath');
is(File::Spec::Win32->catpath('', "\\./.\\d1/", ''),                      "\\./.\\d1/"                      ,'checking catpath');
is(File::Spec::Win32->catpath("\\\\node\\share", "\\d1/d2\\d3/", ''),     "\\\\node\\share\\d1/d2\\d3/"     ,'checking catpath');
is(File::Spec::Win32->catpath("\\\\node\\share", "\\d1/d2\\d3/", 'file'), "\\\\node\\share\\d1/d2\\d3/file" ,'checking catpath');
is(File::Spec::Win32->catpath("\\\\node\\share", "\\d1/d2\\", 'file'),    "\\\\node\\share\\d1/d2\\file"    ,'checking catpath');

is(File::Spec::Win32->catfile('a', 'b', 'c'),    "a\\b\\c", 'checking catfile');
is(File::Spec::Win32->catfile('a', 'b', ".\\c"), "a\\b\\c", 'checking catfile');
is(File::Spec::Win32->catfile(".\\a", 'b', 'c'), "a\\b\\c", 'checking catfile');
is(File::Spec::Win32->catfile('c'),              'c', 'checking catfile');
is(File::Spec::Win32->catfile(".\\c"),           'c', 'checking catfile');  

is(File::Spec::Win32->catdir(),                        ''                   ,'checking catdir');
is(File::Spec::Win32->catdir(''),                      "\\"                 ,'checking catdir');
is(File::Spec::Win32->catdir("/"),                     "\\"                 ,'checking catdir');
is(File::Spec::Win32->catdir("/", "../"),              "\\"                 ,'checking catdir');
is(File::Spec::Win32->catdir("/", "..\\"),             "\\"                 ,'checking catdir');
is(File::Spec::Win32->catdir("\\", "../"),             "\\"                 ,'checking catdir');
is(File::Spec::Win32->catdir("\\", "..\\"),            "\\"                 ,'checking catdir');
is(File::Spec::Win32->catdir("//d1",'d2'),             "\\\\d1\\d2"         ,'checking catdir');
is(File::Spec::Win32->catdir("\\d1\\",'d2'),           "\\d1\\d2"           ,'checking catdir');
is(File::Spec::Win32->catdir("\\d1",'d2'),             "\\d1\\d2"           ,'checking catdir');
is(File::Spec::Win32->catdir("\\d1","\\d2"),           "\\d1\\d2"           ,'checking catdir');
is(File::Spec::Win32->catdir("\\d1","\\d2\\"),         "\\d1\\d2"           ,'checking catdir');
is(File::Spec::Win32->catdir('',"/d1",'d2'),           "\\\\d1\\d2"         ,'checking catdir');
is(File::Spec::Win32->catdir('','',"/d1",'d2'),        "\\\\\\d1\\d2"       ,'checking catdir');
is(File::Spec::Win32->catdir('',"//d1",'d2'),          "\\\\\\d1\\d2"       ,'checking catdir');
is(File::Spec::Win32->catdir('','',"//d1",'d2'),       "\\\\\\\\d1\\d2"     ,'checking catdir');
is(File::Spec::Win32->catdir('','d1','','d2',''),      "\\d1\\d2"           ,'checking catdir');
is(File::Spec::Win32->catdir('','d1','d2','d3',''),    "\\d1\\d2\\d3"       ,'checking catdir');
is(File::Spec::Win32->catdir('d1','d2','d3',''),       "d1\\d2\\d3"         ,'checking catdir');
is(File::Spec::Win32->catdir('','d1','d2','d3'),       "\\d1\\d2\\d3"       ,'checking catdir');
is(File::Spec::Win32->catdir('d1','d2','d3'),          "d1\\d2\\d3"         ,'checking catdir');
is(File::Spec::Win32->catdir("A:/d1",'d2','d3'),       "A:\\d1\\d2\\d3"     ,'checking catdir');
is(File::Spec::Win32->catdir("A:/d1",'d2','d3',''),    "A:\\d1\\d2\\d3"     ,'checking catdir');
is(File::Spec::Win32->catdir("A:/d1","B:/d2",'d3',''), "A:\\d1\\B:\\d2\\d3" ,'checking catdir');
is(File::Spec::Win32->catdir("A:/"),                   "A:\\"               ,'checking catdir');
is(File::Spec::Win32->catdir("\\", 'foo'),             "\\foo"              ,'checking catdir'); 

is(File::Spec::Win32->rel2abs('temp', "C:/"),                       "C:\\temp"                     ,'checking real2abs');
is(File::Spec::Win32->rel2abs('temp', "C:/a"),                      "C:\\a\\temp"                  ,'checking real2abs');
is(File::Spec::Win32->rel2abs('temp', "C:/a/"),                     "C:\\a\\temp"                  ,'checking real2abs');
is(File::Spec::Win32->rel2abs("../",  "C:/"),                       "C:\\"                         ,'checking real2abs');
is(File::Spec::Win32->rel2abs("../", "C:/a"),                       "C:\\"                         ,'checking real2abs');
is(File::Spec::Win32->rel2abs('temp', "//prague_main/work/"),       "\\\\prague_main\\work\\temp" ,'checking real2abs');
is(File::Spec::Win32->rel2abs("../temp", "//prague_main/work/"),    "\\\\prague_main\\work\\temp" ,'checking real2abs');
is(File::Spec::Win32->rel2abs('temp', "//prague_main/work"),        "\\\\prague_main\\work\\temp" ,'checking real2abs');
is(File::Spec::Win32->rel2abs("../", "//prague_main/work"),         "\\\\prague_main\\work"       ,'checking real2abs');

is(File::Spec::Win32->abs2rel("/t1/t2/t3", "/t1/t2/t3"),         ''                       ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("/t1/t2/t4", "/t1/t2/t3"),         "..\\t4"                 ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("/t1/t2", "/t1/t2/t3"),            '..'                     ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("/t1/t2/t3/t4", "/t1/t2/t3"),      't4'                     ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("/t4/t5/t6", "/t1/t2/t3"),         "..\\..\\..\\t4\\t5\\t6" ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("/", "/t1/t2/t3"),                 "..\\..\\.."             ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("///", "/t1/t2/t3"),               "..\\..\\.."             ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("/.", "/t1/t2/t3"),                "..\\..\\.."             ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("/./", "/t1/t2/t3"),               "..\\..\\.."             ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("\\\\a/t1/t2/t4", "/t2/t3"),       "\\\\a\\t1\\t2\\t4"      ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("//a/t1/t2/t4", "/t2/t3"),         "\\\\a\\t1\\t2\\t4"      ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("A:/t1/t2/t3", "A:/t1/t2/t3"),     ''                       ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("A:/t1/t2/t3/t4", "A:/t1/t2/t3"),  't4'                     ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("A:/t1/t2/t3", "A:/t1/t2/t3/t4"),  '..'                     ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("A:/t1/t2/t3", "B:/t1/t2/t3"),     "A:\\t1\\t2\\t3"         ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("A:/t1/t2/t3/t4", "B:/t1/t2/t3"),  "A:\\t1\\t2\\t3\\t4"     ,'checking abs2rel');
is(File::Spec::Win32->abs2rel("E:/foo/bar/baz"),                 "E:\\foo\\bar\\baz"      ,'checking abs2rel');
