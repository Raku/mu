#!/usr/bin/pugs

use v6;
use Test;

plan 165;

=pod

This is a test of the FileSpecWin32 module.

=cut

use File::Spec::Win32; pass "(dummy instead of broken use_ok)";

is(curdir(),  '.',         '... got the right curdir');
is(updir(),   '..',        '... got the right updir');
is(rootdir(), '\\',         '... got the right rootdir');
is(devnull(), 'nul', '... got the right devnull');

ok(case_tolerant(), '... Win32 is case tolerant');

{
    my $path = "\\path\\to\\a\\dir";

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
    my $path = "path\\to\\a\\dir";

    my @path = splitdir($path);
    is(+@path, 4, '... we have 4 elements in the path');
    is(@path[0], 'path', '... our third element is "path"');    
    is(@path[1], 'to', '... our fourth element is "to"');    
    is(@path[2], 'a', '... our fifth element is "a"');      
    is(@path[3], 'dir', '... our second element is "dir"');    
    
    is(catdir(@path), $path, '... got the right catdir string');                 
}

{
    my $path = "\\path\\to\\a\\file.txt";

    my @path = splitdir($path);
    is(+@path, 5, '... we have 5 elements in the path');
    is(@path[0], '', '... our first element is ""');    
    is(@path[1], 'path', '... our second element is "path"');    
    is(@path[2], 'to', '... our third element is "to"');    
    is(@path[3], 'a', '... our fourth element is "a"');    
    is(@path[4], 'file.txt', '... our fifth element is "file.txt"');      
    
    is(catfile(@path), $path, '... got the right catfile string');                 
}

ok(file_name_is_absolute("C:\\\\path\\from\\root"), '... checking if path is absolute (yes)');
ok(!file_name_is_absolute("path\\from\\root"), '... checking if path is absolute (no)');
ok(!file_name_is_absolute("\nC:\\\\path\\from\\root"), '... checking if path is absolute (no)');

is(catpath("C:\\\\", 'dir', 'file'), "C:\\\\dir\\file", 
   '... got the right catpath string (volume is ignored)'); 
   
{
    my @upwards = ('path/to/file', '..', '.', ".\n/path");
    my @no_upwards = no_upwards(@upwards);
    is(+@no_upwards, 2, '... got one element');
    is(@no_upwards[0], 'path/to/file', '... got the right element');  
    is(@no_upwards[1], ".\n/path", '... got the right element');        
}   

{
    my @path = path();
    ok(+@path, '... we have elements in the path'); 
}

{
    my ($vol, $dir, $file) = splitpath("C:\\path\\to\\file");
    is($vol, "C:", '... got the right volume');    
    is($dir, "\\path\\to\\", '... got the right directory');
    is($file, 'file', '... got the right file');    
}
 
{
    my ($vol, $dir, $file) = splitpath("C:\\path\\to\\dir", 1);
    is($vol, "C:", '... got the right volume');    
    is($dir, "\\path\\to\\dir", '... got the right directory');
    is($file, '', '... got the right file');    
} 

# these are tests from the t/Spec.t file from the perl5 File::Spec

is(canonpath(""),               "",             'checking canonpath');
is(canonpath("a:"),             "A:",           'checking canonpath');
is(canonpath("A:f"),            "A:f",          'checking canonpath');
is(canonpath("A:/"),            "A:\\",         'checking canonpath');
is(canonpath("//a\\b//c"),      "\\\\a\\b\\c",  'checking canonpath');
is(canonpath("/a/..../c"),      "\\a\\....\\c", 'checking canonpath');
is(canonpath("//a/b\\c"),       "\\\\a\\b\\c",  'checking canonpath');
is(canonpath("////"),           "\\\\\\",       'checking canonpath');
is(canonpath("//"),             "\\",           'checking canonpath');
is(canonpath("/."),             "\\.",          'checking canonpath');
is(canonpath("//a/b/../../c"),  "\\\\a\\b\\c",  'checking canonpath');
is(canonpath("//a/b/c/../d"),   "\\\\a\\b\\d",  'checking canonpath');
is(canonpath("//a/b/c/../../d"),"\\\\a\\b\\d",  'checking canonpath');
is(canonpath("//a/b/c/.../d"),  "\\\\a\\b\\d",  'checking canonpath');
is(canonpath("/a/b/c/../../d"), "\\a\\d",       'checking canonpath');
is(canonpath("/a/b/c/.../d"),   "\\a\\d",       'checking canonpath');
is(canonpath("\\../temp\\"),    "\\temp",       'checking canonpath');
is(canonpath("\\../"),          "\\",           'checking canonpath');
is(canonpath("\\..\\"),         "\\",           'checking canonpath');
is(canonpath("/../"),           "\\",           'checking canonpath');
is(canonpath("/..\\"),          "\\",           'checking canonpath');

is(join(',', splitpath('file')),                            ",,file",                            'checking splitpath');
is(join(',', splitpath("\\d1/d2\\d3/")),                    ",\\d1/d2\\d3/,",                    'checking splitpath');
is(join(',', splitpath("d1/d2\\d3/")),                      ",d1/d2\\d3/,",                      'checking splitpath');
is(join(',', splitpath("\\d1/d2\\d3/.")),                   ",\\d1/d2\\d3/.,",                   'checking splitpath');
is(join(',', splitpath("\\d1/d2\\d3/..")),                  ",\\d1/d2\\d3/..,",                  'checking splitpath');
is(join(',', splitpath("\\d1/d2\\d3/.file")),               ",\\d1/d2\\d3/,.file",               'checking splitpath');
is(join(',', splitpath("\\d1/d2\\d3/file")),                ",\\d1/d2\\d3/,file",                'checking splitpath');
is(join(',', splitpath("d1/d2\\d3/file")),                  ",d1/d2\\d3/,file",                  'checking splitpath');
is(join(',', splitpath("C:\\d1/d2\\d3/")),                  "C:,\\d1/d2\\d3/,",                  'checking splitpath');
is(join(',', splitpath("C:d1/d2\\d3/")),                    "C:,d1/d2\\d3/,",                    'checking splitpath');
is(join(',', splitpath("C:\\d1/d2\\d3/file")),              "C:,\\d1/d2\\d3/,file",              'checking splitpath');
is(join(',', splitpath("C:d1/d2\\d3/file")),                "C:,d1/d2\\d3/,file",                'checking splitpath');
is(join(',', splitpath("C:\\../d2\\d3/file")),              "C:,\\../d2\\d3/,file",              'checking splitpath');
is(join(',', splitpath("C:../d2\\d3/file")),                "C:,../d2\\d3/,file",                'checking splitpath');
is(join(',', splitpath("\\../..\\d1/")),                    ",\\../..\\d1/,",                    'checking splitpath');
is(join(',', splitpath("\\./.\\d1/")),                      ",\\./.\\d1/,",                      'checking splitpath');
is(join(',', splitpath("\\\\node\\share\\d1/d2\\d3/")),     "\\\\node\\share,\\d1/d2\\d3/,",     'checking splitpath');
is(join(',', splitpath("\\\\node\\share\\d1/d2\\d3/file")), "\\\\node\\share,\\d1/d2\\d3/,file", 'checking splitpath');
is(join(',', splitpath("\\\\node\\share\\d1/d2\\file")),    "\\\\node\\share,\\d1/d2\\,file",    'checking splitpath');
is(join(',', splitpath("file", 1)),                         ",file,",                            'checking splitpath');
is(join(',', splitpath("\\d1/d2\\d3/", 1)),                 ",\\d1/d2\\d3/,",                    'checking splitpath');
is(join(',', splitpath("d1/d2\\d3/", 1)),                   ",d1/d2\\d3/,",                      'checking splitpath');
is(join(',', splitpath("\\\\node\\share\\d1/d2\\d3/", 1)),  "\\\\node\\share,\\d1/d2\\d3/,",     'checking splitpath');

is(join(',', splitdir('')),             ''           ,'checking splitdir');
is(join(',', splitdir("\\d1/d2\\d3/")), ',d1,d2,d3,' ,'checking splitdir');
is(join(',', splitdir("d1/d2\\d3/")),   'd1,d2,d3,'  ,'checking splitdir');
is(join(',', splitdir("\\d1/d2\\d3")),  ',d1,d2,d3'  ,'checking splitdir');
is(join(',', splitdir("d1/d2\\d3")),    'd1,d2,d3'   ,'checking splitdir');

is(catpath('', '', 'file'),                            'file'                            ,'checking catpath');
is(catpath('', "\\d1/d2\\d3/", ''),                    "\\d1/d2\\d3/"                    ,'checking catpath');
is(catpath('', "d1/d2\\d3/", ''),                      "d1/d2\\d3/"                      ,'checking catpath');
is(catpath('', "\\d1/d2\\d3/.", ''),                   "\\d1/d2\\d3/."                   ,'checking catpath');
is(catpath('', "\\d1/d2\\d3/..",''),                   "\\d1/d2\\d3/.."                  ,'checking catpath');
is(catpath('', "\\d1/d2\\d3/", '.file'),               "\\d1/d2\\d3/.file"               ,'checking catpath');
is(catpath('', "\\d1/d2\\d3/", 'file'),                "\\d1/d2\\d3/file"                ,'checking catpath');
is(catpath('', "d1/d2\\d3/", 'file'),                  "d1/d2\\d3/file"                  ,'checking catpath');
is(catpath('C:', "\\d1/d2\\d3/", ''),                  "C:\\d1/d2\\d3/"                  ,'checking catpath');
is(catpath('C:', "d1/d2\\d3/", ''),                    "C:d1/d2\\d3/"                    ,'checking catpath');
is(catpath('C:', "\\d1/d2\\d3/", 'file'),              "C:\\d1/d2\\d3/file"              ,'checking catpath');
is(catpath('C:', "d1/d2\\d3/", 'file'),                "C:d1/d2\\d3/file"                ,'checking catpath');
is(catpath('C:', "\\../d2\\d3/", 'file'),              "C:\\../d2\\d3/file"              ,'checking catpath');
is(catpath('C:', "../d2\\d3/", 'file'),                "C:../d2\\d3/file"                ,'checking catpath');
is(catpath('', "\\../..\\d1/", ''),                    "\\../..\\d1/"                    ,'checking catpath');
is(catpath('', "\\./.\\d1/", ''),                      "\\./.\\d1/"                      ,'checking catpath');
is(catpath("\\\\node\\share", "\\d1/d2\\d3/", ''),     "\\\\node\\share\\d1/d2\\d3/"     ,'checking catpath');
is(catpath("\\\\node\\share", "\\d1/d2\\d3/", 'file'), "\\\\node\\share\\d1/d2\\d3/file" ,'checking catpath');
is(catpath("\\\\node\\share", "\\d1/d2\\", 'file'),    "\\\\node\\share\\d1/d2\\file"    ,'checking catpath');


is(catfile('a', 'b', 'c'),    "a\\b\\c", 'checking catfile');
is(catfile('a', 'b', ".\\c"), "a\\b\\c", 'checking catfile');
is(catfile(".\\a", 'b', 'c'), "a\\b\\c", 'checking catfile');
is(catfile('c'),              'c', 'checking catfile');
is(catfile(".\\c"),           'c', 'checking catfile');

is(catdir(),                        ''                   ,'checking catdir');
is(catdir(''),                      "\\"                 ,'checking catdir');
is(catdir("/"),                     "\\"                 ,'checking catdir');
is(catdir("/", "../"),              "\\"                 ,'checking catdir');
is(catdir("/", "..\\"),             "\\"                 ,'checking catdir');
is(catdir("\\", "../"),             "\\"                 ,'checking catdir');
is(catdir("\\", "..\\"),            "\\"                 ,'checking catdir');
is(catdir("//d1",'d2'),             "\\\\d1\\d2"         ,'checking catdir');
is(catdir("\\d1\\",'d2'),           "\\d1\\d2"           ,'checking catdir');
is(catdir("\\d1",'d2'),             "\\d1\\d2"           ,'checking catdir');
is(catdir("\\d1","\\d2"),           "\\d1\\d2"           ,'checking catdir');
is(catdir("\\d1","\\d2\\"),         "\\d1\\d2"           ,'checking catdir');
is(catdir('',"/d1",'d2'),           "\\\\d1\\d2"         ,'checking catdir');
is(catdir('','',"/d1",'d2'),        "\\\\\\d1\\d2"       ,'checking catdir');
is(catdir('',"//d1",'d2'),          "\\\\\\d1\\d2"       ,'checking catdir');
is(catdir('','',"//d1",'d2'),       "\\\\\\\\d1\\d2"     ,'checking catdir');
is(catdir('','d1','','d2',''),      "\\d1\\d2"           ,'checking catdir');
is(catdir('','d1','d2','d3',''),    "\\d1\\d2\\d3"       ,'checking catdir');
is(catdir('d1','d2','d3',''),       "d1\\d2\\d3"         ,'checking catdir');
is(catdir('','d1','d2','d3'),       "\\d1\\d2\\d3"       ,'checking catdir');
is(catdir('d1','d2','d3'),          "d1\\d2\\d3"         ,'checking catdir');
is(catdir("A:/d1",'d2','d3'),       "A:\\d1\\d2\\d3"     ,'checking catdir');
is(catdir("A:/d1",'d2','d3',''),    "A:\\d1\\d2\\d3"     ,'checking catdir');
is(catdir("A:/d1","B:/d2",'d3',''), "A:\\d1\\B:\\d2\\d3" ,'checking catdir');
is(catdir("A:/"),                   "A:\\"               ,'checking catdir');
is(catdir("\\", 'foo'),             "\\foo"              ,'checking catdir');

is(rel2abs('temp', "C:/"),                       "C:\\temp"                     ,'checking real2abs');
is(rel2abs('temp', "C:/a"),                      "C:\\a\\temp"                  ,'checking real2abs');
is(rel2abs('temp', "C:/a/"),                     "C:\\a\\temp"                  ,'checking real2abs');
is(rel2abs("../",  "C:/"),                       "C:\\"                         ,'checking real2abs');
is(rel2abs("../", "C:/a"),                       "C:\\"                         ,'checking real2abs');
is(rel2abs('temp', "//prague_main/work/"),       "\\\\prague_main\\work\\temp" ,'checking real2abs');
is(rel2abs("../temp", "//prague_main/work/"),    "\\\\prague_main\\work\\temp" ,'checking real2abs');
is(rel2abs('temp', "//prague_main/work"),        "\\\\prague_main\\work\\temp" ,'checking real2abs');
is(rel2abs("../", "//prague_main/work"),         "\\\\prague_main\\work"       ,'checking real2abs');

is(abs2rel("/t1/t2/t3", "/t1/t2/t3"),         ''                       ,'checking abs2rel', :todo<bug>);
is(abs2rel("/t1/t2/t4", "/t1/t2/t3"),         "..\\t4"                 ,'checking abs2rel', :todo<bug>);
is(abs2rel("/t1/t2", "/t1/t2/t3"),            '..'                     ,'checking abs2rel', :todo<bug>);
is(abs2rel("/t1/t2/t3/t4", "/t1/t2/t3"),      't4'                     ,'checking abs2rel', :todo<bug>);
skip(5, 'erratic behaviour on win32 vs non-win32 - skip for release');
#is(abs2rel("/t4/t5/t6", "/t1/t2/t3"),         "..\\..\\..\\t4\\t5\\t6" ,'checking abs2rel', :todo<bug>);
#is(abs2rel("/", "/t1/t2/t3"),                 "..\\..\\.."             ,'checking abs2rel', :todo<bug>);
#is(abs2rel("///", "/t1/t2/t3"),               "..\\..\\.."             ,'checking abs2rel', :todo<bug>);
#is(abs2rel("/.", "/t1/t2/t3"),                "..\\..\\.."             ,'checking abs2rel', :todo<bug>);
#is(abs2rel("/./", "/t1/t2/t3"),               "..\\..\\.."             ,'checking abs2rel', :todo<bug>);
is(abs2rel("\\\\a/t1/t2/t4", "/t2/t3"),       "\\\\a\\t1\\t2\\t4"      ,'checking abs2rel');
is(abs2rel("//a/t1/t2/t4", "/t2/t3"),         "\\\\a\\t1\\t2\\t4"      ,'checking abs2rel');
is(abs2rel("A:/t1/t2/t3", "A:/t1/t2/t3"),     ''                       ,'checking abs2rel', :todo<bug>);
is(abs2rel("A:/t1/t2/t3/t4", "A:/t1/t2/t3"),  't4'                     ,'checking abs2rel', :todo<bug>);
is(abs2rel("A:/t1/t2/t3", "A:/t1/t2/t3/t4"),  '..'                     ,'checking abs2rel', :todo<bug>);
is(abs2rel("A:/t1/t2/t3", "B:/t1/t2/t3"),     "A:\\t1\\t2\\t3"         ,'checking abs2rel');
is(abs2rel("A:/t1/t2/t3/t4", "B:/t1/t2/t3"),  "A:\\t1\\t2\\t3\\t4"     ,'checking abs2rel');
is(abs2rel("E:/foo/bar/baz"),                 "E:\\foo\\bar\\baz"      ,'checking abs2rel');

