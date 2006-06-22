General Information on this directory/project:

Perl 5 to Perl 6 Translation
Sage LaTorra
Google Summer of Code 2006

Any changes and comments are welcome to anything in this directory, except 
translate-sage.hs which is my personal working copy, should I need it. Everything
else is up for grabs.

Description of files:

AST_Description.txt - A description of the AST produced by Larry Wall's P5 parser.
The info in this file is both to help me remember what's going on and to aid anybody
else picking up development.

ConversionStageOne.txt - A description of the translations I'm trying to accomplish
in the first stage of this project. I'll try to keep it updated so that finished 
translations are marked as such.

TestInit.pm - This is a sample output of the reconstructed, P6 version of TestInit.pm
from the /t directory of the P5 source. It's not fully P6 (yet), but it's supposed to
show what progress I've/We've made.

TestInit.pm.yaml - The AST output of Larry's parser. This is the first sample input
I'm working on. I have parsed versions of the rest of /t, but for now this is my
main test file.

translate.hs - The actual haskell code. Currently it's very simple, just load it
into ghci and run 'mainParse infile outfile', I use 
'mainParse "TestInit.pm.yml" "TestInit.pm"' for tests at this point. It's very 
verbose, since code is still very rough. Changes are welcome.

translate-sage.hs - My working copy. Just so I can keep track of my own changes.
Don't mess with it, make changes to translate.hs.

