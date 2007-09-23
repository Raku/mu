/**
    Perl6 Syntax Highlighter Brush for syntaxhighlighter
    
    This is really a first attempt at the problem in 
    client-side. This component maybe used in the future by runpugs.
    
    Written by: Ahmad M. Zawawi <ahmad.zawawi@gmail.com>
    
    Version 0.1
        Parses operators,builtins, keywords, single comments
        Has some problems with symbols and variables [TODO]
        Has a bug with html escapes (like &lt and &gt) [TODO]
    
    Adapted from:
        pugs/util/perl6.vim 
            (Luke Palmer <fibonaci@babylonia.flatirons.org>)
            (Moritz Lenz <moritz@faui2k3.org>)
        Thanks guys... ;-)
    
    Resources:    
        syntaxhighlighter (GPL license)
        http://code.google.com/p/syntaxhighlighter/
*/
dp.sh.Brushes.p6 = function()
{
    //p6KeyCompare
    //this is commented since syntaxhiglighter has a problem with &gt & &lt
    //var operators = "eq ne lt le gt ge cmp == != <= >=";

    //p6KeyFunc       
    var builtins = "substr index rindex " +
    "grep map sort join split reduce min max reverse truncate zip " +
    "cat roundrobin classify first " +
    "keys values pairs defined delete exists elems end kv " +
    "arity assuming gather take any pick all none " +
    "pop push shift splice unshift " +
    "abs exp log log10 rand sign sqrt sin cos tan " + 
    "floor ceil round srand roots cis unpolar polar " +
    "p5chop chop p5chomp chomp lc lcfirst uc ucfirst " +
    "capitalize normalize pack unpack quotemeta comb " +
    "nfd nfc nfkd nfkc " +
    "printf sprintf caller evalfile run runinstead " +
    "nothing want bless chr ord list item gmtime " +
    "localtime time gethost getpw chroot getlogin kill " +
    "fork wait perl context";

    // a keyword for every task ;-)
	var keywords =	
    //p6Module
    "module class role use require package enum grammar " +

    //p6KeyDecl       
    "coro sub submethod method is but does trusts multi " +

    //p6KeyDecl       
    "rule token regex category " +

    //p6KeyScopeDecl
    "let my our state temp has constant proto " +

    //p6KeyFlow
    "if else elsif unless " +  
    "for foreach loop while until when next last redo " +
    "given not or and andthen orelse xor return default exit " +

    //p6ClosureTrait
    "BEGIN CHECK INIT START FIRST ENTER LEAVE KEEP UNDO NEXT LAST " +
    "PRE POST END rw signature returns of parsed cached " +
    "readonly ref copy " +
    "inline tighter looser equiv assoc " +
    "deep also " +

    //p6KeyException  
    "die fail try CATCH CONTROL warn " +

    //p6KeyIO         
    "print open read write readline say seek close slurp " +
    "opendir readdir " +

    //p6KeyProperty
    "constant prec key value irs ofs ors pos export " +
    "float int str true false int1 int2 int4 int8 " +
    "int16 int32 int64 uint1 uint2 uint4 uint8 uint16 " +
    "uint32 uint64 num16 num32 num64 complex16 complex32 " +
    "complex64 complex128 buf8 buf16 buf32 buf64 " +
    "WHAT HOW " +

    //p6KeyType       
    "Array Bool Class Code Hash Int IO Num NumRange " +
    "Str StrRange Sub Role Rule Rat Complex Any " +
    "Scalar List " +

    //p6KeySpecial    
    "eval operator undef undefine " +
    "infix postfix prefix cirumfix postcircumfix"; 

            
	this.regexList = [
		{ 
            // one line comments
            regex: dp.sh.RegexLib.SingleLinePerlComments,							
            css: 'comment' 
        },		
		{ 
            // multiline comments
            regex: new RegExp('=\\w[\\s\\S]+=cut', 'gmi'),
            css: 'comment' 
        },
		{ 
            // $global, @array, and %hash variables
            regex: new RegExp('(\\$|@|%)\\w+', 'g'),				
            css: 'variable' 
        },	
		{ 
            // strings
            regex: dp.sh.RegexLib.DoubleQuotedString,								
            css: 'string' 
        },		
		{ 
            // strings
            regex: dp.sh.RegexLib.SingleQuotedString,								
            css: 'string' 
        },		
/*		{ 
            // multi-line strings
            regex: new RegExp('"[\\s\\S]*"', 'gmi'),
            css: 'string' 
        },		*/
		{ 
            // numbers
            regex: new RegExp('\\b([\\d]+(\\.[\\d]+)?|0x[a-f0-9]+)\\b', 'gi'),	
            css: 'number' 
        },
        /*
        //disabled code... wont work.
		{ 
            // p6 comparison operators...
            regex: new RegExp(this.GetKeywords(operators), 'gm'),					
            css: 'op' 
        },*/
    	{ 
            // Lots of perl6 keywords ;-)
            regex: new RegExp(this.GetKeywords(keywords), 'gm'),					
            css: 'keyword' 
        },
		{ 
            // builtins
            regex: new RegExp(this.GetKeywords(builtins), 'gm'),	
            css: 'builtin' 
        },
        { 
            // symbols  
            regex: new RegExp(' [a-z][A-Za-z0-9_]*[\-A-Za-z0-9_]*', 'g'),		
            css: 'symbol' 
        }

    ];

	this.CssClass = 'dp-p6';
	this.Style =	'.dp-p6 .builtin { color: orange; }' +
                    '.dp-p6 .symbol { color: yellow; }' +
					'.dp-p6 .op { color: #808080; }' +
					'.dp-p6 .number { color: #C00000; }' +
                    '.dp-p6 .variable { color: yellow; font-weight: bold; }';
}

dp.sh.Brushes.p6.prototype	= new dp.sh.Highlighter();
dp.sh.Brushes.p6.Aliases	= ['perl6','p6'];
