" Vim syntax file
" Language:     Perl 6
" Last Change:  Dec 24th 2008
" Contributors: Luke Palmer <fibonaci@babylonia.flatirons.org>
"               Moritz Lenz <moritz@faui2k3.org>
"               Hinrik Örn Sigurðsson <hinrik.sig@gmail.com>
"                
" This is a big undertaking. Perl 6 is the sort of language that only Perl
" can parse. But I'll do my best to get vim to.
"
" You can associate the extension ".pl" with the filetype "perl6" by setting
"     autocmd BufNewFile,BufRead *.pl setf perl6
" in your ~/.vimrc. But that will infringe on Perl 5, so you might want to
" put a modeline near the beginning or end of your Perl 6 files instead:
"     # vim: filetype=perl6

" TODO:
"   * Add more support for folding
"   * Add more syntax syncing hooks
"   * Overhaul regexes, S05
"   * Q//:
"       :to, :heredoc
"       interpolate \q:s{$scalar} and such
"
" Impossible TODO?:
"   * Unspace
"   * Anything that allows characters outside ascii/latin1
"   * Selective highlighting of Pod formatting codes with the :allow option
"   * Arbitrary number, order, and negation of adverbs to Q//, q//, qq//.
"     Currently only the first adverb is considered significant. Anything
"     more would require an exponential amount of regexes, making this
"     already slow syntax file even slower.

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" this should eventually be put in a $VIMRUNTIME/indent/perl6.vim file
setlocal autoindent expandtab smarttab shiftround shiftwidth=4 softtabstop=4

" this belongs in $VIMRUNTIME/ftplugin/perl6.vim
setlocal iskeyword=@,48-57,_,192-255

" identifiers
syn match p6Normal display "\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*"

" This is used in the for loops below
" Don't use the "syn keyword" construct because that always has higher
" priority than matches/regions, so the words can't be autoquoted with
" the "=>" and "p5=>" operators. All the lookaround stuff is to make sure
" we don't match them as part of some other identifier.
let s:before_keyword = " display \"\\%(\\k\\|\\%(\\k\\d\\@<!\\)\\@<=[-']\\)\\@<!\\%("
let s:after_keyword = "\\)\\%(\\k\\|[-']\\%(\\k\\d\\@<!\\)\\@=\\)\\@!\""

" Billions of keywords
let s:keywords = [
 \ ["p6Attention",      "ACHTUNG ATTN ATTENTION FIXME"],
 \ ["p6Attention",      "NB TODO TBD WTF XXX NOTE"],
 \ ["p6DeclareRoutine", "macro sub submethod method multi only rule"],
 \ ["p6DeclareRoutine", "token regex category"],
 \ ["p6Module",         "module class role use require package enum"],
 \ ["p6Module",         "grammar subset self"],
 \ ["p6Conditional",    "if else elsif unless"],
 \ ["p6VarStorage",     "let my our state temp has proto constant"],
 \ ["p6Repeat",         "for loop repeat while until gather given"],
 \ ["p6FlowControl",    "take do when next last redo return lastcall"],
 \ ["p6FlowControl",    "default exit make continue break goto leave async"],
 \ ["p6FlowControl",    "contend maybe defer"],
 \ ["p6TypeConstraint", "is as but does trusts of returns also handles where"],
 \ ["p6ClosureTrait",   "BEGIN CHECK INIT START FIRST ENTER LEAVE KEEP"],
 \ ["p6ClosureTrait",   "UNDO NEXT LAST PRE POST END CATCH CONTROL TEMP"],
 \ ["p6Exception",      "die fail try warn"],
 \ ["p6Property",       "prec irs ofs ors export deep binary unary reparsed"],
 \ ["p6Property",       "rw parsed cached readonly instead defequiv will"],
 \ ["p6Property",       "ref copy inline tighter looser equiv assoc required"],
 \ ["p6Number",         "NaN Inf"],
 \ ["p6Pragma",         "oo"],
 \ ["p6Type",           "Object Any Junction Whatever Capture Match"],
 \ ["p6Type",           "Signature Proxy Matcher Package Module Class"],
 \ ["p6Type",           "Grammar Scalar Array Hash KeyHash KeySet KeyBag"],
 \ ["p6Type",           "Pair List Seq Range Set Bag Mapping Void Undef"],
 \ ["p6Type",           "Failure Exception Code Block Routine Sub Macro"],
 \ ["p6Type",           "Method Submethod Regex Str Blob Char Byte"],
 \ ["p6Type",           "Codepoint Grapheme StrPos StrLen Version Num"],
 \ ["p6Type",           "Complex num complex Bit bit bool True False"],
 \ ["p6Type",           "Increasing Decreasing Ordered Callable AnyChar"],
 \ ["p6Type",           "Positional Associative Ordering KeyExtractor"],
 \ ["p6Type",           "Comparator OrderingPair IO KitchenSink"],
 \ ["p6Type",           "Int int int1 int2 int4 int8 int16 int32 int64"],
 \ ["p6Type",           "Rat rat rat1 rat2 rat4 rat8 rat16 rat32 rat64"],
 \ ["p6Type",           "Buf buf buf1 buf2 buf4 buf8 buf16 buf32 buf64"],
 \ ["p6Type",           "UInt uint1 uint2 uint4 uint8 uint16 uint32 uint64"],
 \ ["p6Operator",       "div x xx mod also leg cmp before after eq ne lt"],
 \ ["p6Operator",       "gt ge eqv ff fff true not and andthen Z X or xor"],
 \ ["p6Operator",       "orelse extra Q Qq Qqq q qq Qw qw qqw Qww qww qqww"],
 \ ["p6Operator",       "Qx qx qqx Qs qs qqs Qa qa qqa Qh qh qqh Qf qf qqf"],
 \ ["p6Operator",       "Qc qc qqc Qb qb qqb Qto qto qqto"],
\ ]

for [group, string] in s:keywords
    let s:word_list = split(string)
    let s:words = join(s:word_list, "\\|")
    exec "syn match ". group ." ". s:before_keyword . s:words . s:after_keyword
endfor

" More operators
" Don't put a "\+" at the end of the character class. That makes it so
" greedy that the "%" " in "+%foo" won't be allowed to match as a sigil,
" among other things
syn match p6Operator display "[-+/*~?|=^!%&,<>.;\\]"
syn match p6Operator display "\%(:\@<!::\@!\|::=\|\.::\)"
" these require whitespace on the left side
syn match p6Operator display "\%(\s\|^\)\@<=\%(xx=\|p5=>\)"
" "i" requires a digit to the left, and no keyword char to the right
syn match p6Operator display "\d\@<=i\k\@!"
" index overloading
syn match p6Operator display "\%(&\.(\@=\|@\.\[\@=\|%\.{\@=\)"
" reduce, may need to add more to this
syn match p6Operator display "\k\@<!\[\\\?\%(\*\|\*\*\|/\|%\|x\|xx\|+&\|+<\|+>\|\~&\|\~<\|\~>\|+\|-\|\~\|=>\)]\%(«\|<<\)\?"
syn match p6Operator display "\k\@<!\[\\\?\%(+|\|+\^\|\~|\|\~\^\|&\||\|\^\|!==\|==\|before\|after\|<\|<=\|p5=>\)]\%(«\|<<\)\?"
syn match p6Operator display "\k\@!\[\\\?\%(>\|>=\|\~\~\|!\~\~\|eq\|!eq\|ne\|lt\|le\|gt\|ge\|=:=\|!=:=\|or\)]\%(«\|<<\)\?"
syn match p6Operator display "\k\@!\[\\\?\%(===\|!===\|eqv\|!eqv\|&&\|||\|\^\^\|//\|min\|max\|=\|!=\|,\|Z\)]\%(«\|<<\)\?"
" the =<> operator
syn region p6Iterate
    \ matchgroup=p6Operator
    \ start="=<<\@!"
    \ skip="<[^>]>"
    \ end=">"
    \ transparent

" some standard packages
syn match p6Type display "\%(::\|\k\|\%(\k\d\@<!\)\@<=[-']\)\@<!\%(Order\%(::Same\|::Increase\|::Decrease\)\?\)\%(\k\|[-']\%(\k\d\@<!\)\@=\)\@!"
syn match p6Type display "\%(::\|\k\|\%(\k\d\@<!\)\@<=[-']\)\@<!\%(Bool\%(::True\|::False\)\?\)\%(\k\|[-']\%(\k\d\@<!\)\@=\)\@!"

let s:routines = [
 \ ["eager hyper substr index rindex grep map sort join lines hints chmod"],
 \ ["split reduce min max reverse truncate zip cat roundrobin classify"],
 \ ["first sum keys values pairs defined delete exists elems end kv any"],
 \ ["all one wrap shape key value name pop push shift splice unshift floor"],
 \ ["ceiling abs exp log log10 rand sign sqrt sin cos tan round strand"],
 \ ["roots cis unpolar polar atan2 pick chop p5chop chomp p5chomp lc"],
 \ ["lcfirst uc ucfirst capitalize normalize pack unpack quotemeta comb"],
 \ ["samecase sameaccent chars nfd nfc nfkd nfkc printf sprintf caller"],
 \ ["evalfile run runinstead nothing want bless chr ord gmtime time eof"],
 \ ["localtime gethost getpw chroot getlogin getpeername kill fork wait"],
 \ ["perl graphs codes bytes clone print open read write readline say seek"],
 \ ["close opendir readdir slurp pos fmt vec link unlink symlink uniq pair"],
 \ ["asin atan sec cosec cotan asec acosec acotan sinh cosh tanh asinh"],
 \ ["acos acosh atanh sech cosech cotanh sech acosech acotanh asech ok"],
 \ ["plan_ok dies_ok lives_ok skip todo pass flunk force_todo use_ok isa_ok"],
 \ ["diag is_deeply isnt like skip_rest unlike cmp_ok eval_dies_ok nok_error"],
 \ ["eval_lives_ok approx is_approx throws_ok version_lt plan eval succ pred"],
 \ ["times nonce once signature new connect operator undef undefine sleep"],
 \ ["from to infix postfix prefix circumfix postcircumfix minmax lazy count"],
 \ ["unwrap getc pi e context void quasi body each contains rewinddir subst"],
 \ ["can isa flush arity assuming rewind callwith callsame nextwith nextsame"],
\ ]

" q() or whatever() is always a function call
syn match p6Normal display "\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*(\@="

" we want to highlight builtins like split() though, so this comes afterwards
for [string] in s:routines
    let s:word_list = split(string)
    let s:words = join(s:word_list, "\\|")
    exec "syn match p6Routine ". s:before_keyword . s:words . s:after_keyword
endfor

" packages, must come after all the keywords
syn match p6Normal display "\%(::\)\@<=\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*"
syn match p6Normal display "\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\%(::\)\@="

syn match p6Shebang    display "\%^#!.*"
syn match p6BlockLabel display "\%(\s\|^\)\@<=\h\w*\s*::\@!\_s\@="
syn match p6Number     display "\k\@<!_\@!\%(\d\|__\@!\)\+_\@<!\%([eE]_\@!+\?\%(\d\|_\)\+\)\?_\@<!"
syn match p6Float      display "\k\@<!_\@!\%(\d\|__\@!\)\+_\@<![eE]_\@!-\%(\d\|_\)\+"
syn match p6Float      display "\k\@<!_\@<!\%(\d\|__\@!\)*_\@<!\.\@<!\._\@!\.\@!\a\@!\%(\d\|_\)\+_\@<!\%([eE]_\@!\%(\d\|_\)\+\)\?"

syn match p6NumberBase display "[obxd]" contained
syn match p6Number     display "\<0\%(o[0-7][0-7_]*\)\@="     nextgroup=p6NumberBase
syn match p6Number     display "\<0\%(b[01][01_]*\)\@="       nextgroup=p6NumberBase
syn match p6Number     display "\<0\%(x\x[[:xdigit:]_]*\)\@=" nextgroup=p6NumberBase
syn match p6Number     display "\<0\%(d\d[[:digit:]_]*\)\@="  nextgroup=p6NumberBase
syn match p6Number     display "\%(\<0o\)\@<=[0-7][0-7_]*"
syn match p6Number     display "\%(\<0b\)\@<=[01][01_]*"
syn match p6Number     display "\%(\<0x\)\@<=\x[[:xdigit:]_]*"
syn match p6Number     display "\%(\<0d\)\@<=\d[[:digit:]_]*"

" try to distinguish the "is" function from the "is" trail auxiliary
syn match p6Routine     display "\%(\%(^\|{\)\s*\)\@<=is\k\@!"

" int is a function sometimes
syn match p6Routine     display "\<int\%(\s*(\|\s\+\d\)\@="

" these Routine names are also Properties, if preceded by "is"
syn match p6Property    display "\%(is\s\+\)\@<=\%(signature\|context\|also\|shape\)"

" The sigil in ::*Package
syn match p6PackageTwigil display "\%(::\)\@<=\*"

" $!, $var, $!var, $::var, $package::var $*::package::var, etc
syn match p6Sigil        display "\%(&\|@@\|[@$%]\$*\)\%(::\|\%(\$\@<=\d\+\|!\|/\|¢\)\|\%(\%([.^*+?=!]\|:\@<!::\@!\)\k\d\@<!\)\|\%(\k\d\@<!\)\)\@=" nextgroup=p6PunctVar,p6Twigil,p6Variable,p6PackageScope
syn match p6PunctVar     display "\%(\$\@<=\d\+\|!\|/\|¢\)\%(\k\d\@<!\)\@!" contained
syn match p6Variable     display "\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*" contained
syn match p6Twigil       display "\%([.^*+?=!]\|:\@<!::\@!\)\%(\k\d\@<!\)\@=" nextgroup=p6PackageScope,p6Variable contained
syn match p6PackageScope display "\%(\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\)\?::" nextgroup=p6PackageScope,p6Variable contained

" $<match>
syn region p6MatchSigil
    \ matchgroup=p6Sigil
    \ start="\$\%(<<\@!\)\@="
    \ end=">\@<="
    \ contains=p6Match

syn region p6Match
    \ matchgroup=p6Twigil
    \ start="<"
    \ end=">"
    \ contained

syn match p6CustomRoutine display "\%(\<\%(sub\|method\|submethod\|macro\|rule\|regex\|token\)\s\+\)\@<=\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*"
syn match p6CustomRoutine display "\%(\<\%(multi\|proto\|only\)\s\+\)\@<=\%(\%(sub\|method\|submethod\|macro\|rule\|regex\|token\)\>\)\@!\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*"

" Contextualizers
syn match p6Context display "\%([$@%&]\|\k\)\@<!\%(\$\|@\|%\|@@\|&\)\_s\@="
syn match p6Context display "\<\%(item\|list\|slice\|hash\)\>"
syn match p6Context display "\%(\$\|@\|%\|&\|@@\)(\@="

" the "$" placeholder in "$var1, $, var2 = @list"
syn match p6Placeholder display "\%(,\s*\)\@<=\$\%(\k\d\@<!\|\%([.^*+?=!]\|:\@<!::\@!\)\)\@!"
syn match p6Placeholder display "\$\%(\k\d\@<!\|\%([.^*+?=!]\|:\@<!::\@!\)\)\@!\%(,\s*\)\@="

" Comments

" normal end-of-line comment
syn match p6Comment display "#.*" contains=p6Attention

" Multiline comments. Arbitrary numbers of opening brackets are allowed,
" but we only define regions for 1 to 3
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#("
    \ skip="([^)]*)"
    \ end=")"
    \ matchgroup=p6Error
    \ start="^#("
    \ contains=p6Attention,p6Comment
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#(("
    \ skip="((\%([^)\|))\@!]\)*))"
    \ end="))"
    \ matchgroup=p6Error
    \ start="^#(("
    \ contains=p6Attention,p6Comment
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#((("
    \ skip="(((\%([^)]\|)\%())\)\@!\)*)))"
    \ end=")))"
    \ matchgroup=p6Error
    \ start="^#((("
    \ contains=p6Attention,p6Comment

syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#\["
    \ skip="\[[^\]]*]"
    \ end="]"
    \ matchgroup=p6Error
    \ start="^#\["
    \ contains=p6Attention,p6Comment
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#\[\["
    \ skip="\[\[\%([^\]]\|]]\@!\)*]]"
    \ end="]]"
    \ matchgroup=p6Error
    \ start="^#\[\["
    \ contains=p6Attention,p6Comment
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#\[\[\["
    \ skip="\[\[\[\%([^\]]\|]\%(]]\)\@!\)*]]]"
    \ end="]]]"
    \ matchgroup=p6Error
    \ start="^#\[\[\["
    \ contains=p6Attention,p6Comment

syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#{"
    \ skip="{[^}]*}"
    \ end="}"
    \ matchgroup=p6Error
    \ start="^#{"
    \ contains=p6Attention,p6Comment
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#{{"
    \ skip="{{\%([^}]\|}}\@!\)*}}"
    \ end="}}"
    \ matchgroup=p6Error
    \ start="^#{{"
    \ contains=p6Attention,p6Comment
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#{{{"
    \ skip="{{{\%([^}]\|}\%(}}\)\@!\)*}}}"
    \ end="}}}"
    \ matchgroup=p6Error
    \ start="^#{{{"
    \ contains=p6Attention,p6Comment

syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#<"
    \ skip="<[^>]*>"
    \ end=">"
    \ matchgroup=p6Error
    \ start="^#<"
    \ contains=p6Attention,p6Comment
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#<<"
    \ skip="<<\%([^>]\|>>\@!\)*>>"
    \ end=">>"
    \ matchgroup=p6Error
    \ start="^#<<"
    \ contains=p6Attention,p6Comment
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#<<<"
    \ skip="<<<\%([^>]\|>\%(>>\)\@!\)*>>>"
    \ end=">>>"
    \ matchgroup=p6Error
    \ start="^#<<<"
    \ contains=p6Attention,p6Comment

syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#«"
    \ skip="«[^»]*»"
    \ end="»"
    \ matchgroup=p6Error
    \ start="^#«"
    \ contains=p6Attention,p6Comment
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#««"
    \ skip="««\%([^»]\|»»\@!\)*»»"
    \ end="»»"
    \ matchgroup=p6Error
    \ start="^#««"
    \ contains=p6Attention,p6Comment
syn region p6Comment
    \ matchgroup=p6Comment
    \ start="^\@<!#«««"
    \ skip="«««\%([^»]\|»\%(»»\)\@!\)*»»»"
    \ end="»»»"
    \ matchgroup=p6Error
    \ start="^#«««"
    \ contains=p6Attention,p6Comment

" Quoting

" one cluster for every quote adverb
syn cluster p6Interp_s
    \ add=p6InterpScalar
syn cluster p6Interp_scalar
    \ add=p6InterpScalar

syn cluster p6Interp_a
    \ add=p6InterpArray
syn cluster p6Interp_array
    \ add=p6InterpArray

syn cluster p6Interp_h
    \ add=p6InterpHash
syn cluster p6Interp_hash
    \ add=p6InterpHash

syn cluster p6Interp_f
    \ add=p6InterpFunction
syn cluster p6Interp_f
    \ add=p6InterpFunction

syn cluster p6Interp_c
    \ add=p6InterpClosure
syn cluster p6Interp_closure
    \ add=p6InterpClosure

syn cluster p6Interp_ww
    \ add=p6StringSQ
    \ add=p6StringDQ
syn cluster p6Interp_quotewords
    \ add=p6StringSQ
    \ add=p6StringDQ

syn cluster p6Interp_q
    \ add=p6EscQQ
    \ add=p6EscBackSlash
syn cluster p6Interp_single
    \ add=p6EscQQ
    \ add=p6EscBackSlash

syn cluster p6Interp_b
    \ add=@p6Interp_q
    \ add=p6Escape
syn cluster p6Interp_backslash
    \ add=@p6Interp_q
    \ add=p6Escape

syn cluster p6Interp_qq
    \ add=@p6Interp_scalar
    \ add=@p6Interp_array
    \ add=@p6Interp_hash
    \ add=@p6Interp_function
    \ add=@p6Interp_closure
    \ add=@p6Interp_backslash
    \ add=p6EscOpenCurly
    \ add=p6EscCodePoint
    \ add=p6EscHex
    \ add=p6EscOct
syn cluster p6Interp_double
    \ add=@p6Interp_scalar
    \ add=@p6Interp_array
    \ add=@p6Interp_hash
    \ add=@p6Interp_function
    \ add=@p6Interp_closure
    \ add=@p6Interp_backslash
    \ add=p6EscOpenCurly
    \ add=p6EscCodePoint
    \ add=p6EscHex
    \ add=p6EscOct

syn region p6InterpScalar
    \ start="\ze\z(\$\%(\%(\d\+\|!\|/\|¢\)\|\%(\%(\%([.^*+?=!]\|:\@<!::\@!\)\%(\k\d\@<!\)\@=\)\?\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\%(\.\%(\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\)\|\%(([^)]*)\|\[[^\]]*]\|<[^>]*>\|«[^»]*»\|{[^}]*}\)\)*\)\.\?\%(([^)]*)\|\[[^\]]*]\|<[^>]*>\|«[^»]*»\|{[^}]*}\)\)\)"
    \ start="\ze\z(\$\%(\%(\d\+\|!\|/\|¢\)\|\%(\%(\%([.^*+?=!]\|:\@<!::\@!\)\%(\k\d\@<!\)\@=\)\?\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\)\)\)"
    \ end="\z1\zs"
    \ contained
    \ contains=TOP
    \ keepend

syn region p6InterpScalar
    \ matchgroup=p6Context
    \ start="\$\ze("
    \ skip="([^)]*)"
    \ end=")\zs"
    \ contained
    \ contains=TOP

syn region p6InterpArray
    \ start="\ze\z(@\$*\%(\%(!\|/\|¢\)\|\%(\%(\%([.^*+?=!]\|:\@<!::\@!\)\%(\k\d\@<!\)\@=\)\?\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\%(\.\%(\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\)\|\%(([^)]*)\|\[[^\]]*]\|<[^>]*>\|«[^»]*»\|{[^}]*}\)\)*\)\.\?\%(([^)]*)\|\[[^\]]*]\|<[^>]*>\|«[^»]*»\|{[^}]*}\)\)\)"
    \ end="\z1\zs"
    \ contained
    \ contains=TOP
    \ keepend

syn region p6InterpArray
    \ matchgroup=p6Context
    \ start="@\ze("
    \ start="@@\ze("
    \ skip="([^)]*)"
    \ end=")\zs"
    \ contained
    \ contains=TOP

syn region p6InterpHash
    \ start="\ze\z(%\$*\%(\%(!\|/\|¢\)\|\%(\%(\%([.^*+?=!]\|:\@<!::\@!\)\%(\k\d\@<!\)\@=\)\?\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\%(\.\%(\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\)\|\%(([^)]*)\|\[[^\]]*]\|<[^>]*>\|«[^»]*»\|{[^}]*}\)\)*\)\.\?\%(([^)]*)\|\[[^\]]*]\|<[^>]*>\|«[^»]*»\|{[^}]*}\)\)\)"
    \ end="\z1\zs"
    \ contained
    \ contains=TOP
    \ keepend

syn region p6InterpHash
    \ matchgroup=p6Context
    \ start="%\ze("
    \ skip="([^)]*)"
    \ end=")\zs"
    \ contained
    \ contains=TOP

syn region p6InterpFunction
    \ start="\ze\z(&\%(\%(!\|/\|¢\)\|\%(\%(\%([.^*+?=!]\|:\@<!::\@!\)\%(\k\d\@<!\)\@=\)\?\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\%(\.\%(\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\)\|\%(([^)]*)\|\[[^\]]*]\|<[^>]*>\|«[^»]*»\|{[^}]*}\)\)*\)\.\?\%(([^)]*)\|\[[^\]]*]\|<[^>]*>\|«[^»]*»\|{[^}]*}\)\)\)"
    \ end="\z1\zs"
    \ contained
    \ contains=TOP
    \ keepend

syn region p6InterpFunction
    \ matchgroup=p6Context
    \ start="&\ze("
    \ skip="([^)]*)"
    \ end=")\zs"
    \ contained
    \ contains=TOP

syn region p6InterpClosure
    \ start="\\\@<!{"
    \ skip="{[^}]*}"
    \ end="}"
    \ contained
    \ contains=TOP
    \ keepend

" generic escape
syn match p6Escape          display "\\\w" contained

" escaped closing delimiters
syn match p6EscQuote        display "\\'" contained
syn match p6EscDoubleQuote  display "\\\"" contained
syn match p6EscBackTick     display "\\`" contained
syn match p6EscForwardSlash display "\\/" contained
syn match p6EscPipe         display "\\|" contained
syn match p6EscExclamation  display "\\!" contained
syn match p6EscDollar       display "\\\$" contained
syn match p6EscCloseAngle   display "\\>" contained
syn match p6EscCloseFrench  display "\\»" contained
syn match p6EscCloseCurly   display "\\}" contained
syn match p6EscCloseBracket display "\\\]" contained

" misc escapes
syn match p6EscCodePoint display "\%(\\c\)\@<=\%(\d\|\S\|\[\)\@=" contained nextgroup=p6CodePoint
syn match p6EscHex       display "\%(\\x\)\@<=\%(\x\|\[\)\@=" contained nextgroup=p6HexSequence
syn match p6EscOct       display "\%(\\o\)\@<=\%(\o\|\[\)\@=" contained nextgroup=p6OctSequence
syn match p6EscQQ        display "\\qq" contained nextgroup=p6QQSequence
syn match p6EscOpenCurly display "\\{" contained
syn match p6EscHash      display "\\#" contained
syn match p6EscBackSlash display "\\\\" contained

syn region p6QQSequence
    \ matchgroup=p6Escape
    \ start="\["
    \ skip="\[[^\]]*]"
    \ end="]"
    \ contained
    \ transparent
    \ contains=@p6Interp_qq

syn match p6CodePoint display "\%(\d\+\|\S\)" contained
syn region p6CodePoint
    \ matchgroup=p6Escape
    \ start="\["
    \ end="]"
    \ contained

syn match p6HexSequence display "\x\+" contained
syn region p6HexSequence
    \ matchgroup=p6Escape
    \ start="\["
    \ end="]"
    \ contained

syn match p6OctSequence display "\o\+" contained
syn region p6OctSequence
    \ matchgroup=p6Escape
    \ start="\["
    \ end="]"
    \ contained

" matches :key, :!key, :$var, :key<var>, etc
syn region p6Adverb
    \ start="\ze\z(:!\?\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\%(([^)]*)\|\[[^\]]*]\|<[^>]*>\|«[^»]*»\|{[^}]*}\)\?\)"
    \ start="\ze\z(:!\?[@$%]\$*\%(::\|\%(\$\@<=\d\+\|!\|/\|¢\)\|\%(\%([.^*+?=!]\|:\@<!::\@!\)\k\d\@<!\)\|\%(\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\)\)\)"
    \ end="\z1\zs"
    \ contained
    \ contains=TOP
    \ keepend

" <words>
" FIXME: not sure how to distinguish this from the "less than" operator
" in all cases. For now, it matches if any of the following is true:
"
" * There is whitespace missing on either side of the "<", since
"   people tend to put spaces around "less than"
" * It comes after "enum", "for", "any", "all", or "none"
" * It's followed by optional whitespace plus a newline
" * It's preceded by whitespace plus a "="
"
" It never matches when:
"
" * Preceded by [<=] (e.g. <<foo>>, =<$foo>)
" * Followed by [-=] (e.g. <--, <=, <==)
syn region p6StringAngle
    \ matchgroup=p6Quote
    \ start="\%(<\|\%(\%(\<enum\|for\|any\|all\|none\>\)\s*(\?\s*\)\@<!\s\)\@<!<\%(<\|=>\|[-=]\{1,2}\_s\)\@!"
    \ start="\%(=\s\+\)\@<=<\%(<\|=>\|[-=]\{1,2}\_s\)\@!"
    \ start="<\@<!<\%(<\|\s\|=>\|[-=]\)\@!"
    \ start="<\@<!<\%(\s*$\)\@="
    \ skip="\%(\\\@<!\\>\|<[^>]*>\)"
    \ end=">"
    \ contains=p6EscBackSlash,p6EscCloseAngle

" <<words>>
syn region p6StringAngles
    \ matchgroup=p6Quote
    \ start="<<=\@!"
    \ skip="\\\@<!\\>"
    \ end=">>"
    \ contains=@p6Interp_qq,p6Comment,p6EscHash,p6EscCloseAngle,p6Adverb,p6StringSQ,p6StringDQ

" «words»
syn region p6StringFrench
    \ matchgroup=p6Quote
    \ start="«"
    \ skip="\\\@<!\\»"
    \ end="»"
    \ contains=@p6Interp_qq,p6Comment,p6EscHash,p6EscCloseFrench,p6Adverb,p6StringSQ,p6StringDQ

" 'string'
syn region p6StringSQ
    \ matchgroup=p6Quote
    \ start="'"
    \ skip="\\\@<!\\'"
    \ end="'"
    \ contains=@p6Interp_q,p6EscQuote

" "string"
syn region p6StringDQ
    \ matchgroup=p6Quote
    \ start=+"+
    \ skip=+\\\@<!\\"+
    \ end=+"+
    \ contains=@p6Interp_qq,p6EscDoubleQuote

" Now for Q// and friends

let s:delims = [
 \ ["\\\"",         "\\\"", "p6EscDoubleQuote",  "\\\\\\@<!\\\\\\\""],
 \ ["'",            "'",    "p6EscQuote",        "\\\\\\@<!\\\\'"],
 \ ["/",            "/",    "p6EscForwardSlash", "\\\\\\@<!\\\\/"],
 \ ["`",            "`",    "p6EscBackTick",     "\\\\\\@<!\\\\`"],
 \ ["|",            "|",    "p6EscPipe",         "\\\\\\@<!\\\\|"],
 \ ["!",            "!",    "p6EscExclamation",  "\\\\\\@<!\\\\!"],
 \ ["\\$",          "\\$",  "p6EscDollar",       "\\\\\\@<!\\\\\\$"],
 \ ["{",            "}",    "p6EscCloseCurly",   "\\%(\\\\\\@<!\\\\}\\|{[^}]*}\\)"],
 \ ["{{",           "}}",   "p6EscCloseCurly",   "\\%(\\\\\\@<!\\\\}}\\|{{\\%([^}]\\|}}\\@!\\)*}}\\)"],
 \ ["{{{",          "}}}",  "p6EscCloseCurly",   "\\%(\\\\\\@<!\\\\}}}\\|{{{\\%([^}]\\|}\\%(}}\\)\\@!\\)*}}}\\)"],
 \ ["«",            "»",    "p6EscCloseFrench",  "\\%(\\\\\\@<!\\\\»\\|«[^»]*»\\)"],
 \ ["««",           "»»",   "p6EscCloseFrench",  "\\%(\\\\\\@<!\\\\»»\\|««\\%([^»]\\|»»\\@!\\)*»»\\)"],
 \ ["«««",          "»»»",  "p6EscCloseFrench",  "\\%(\\\\\\@<!\\\\»»»\\|«««\\%([^»]\\|»\\%(»»\\)\\@!\\)*»»»\\)"],
 \ ["\\\[",         "]",    "p6EscCloseBracket", "\\%(\\\\\\@<!\\\\]\\|\\[^\\]]*]\\)"],
 \ ["\\\[\\\[",     "]]",   "p6EscCloseBracket", "\\%(\\\\\\@<!\\\\]]\\|\\[\\[\\%([^\\]]\\|]]\\@!\\)*]]\\)"],
 \ ["\\\[\\\[\\\[", "]]]",  "p6EscCloseBracket", "\\%(\\\\\\@<!\\\\]]]\\|\\[\\[\\[\\%([^\\]]\\|]\\%(]]\\)\\@!\\)*]]]\\)"],
 \ ["\\s\\@<=(",    ")",    "p6EscCloseParen",   "\\%(\\\\\\@<!\\\\)\\|([^)]*)\\)"],
 \ ["\\s\\@<=((",   "))",   "p6EscCloseParen",   "\\%(\\\\\\@<!\\\\))\\|((\\%([^)]\\|))\\@!\\)*))\\)"],
 \ ["\\s\\@<=(((",  ")))",  "p6EscCloseParen",   "\\%(\\\\\\@<!\\\\)))\\|(((\\%([^)]\\|)\\%())\\)\\@!\\)*)))\\)"],
 \ ["\\s\\@<=<",    ">",    "p6EscCloseAngle",   "\\%(\\\\\\@<!\\\\>\\|<[^>]*>\\)"],
 \ ["\\s\\@<=<<",   ">>",   "p6EscCloseAngle",   "\\%(\\\\\\@<!\\\\>>\\|<<\\%([^>]\\|>>\\@!\\)*>>\\)"],
 \ ["\\s\\@<=<<<",  ">>>",  "p6EscCloseAngle",   "\\%(\\\\\\@<!\\\\>>>\\|<<<\\%([^>]\\|>\\%(>>\\)\\@!\\)*>>>\\)"],
\ ]

let s:adverbs = [
    \ ["s", "scalar"],
    \ ["a", "array"],
    \ ["h", "hash"],
    \ ["f", "function"],
    \ ["c", "closure"],
    \ ["b", "backslash"],
    \ ["w", "words"],
    \ ["ww", "quotewords"],
    \ ["x", "exec"],
\ ]

" these can't be conjoined with q and qq (e.g. as qqq and qqqq)
let s:q_adverbs = [
    \ ["q", "single"],
    \ ["qq", "double"],
\ ]

let s:before = "syn region p6String matchgroup=p6Quote start=\"\\%("
let s:after  = "\\%(\\s*:!\\?\\k\\d\\@<!\\%(\\k\\|[-']\\%(\\k\\d\\@<!\\)\\@=\\)*\\%(([^)]*)\\|\\[[^\\]]*]\\|<[^>]*>\\|«[^»]*»\\|{[^}]*}\\)\\?\\)*\\s*\\)\\@<="

for [start_delim, end_delim, end_group, skip] in s:delims
    " Q, q, and qq with any number of (ignored) adverbs
    exec s:before ."Q". s:after .start_delim."\" end=\"". end_delim ."\""
    exec s:before ."q". s:after .start_delim ."\" skip=\"". skip ."\" end=\"". end_delim ."\" contains=". end_group .",@p6Interp_q"
    exec s:before ."qq". s:after .start_delim ."\" skip=\"". skip ."\" end=\"". end_delim ."\" contains=". end_group .",@p6Interp_qq"
    
    for [short, long] in s:adverbs
        " Qs, qs, qqs, Qa, qa, qqa, etc, with ignored adverbs
        exec s:before ."Q".short. s:after .start_delim ."\" end=\"". end_delim ."\" contains=@p6Interp_".long
        exec s:before ."q".short. s:after .start_delim ."\" skip=\"". skip ."\" end=\"". end_delim ."\" contains=". end_group .",@p6Interp_q,@p6Interp_".long
        exec s:before ."qq".short. s:after .start_delim ."\" skip=\"". skip ."\" end=\"". end_delim ."\" contains=". end_group .",@p6Interp_qq,@p6Interp_".long

        " Q, q, and qq, with one significant adverb
        exec s:before ."Q\\s*:\\%(".short."\\|".long."\\)". s:after .start_delim ."\" end=\"". end_delim ."\" contains=@p6Interp_".long
        for [q_short, q_long] in s:q_adverbs
            exec s:before ."Q\\s*:\\%(".q_short."\\|".q_long."\\)". s:after .start_delim ."\" end=\"". end_delim ."\" contains=@p6Interp_".q_long
        endfor
        exec s:before ."q\\s*:\\%(".short."\\|".long."\\)". s:after .start_delim ."\" skip=\"". skip ."\" end=\"". end_delim ."\" contains=". end_group .",@p6Interp_q,@p6Interp_".long
        exec s:before ."qq\\s*:\\%(".short."\\|".long."\\)". s:after .start_delim ."\" skip=\"". skip ."\" end=\"". end_delim ."\" contains=". end_group .",@p6Interp_qq,@p6Interp_".long
    
        for [short2, long2] in s:adverbs
            " Qs, qs, qqs, Qa, qa, qqa, etc, with one significant adverb
            exec s:before ."Q".short."\\s*:\\%(".short2."\\|".long2."\\)". s:after .start_delim ."\" end=\"". end_delim ."\" contains=@p6Interp_".long.",@p6Interp_".long2
            for [q_short2, q_long2] in s:q_adverbs
                exec s:before ."Q".short."\\s*:\\%(".q_short2."\\|".q_long2."\\)". s:after .start_delim ."\" end=\"". end_delim ."\" contains=@p6Interp_".long.",@p6Interp_".q_long2
            endfor
            exec s:before ."q".short."\\s*:\\%(".short2."\\|".long2."\\)". s:after .start_delim ."\" skip=\"". skip ."\" end=\"". end_delim ."\" contains=". end_group .",@p6Interp_q,@p6Interp_".long.",@p6Interp_".long2
            exec s:before ."qq".short."\\s*:\\%(".short2."\\|".long2."\\)". s:after .start_delim ."\" skip=\"". skip ."\" end=\"". end_delim ."\" contains=". end_group .",@p6Interp_qq,@p6Interp_".long.",@p6Interp_".long2
        endfor
    endfor
endfor

" :key
syn match p6String display "\%(:\@<!:!\?\)\@<=\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*"

" => and p5=> autoquoting
syn match p6String display "\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\ze\s\+p5=>"
syn match p6String display "\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\ze\%(p5\)\@<!=>"
syn match p6String display "\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*\ze\s\+=>"
syn match p6String display "\k\d\@<!\%(\k\|[-']\%(\k\d\@<!\)\@=\)*p5\ze=>"

" cross operators
syn match p6Operator display "X[^X\[\]()[:space:]]\+X"

" Hyperoperators, may need to add some more operators here
let s:hyperops = "\\%(\\%(\\.\\|+\\|-\\|\\*\\|\\*\\*\\|\\~\\|/\\|x\\|xx\\|&\\||\\)=\\?\\|++\\|--\\)"
exec "syn match p6Operator display \"»"   .s:hyperops."»\\?\""
exec "syn match p6Operator display \"«\\?".s:hyperops."«\""
exec "syn match p6Operator display \"»"   .s:hyperops."«\""
exec "syn match p6Operator display \"«"   .s:hyperops. "»\""

exec "syn match p6Operator display \">>"          .s:hyperops."\\%(>>\\)\\?\""
exec "syn match p6Operator display \"\\%(<<\\)\\?".s:hyperops."<<\""
exec "syn match p6Operator display \">>"          .s:hyperops."<<\""
exec "syn match p6Operator display \"<<"          .s:hyperops.">>\""

" Regexes

syn cluster p6Regexen
    \ add=@p6Interp_qq
    \ add=p6Comment
    \ add=p6CharClass
    \ add=p6RuleCall
    \ add=p6TestExpr
    \ add=p6RegexSpecial

" Standard /.../
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\(\w\_s*\)\@<!/"
    \ start="\(\(\<split\|\<grep\)\s*\)\@<=/"
    \ skip="\\/"
    \ end="/"
    \ contains=@p6Regexen

" m:/.../
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<\(m\|mm\|rx\)\_s*\(\_s*:\_s*[[:alnum:]_()]\+\)*\_s*\z([^[:alnum:]_:(]\)"
    \ skip="\\\z1"
    \ end="\z1"
    \ contains=@p6Regexen

" m:[] m:{} and m:<>
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<\(m\|mm\|rx\)\_s*\(\_s*:\_s*[[:alnum:]_()]\+\)*\_s*\["
    \ skip="\\]"
    \ end="]"
    \ contains=@p6Regexen
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<\(m\|mm\|rx\)\_s*\(\_s*:\_s*[[:alnum:]_()]\+\)*\_s*{"
    \ skip="\\}"
    \ end="}"
    \ contains=@p6Regexen
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<\(m\|mm\|rx\)\_s*\(\_s*:\_s*[[:alnum:]_()]\+\)*\_s*<"hs=e
    \ skip="\\>"
    \ end=">"
    \ contains=@p6Regexen

" rule { }
syn region p6Regex
    \ start="rule\(\_s\+\w\+\)\{0,1}\_s*{"hs=e
    \ end="}"
    \ contains=@p6Regexen
"syn region p6Regex
"    \ start="\(rule\|token\|regex\)\(\_s\+\w\+\)\{0,1}\_s*{"hs=e
"    \ end="}"
"    \ contains=@p6Regexen

" Closure (FIXME: Really icky hack, also doesn't support :blah modifiers)
" However, don't do what you might _expect_ would work, because it won't.
" And no variant of it will, either.  I found this out through 4 hours from
" miniscule tweaking to complete redesign. This is the only way I've found!
"syn region p6Closure
"    \ start="\(\(rule\(\_s\+\w\+\)\{0,1}\|s\|rx\)\_s*\)\@<!{"
"    \ end="}"
"    \ matchgroup=p6Error
"    \ end="[\])]"
"    \ contains=TOP
"    \ fold
"syn region p6Closure
"    \ start="\(\(\(rule\|token\|regex\)\(\_s\+\w\+\)\{0,1}\|s\|rx\)\_s*\)\@<!{"
"    \ end="}"
"    \ matchgroup=p6Error
"    \ end="[\])]"
"    \ contains=TOP
"    \ fold

" s:///, tr:///,  and all variants
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<s\_s*\(\_s*:\_s*[[:alnum:]_()]\+\)*\_s*\z([^[:alnum:]_:(]\)"
    \ skip="\\\z1"
    \ end="\z1"me=e-1
    \ contains=@p6Regexen
    \ nextgroup=p6SubNonBracket
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<s\_s*\(\_s*:\_s*[[:alnum:]_()]\+\)*\_s*\["
    \ skip="\\]"
    \ end="]\_s*"
    \ contains=@p6Regexen
    \ nextgroup=p6SubBracket
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<s\_s*\(\_s*:\_s*[[:alnum:]_()]\+\)*\_s*{"
    \ skip="\\}"
    \ end="}\_s*"
    \ contains=@p6Regexen
    \ nextgroup=p6SubBracket
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<s\_s*\(\_s*:\_s*[[:alnum:]_()]\+\)*\_s*<"
    \ skip="\\>"
    \ end=">\_s*"
    \ contains=@p6Regexen
    \ nextgroup=p6SubBracket
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<tr\_s*\(\_s*:\_s*[[:alnum:]_()]\+\)*\_s*\z([^[:alnum:]_:(]\)"
    \ skip="\\\z1"
    \ end="\z1"me=e-1
    \ nextgroup=p6TransNonBracket

" This is kinda tricky. Since these are contained, they're "called" by the
" previous four groups. They just pick up the delimiter at the current location
" and behave like a string.
syn region p6SubNonBracket
    \ matchgroup=p6Keyword
    \ start="\z(\W\)"
    \ skip="\\\z1"
    \ end="\z1"
    \ contained
    \ contains=@p6Interp_qq
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="\z(\W\)"
    \ skip="\\\z1"
    \ end="\z1"
    \ contained
    \ contains=@p6Interp_qq
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="\["
    \ skip="\\]"
    \ end="]"
    \ contained
    \ contains=@p6Interp_qq
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="{"
    \ skip="\\}"
    \ end="}"
    \ contained
    \ contains=@p6Interp_qq
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="<"
    \ skip="\\>"
    \ end=">"
    \ contained
    \ contains=@p6Interp_qq
syn region p6TransNonBracket
    \ matchgroup=p6Keyword
    \ start="\z(\W\)"
    \ skip="\\\z1"
    \ end="\z1"
    \ contained

syn match  p6RuleCall     contained "<\s*!\{0,1}\s*\w\+"hs=s+1
syn match  p6CharClass    contained "<\s*!\{0,1}\s*\[\]\{0,1}[^]]*\]\s*>"
syn match  p6CharClass    contained "<\s*!\{0,1}\s*-\{0,1}\(alpha\|digit\|sp\|ws\|null\|xdigit\|alnum\|space\|ascii\|cntrl\|graph\|lower\|print\|punct\|title\|upper\|word\|vspace\|hspace\)\s*>"
syn match  p6CharClass    contained "\\[HhVvNnTtEeRrFfWwSs]"
syn match  p6CharClass    contained "\\[xX]\(\[[0-9a-f;]\+\]\|\x\+\)"
syn match  p6CharClass    contained "\\0\(\[[0-7;]\+\]\|\o\+\)"
syn region p6CharClass
    \ start="\\[QqCc]\["
    \ skip="\\]"
    \ end="]"
    \ contained
syn match  p6RegexSpecial contained "\\\@<!:\{1,3\}"
syn match  p6RegexSpecial contained "<\s*\(cut\|commit\)\s*>"
"syn match p6RegexSpecial contained "\\\@<![+*|]"
syn match  p6RegexSpecial contained ":="
syn region p6CharClass
    \ start=+<\s*!\{0,1}\s*\z(['"]\)+
    \ skip="\\\z1"
    \ end="\z1\s*>"
    \ contained
"syn region p6TestExpr
"    \ start="<\s*!\{0,1}\s*("
"    \ end=")\s*>"
"    \ contained
"    \ contains=TOP
syn region p6TestExpr
    \ start="<\(?\|!\)?{"
    \ end="}\s*>"
    \ contained
    \ contains=TOP

" This is an operator, not a regex
syn match p6Operator "//"

" Pod

" Abbreviated blocks
syn region p6PodAbbrRegion
    \ matchgroup=p6PodCommand
    \ start="^=\ze\k\d\@<!\k*"
    \ end="^\ze\%(\s*$\|=\k\d\@<!\)"
    \ contains=p6PodAbbrType
    \ keepend

syn region p6PodAbbrType
    \ matchgroup=p6PodType
    \ start="\k\d\@<!\k*"
    \ end="^\ze\%(\s*$\|=\k\d\@<!\)"
    \ contained
    \ contains=p6PodName,p6PodAbbr

syn match p6PodName ".\+" contained contains=p6PodFormat

syn region p6PodAbbr
    \ start="^"
    \ end="^\ze\%(\s*$\|=\k\d\@<!\)"
    \ contained
    \ contains=@p6PodAmbient

" Directives
syn region p6PodDirectRegion
    \ matchgroup=p6PodCommand
    \ start="^=\%(config\|use\)\>"
    \ end="^\ze\%([^=]\|=\k\d\@<!\|$\)"
    \ contains=p6PodDirectArgRegion
    \ keepend

syn region p6PodDirectArgRegion
    \ matchgroup=p6PodType
    \ start="\S\+"
    \ end="^\ze\%([^=]\|=\k\d\@<!\|$\)"
    \ contained
    \ contains=p6PodDirectConfigRegion

syn region p6PodDirectConfigRegion
    \ start=""
    \ end="^\ze\%([^=]\|=\k\d\@<!\|$\)"
    \ contained
    \ contains=@p6PodConfig

" =encoding is a special directive
syn region p6PodDirectRegion
    \ matchgroup=p6PodCommand
    \ start="^=encoding\>"
    \ end="^\ze\%([^=]\|=\k\d\@<!\|$\)"
    \ contains=p6PodEncodingArgRegion
    \ keepend

syn region p6PodEncodingArgRegion
    \ matchgroup=p6PodName
    \ start="\S\+"
    \ end="^\ze\%([^=]\|=\k\d\@<!\|$\)"
    \ contained

" Paragraph blocks
syn region p6PodParaRegion
    \ matchgroup=p6PodCommand
    \ start="^=for\>"
    \ end="^\ze\%(\s*\|=\k\d\@<!\)"
    \ contains=p6PodParaTypeRegion

syn region p6PodParaTypeRegion
    \ matchgroup=p6PodType
    \ start="\S\+"
    \ end="^\ze\%(\s*$\|=\k\d\@<!\)"
    \ contained
    \ keepend
    \ contains=p6PodPara,p6PodParaConfigRegion

syn region p6PodParaConfigRegion
    \ start=""
    \ end="^\ze\%([^=]\|=\k\@<!\)"
    \ contained
    \ contains=@p6PodConfig

syn region p6PodPara
    \ start="^[^=]"
    \ end="^\ze\%(\s*$\|=\k\d\@<!\)"
    \ contained
    \ extend
    \ contains=@p6PodAmbient

" Delimited blocks
syn region p6PodDelimRegion
    \ matchgroup=p6PodCommand
    \ start="^=begin\>"
    \ end="^=end\>"
    \ contains=p6PodDelimTypeRegion

syn region p6PodDelimTypeRegion
    \ matchgroup=p6PodType
    \ start="\k\d\@<!\k*"
    \ end="^\ze=end\>"
    \ contained
    \ contains=p6PodDelim,p6PodDelimConfigRegion

syn region p6PodDelimConfigRegion
    \ start=""
    \ end="^\ze\%([^=]\|=\k\d\@<!\|$\)"
    \ contained
    \ contains=@p6PodConfig

" Delimited code blocks
syn region p6PodDelimRegion
    \ matchgroup=p6PodCommand
    \ start="^=begin\>\%(\s*code\>\)\@="
    \ end="^=end\>"
    \ contains=p6PodDelimCodeTypeRegion

syn region p6PodDelimCodeTypeRegion
    \ matchgroup=p6PodType
    \ start="\k\d\@<!\k*"
    \ end="^\ze=end\>"
    \ contained
    \ contains=p6PodDelimCode,p6PodDelimConfigRegion

syn cluster p6PodConfig
    \ add=p6PodConfigOperator
    \ add=p6PodExtraConfig

syn region p6PodParens
    \ start="("
    \ end=")"
    \ contained
    \ contains=p6Number,p6StringSQ

syn match p6PodConfigOperator display contained ":" nextgroup=p6PodConfigOption
syn match p6PodConfigOption   display contained "[^[:space:](<]\+" nextgroup=p6PodParens,p6StringAngle
syn match p6PodExtraConfig    display contained "^="
syn match p6PodVerticalBar    display contained "|"
syn match p6PodColon          display contained ":"
syn match p6PodSemicolon      display contained ";"
syn match p6PodComma          display contained ","

syn region p6PodDelim
    \ start="^"
    \ end="^\ze=end\>"
    \ contained
    \ contains=@p6PodNested,@p6PodAmbient

syn region p6PodDelimCode
    \ start="^"
    \ end="^\ze=end\>"
    \ contained
    \ contains=@p6PodNested

syn region p6PodDelimEndRegion
    \ matchgroup=p6PodType
    \ start="\%(^=end\>\)\@<="
    \ end="\S\+"

" Special things one may find in Pod prose
syn cluster p6PodAmbient
    \ add=p6PodFormat
    \ add=p6PodImplicitCode

syn match p6PodImplicitCode contained "^\s.*"

" These may appear inside delimited blocks
syn cluster p6PodNested
    \ add=p6PodAbbrRegion
    \ add=p6PodDirectRegion
    \ add=p6PodParaRegion
    \ add=p6PodDelimRegion
    \ add=p6PodDelimEndRegion

" Pod formatting codes

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="\u<<\@!"
    \ skip="<[^>]*>"
    \ end=">"
    \ contained
    \ contains=p6PodFormat

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="\u<<"
    \ skip="<[^>]*>"
    \ end=">>"
    \ contained
    \ contains=p6PodFormat

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="\u««\@!"
    \ end="»"
    \ contained
    \ contains=p6PodFormat

" C<> and V<> don't allow nested formatting formatting codes

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="[CV]<<\@!"
    \ skip="<[^>]*>"
    \ end=">"
    \ contained

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="[CV]<<"
    \ skip="<<[^>]*>>"
    \ end=">>"
    \ contained

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="[CV]<<<"
    \ skip="<<<[^>]*>>>"
    \ end=">>>"
    \ contained

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="[CV]««\@!"
    \ skip="«[^»]*»"
    \ end="»"
    \ contained

" L<> can have a "|" separator

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="L<<\@!"
    \ skip="<[^>]*>"
    \ end=">"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="L<<"
    \ skip="<<[^>]*>>"
    \ end=">>"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="L<<<"
    \ skip="<<<[^>]*>>>"
    \ end=">>>"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="L««\@!"
    \ skip="«[^»]*»"
    \ end="»"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar

" M<> can have a ":" separator

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="M<<\@!"
    \ skip="<[^>]*>"
    \ end=">"
    \ contained
    \ contains=p6PodFormat,p6PodColon

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="M<<"
    \ skip="<<[^>]*>>"
    \ end=">>"
    \ contained
    \ contains=p6PodFormat,p6PodColon

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="M<<<"
    \ skip="<<<[^>]*>>>"
    \ end=">>>"
    \ contained
    \ contains=p6PodFormat,p6PodColon

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="M««\@!"
    \ skip="«[^»]*»"
    \ end="»"
    \ contained
    \ contains=p6PodFormat,p6PodColon

" D<> can have "|" and ";" separators

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="D<<\@!"
    \ skip="<[^>]*>"
    \ end=">"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar,p6PodSemicolon

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="D<<"
    \ skip="<<[^>]*>>"
    \ end=">>"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar,p6PodSemicolon

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="D<<<"
    \ skip="<<<[^>]*>>>"
    \ end=">>>"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar,p6PodSemicolon

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="D««\@!"
    \ skip="«[^»]*»"
    \ end="»"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar,p6PodSemicolon

" X<> can have "|", "," and ";" separators

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="X<<\@!"
    \ skip="<[^>]*>"
    \ end=">"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar,p6PodSemicolon,p6PodComma

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="X<<"
    \ skip="<<[^>]*>>"
    \ end=">>"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar,p6PodSemicolon,p6PodComma

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="X<<<"
    \ skip="<<<[^>]*>>>"
    \ end=">>>"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar,p6PodSemicolon,p6PodComma

syn region p6PodFormat
    \ matchgroup=p6PodFormatCode
    \ start="X««\@!"
    \ skip="«[^»]*»"
    \ end="»"
    \ contained
    \ contains=p6PodFormat,p6PodVerticalBar,p6PodSemicolon,p6PodComma

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_perl6_syntax_inits")
    if version < 508
        let did_perl6_syntax_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif

    HiLink p6StringAngle     p6String
    HiLink p6StringFrench    p6String
    HiLink p6StringAngles    p6String
    HiLink p6StringSQ        p6String
    HiLink p6StringDQ        p6String
    HiLink p6SubNonBracket   p6String
    HiLink p6SubBracket      p6String
    HiLink p6TransNonBracket p6String
    HiLink p6Escape          p6StringSpecial
    HiLink p6EscHash         p6StringSpecial
    HiLink p6EscQQ           p6StringSpecial
    HiLink p6EscQuote        p6StringSpecial
    HiLink p6EscDoubleQuote  p6StringSpecial
    HiLink p6EscBackTick     p6StringSpecial
    HiLink p6EscForwardSlash p6StringSpecial
    HiLink p6EscPipe         p6StringSpecial
    HiLink p6EscEsclamation  p6StringSpecial
    HiLink p6EscDollar       p6StringSpecial
    HiLink p6EscOpenCurly    p6StringSpecial
    HiLink p6EscCloseCurly   p6StringSpecial
    HiLink p6EscCloseBracket p6StringSpecial
    HiLink p6EscCloseAngle   p6StringSpecial
    HiLink p6EscCloseFrench  p6StringSpecial
    HiLink p6EscBackSlash    p6StringSpecial
    HiLink p6CharClass       p6StringSpecial
    HiLink p6RegexSpecial    p6StringSpecial

    HiLink p6Property       Tag
    HiLink p6Attention      Todo
    HiLink p6Type           Type
    HiLink p6Error          Error
    HiLink p6BlockLabel     Label
    HiLink p6Float          Float
    HiLink p6Normal         Normal
    HiLink p6Package        Normal
    HiLink p6PackageScope   Normal
    HiLink p6Number         Number
    HiLink p6String         String
    HiLink p6Regex          String
    HiLink p6Repeat         Repeat
    HiLink p6Keyword        Keyword
    HiLink p6Pragma         Keyword
    HiLink p6Routine        Keyword
    HiLink p6Module         Keyword
    HiLink p6DeclareRoutine Keyword
    HiLink p6VarStorage     Keyword
    HiLink p6FlowControl    Special
    HiLink p6NumberBase     Special
    HiLink p6Twigil         Special
    HiLink p6PackageTwigil  Special
    HiLink p6CodePoint      Special
    HiLink p6HexSequence    Special
    HiLink p6OctSequence    Special
    HiLink p6Comment        Comment
    HiLink p6Shebang        PreProc
    HiLink p6ClosureTrait   PreProc
    HiLink p6CustomRoutine  Function
    HiLink p6Operator       Operator
    HiLink p6Context        Operator
    HiLink p6Placeholder    Operator
    HiLink p6Quote          Delimiter
    HiLink p6TypeConstraint PreCondit
    HiLink p6Exception      Exception
    HiLink p6Sigil          Identifier
    HiLink p6PunctVar       Identifier
    HiLink p6Variable       Identifier
    HiLink p6Match          Identifier
    HiLink p6RuleCall       Identifier
    HiLink p6Conditional    Conditional
    HiLink p6StringSpecial  SpecialChar

    HiLink p6PodAbbr         p6Pod
    HiLink p6PodPara         p6Pod
    HiLink p6PodDelim        p6Pod
    HiLink p6PodDelimCode    p6PodCode
    HiLink p6PodImplicitCode p6PodCode
    HiLink p6PodExtraConfig  p6PodCommand
    HiLink p6PodVerticalBar  p6PodFormatCode
    HiLink p6PodColon        p6PodFormatCode
    HiLink p6PodSemicolon    p6PodFormatCode
    HiLink p6PodComma        p6PodFormatCode

    HiLink p6PodType           Type
    HiLink p6PodConfigOption   String
    HiLink p6PodCode           PreProc
    HiLink p6Pod               Comment
    HiLink p6PodConfigOperator Operator
    HiLink p6PodCommand        Statement
    HiLink p6PodName           Identifier
    HiLink p6PodFormatCode     SpecialChar
    HiLink p6PodFormat         SpecialComment

    delcommand HiLink
endif

" Syncing to speed up processing
syn sync fromstart
syn sync maxlines=100
syn sync match p6SyncPod groupthere p6PodAbbrRegion     "^=\k\d\@<!\k*\>"
syn sync match p6SyncPod groupthere p6PodDirectRegion   "^=\%(config\|use\|encoding\)\>"
syn sync match p6SyncPod groupthere p6PodParaRegion     "^=for\>"
syn sync match p6SyncPod groupthere p6PodDelimRegion    "^=begin\>"
syn sync match p6SyncPod groupthere p6PodDelimEndRegion "^=end\>"

setlocal foldmethod=syntax

let b:current_syntax = "perl6"
