" Vim syntax file
" Language:     Perl 6
" Last Change:  Nov 27th 2008
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
"   * Handle <? ... > and similar constructs in regexes
"   * Improve POD formatting codes support (S<>, etc) 
"   * Add more support for folding
"   * Add more syntax syncing hooks
"   * Overhaul Q// and its derivatives
"   * Overhaul regexes
"   * Enum declarations
"   * :key should always be highlighted as a string,
"     even if it's a known keyword
"   * Special numbers (1_000, 0b0101, etc)
"   * Multiline #[] comments

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Recommended formatting options (see pugs::hack)
" These should eventually be put in a $VIMRUNTIME/indent/perl6.vim file
setlocal autoindent expandtab smarttab shiftround shiftwidth=4 softtabstop=4

" Billions of keywords
syn keyword p6Attention       ACHTUNG ATTN ATTENTION FIXME NB contained 
syn keyword p6Attention       TODO TBD WTF XXX NOTE contained
" XXX subset declares a type, but not a module, how should it be classified?
syn keyword p6Module          module class role use require package enum
syn keyword p6Module          grammar subset self
syn keyword p6DeclareRoutine  macro sub submethod method multi only rule
syn keyword p6DeclareRoutine  token regex category
syn keyword p6VarStorage      let my our state temp has proto constant
syn keyword p6Repeat          for loop repeat while until gather
syn keyword p6FlowControl     take do when next last redo given return
syn keyword p6FlowControl     default exit make
syn keyword p6TypeConstraint  is as but does trusts of returns also
syn keyword p6ClosureTrait    BEGIN CHECK INIT START FIRST ENTER LEAVE KEEP
syn keyword p6ClosureTrait    UNDO NEXT LAST PRE POST END
syn keyword p6Exception       die fail try CATCH CONTROL warn
syn keyword p6Property        prec irs ofs ors pos export deep
syn keyword p6Property        rw parsed cached readonly
syn keyword p6Property        ref copy inline tighter looser equiv assoc
syn keyword p6Type            Object Any Junction Whatever Capture Match
syn keyword p6Type            Signature Proxy Matcher Package Module Class
syn keyword p6Type            Grammar Scalar Array Hash KeyHash KeySet KeyBag
syn keyword p6Type            Pair List Seq Range Set Bag Mapping Void Undef
syn keyword p6Type            Failure Exception Code Block Routine Sub Macro
syn keyword p6Type            Method Submethod Regex Str Blob Char Byte
syn keyword p6Type            Codepoint Grapheme StrPos StrLen Version Num
syn keyword p6Type            Complex num complex Bit Bool bit bool Order
syn keyword p6Type            Increasing Decreasing Ordered Callable AnyChar
syn keyword p6Type            Positional Associative Ordering KeyExtractor
syn keyword p6Type            Comparator OrderingPair IO KitchenSink
syn keyword p6Type            Int int int1 int2 int4 int8 int16 int32 int64
syn keyword p6Type            Rat rat rat1 rat2 rat4 rat8 rat16 rat32 rat64
syn keyword p6Type            UInt uint uint1 uint2 uint4 uint8 uint16
syn keyword p6Type            uint32 uint64 Buf buf buf1 buf2 buf4 buf8
syn keyword p6Type            buf16 buf32 buf64 True False
syn keyword p6Number          NaN Inf
syn keyword p6Routine         WHAT WHICH VAR eager hyper substr index rindex
syn keyword p6Routine         grep map sort join split reduce min max reverse
syn keyword p6Routine         truncate zip cat roundrobin classify first sum
syn keyword p6Routine         keys values pairs defined delete exists elems
syn keyword p6Routine         end kv arity assuming pick slice clone key new
syn keyword p6Routine         any all none one wrap shape classify value
syn keyword p6Routine         callsame callwith nextsame nextwith ACCEPTS
syn keyword p6Routine         pop push shift splice unshift floor ceiling
syn keyword p6Routine         abs exp log log10 rand sign sqrt sin cos tan
syn keyword p6Routine         round srand roots cis unpolar polar atan2
syn keyword p6Routine         p5chop chop p5chomp chomp lc lcfirst uc ucfirst
syn keyword p6Routine         capitalize normalize pack unpack quotemeta comb
syn keyword p6Routine         samecase sameaccent chars nfd nfc nfkd nfkc
syn keyword p6Routine         printf sprintf caller evalfile run runinstead 
syn keyword p6Routine         nothing want bless chr ord list item gmtime 
syn keyword p6Routine         localtime time gethost getpw chroot getlogin
syn keyword p6Routine         kill fork wait perl context graphs codes bytes
syn keyword p6Routine         print open read write readline say seek close
syn keyword p6Routine         opendir readdir slurp pos fmt vec link unlink
syn keyword p6Routine         symlink uniq pair asin acos atan sec cosec
syn keyword p6Routine         cotan asec acosec acotan sinh cosh tanh asinh
syn keyword p6Routine         acosh atanh sech cosech cotanh sech acosech
syn keyword p6Routine         acotanh plan ok dies_ok lives_ok skip todo
syn keyword p6Routine         pass flunk force_todo use_ok isa_ok cmp_ok
syn keyword p6Routine         diag is_deeply isnt like skip_rest unlike
syn keyword p6Routine         nonce skip_rest eval_dies_ok eval_lives_ok
syn keyword p6Routine         approx is_approx throws_ok version_lt signature
syn keyword p6Routine         eval operator undef undefine sleep from to
syn keyword p6Routine         infix postfix prefix circumfix postcircumfix
syn keyword p6Routine         minmax
syn keyword p6Operator        x xx div mod also leg cmp
syn keyword p6Operator        eq ne lt le gt ge eqv ff fff true not Z
syn keyword p6Operator        X XeqvX and andthen or xor orelse extra

" more operators (not very smart, allows any combination)
syn match p6Operator display "\%(+\|-\|/\|\*\|\~\|?\||\|\\\|=\|\^\|!\|%\)"
syn match p6Operator display "\%(&\|,\|<\|>\|\.\|;\)"
" these require whitespace on the left side
syn match p6Operator display "\%(^\|\s\)\@<=\%(xx=\|p5=>\)"
" these require whitespace on both sides
syn match p6Operator display "\%(^\|\s\)\@=\%(!eqv\|X\~X\|X\*X\)\@=\%(\s\|$\)"
" only a single colon is an operator
syn match p6Operator display ":\@<!::\@!"
" reduce
syn match p6Operator display "\[[^[:digit:];]]"

" conditionals need whitespace to the right
syn match p6Conditional       "\%(if\|else\|elsif\|unless\)\s\@="

" misc
syn match p6Normal     display "\w*::\w\+"
syn match p6Comment    display "#.*" contains=p6Attention
syn match p6Shebang    display "\%^#!.*"
syn match p6BlockLabel display "\%(^\s*\)\@<=\h\w*\s*:\s\@="
syn match p6Variable   display "[$@%&][!.*^?]\?\%([[:alnum:]_¢]\|::\)*"

" FIXME: This ugly hack will show up later on. Once again, don't try to fix it.
" E.g. this makes "@()" highlight properly in "@( bla() )"
syn region p6ParenExpression
    \ start="\(<\s*\)\@<!("
    \ end=")"
    \ transparent

syn region p6BracketExpression
    \ start="\["
    \ end="]"
    \ transparent

" contextualizers
syn match p6Contextualizer display "hash"
syn region p6ExplicitContext
    \ matchgroup=p6Contextualizer
    \ start="\$("
    \ start="@("
    \ start="%("
    \ start="&("
    \ start="@@("
    \ end=")"
    \ contains=TOP

" { ... } closure in interpolated strings
syn region p6InterpClosure
    \ matchgroup=p6StringSpecial
    \ start="{"
    \ end="}"
    \ contained
    \ contains=TOP

" Interpolated strings

syn cluster p6Interp
    \ add=p6Variable
    \ add=p6InterpClosure
    \ add=p6ExplicitContext

" "string"
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start=+"+
    \ skip=+\\\@<!\\"+
    \ end=+"+
    \ contains=@p6Interp
" «string»
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start="«"
    \ end="»"
    \ contains=@p6Interp
" <<string>>
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start="<<"
    \ end=">>"
    \ contains=@p6Interp

" Punctuation-delimited interpolated strings
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start="\<q[qwx]\(:\(!\?[[:alnum:]]\((\w\+)\)\?\)\+\)\?\s*\z([^[:alnum:]:#_ ]\)"
    \ skip="\\\z1"
    \ end="\z1"
    \ contains=@p6Interp
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start="\<q[qwx]\(:\(!\?[[:alnum:]]\((\w\+)\)\?\)\+\)\?\s*{"
    \ skip="\\}"
    \ end="}"
    \ contains=@p6Interp
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start="\<q[qwx]\(:\(!\?[[:alnum:]]\((\w\+)\)\?\)\+\)\?\s*("
    \ skip="\\)"
    \ end=")"
    \ contains=@p6Interp
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start="\<q[qwx]\(:\(!\?[[:alnum:]]\((\w\+)\)\?\)\+\)\?\s*\["
    \ skip="\\]"
    \ end="]"
    \ contains=@p6Interp
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start="\<q[qwx]\(:\(!\?[[:alnum:]]\((\w\+)\)\?\)\+\)\?\s*<"
    \ skip="\\>"
    \ end=">"
    \ contains=@p6Interp

" Literal strings

syn match p6EscapedQuote display "\\\@<!\\'" contained
syn match p6EscapedArrow display "\\\@<!\\>" contained

" 'string'
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="'"
    \ skip="\\\@<!\\'"
    \ end="'"
    \ contains=p6EscapedQuote
" <string>,  not sure how to distinguish this from "less than" in all
" cases. The following only matches if whitespace is missing on
" either side, since people tend to put spaces around "less than".
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="\s\@<!<"
    \ start="<\s\@!"
    \ skip="\\>"
    \ end=">\@<!>"
    \ contains=p6EscapedRightArrow
" $<rule>
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="\$<\(.*>\)\@="
    \ end=">\@<!>"

" Punctuation-delimited literal strings
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="\<q\(:\(!\?[[:alnum:]]\((\w\+)\)\?\)\+\)\?\s*\z([^[:alnum:]:#_ ]\)"
    \ skip="\\\z1"
    \ end="\z1"
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="\<q\(:\(!\?[[:alnum:]]\((\w\+)\)\?\)\+\)\?\s*{"
    \ skip="\\}"
    \ end="}"
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="\<q\(:\(!\?[[:alnum:]]\((\w\+)\)\?\)\+\)\?\s*("
    \ skip="\\)"
    \ end=")"
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="\<q\(:\(!\?[[:alnum:]]\((\w\+)\)\?\)\+\)\?\s*\["
    \ skip="\\]"
    \ end="]"
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="\<q\(:\(!\?[[:alnum:]]\((\w\+)\)\?\)\+\)\?\s*<"
    \ skip="\\>"
    \ end=">"

" Numbers
syn match p6Number display "\<\(\d*\.\d\+\|\d\+\)\(e\d\+\)\{0,1}"
syn match p6Number display "\<0o[0-7]\+"
syn match p6Number display "\<0x[0-9a-fA-F]\+"

" :string
syn match p6LiteralString display ":\@<=\w\+"

" => and p5=> autoquoting. Edit with care.
syn match p6LiteralString display "\w\+\ze\s\+p5=>"
syn match p6LiteralString display "\w\+\ze\(p5\)\@<!=>"
syn match p6LiteralString display "\w\+\ze\s\+=>"
syn match p6LiteralString display "\w\+p5\ze=>"

" these are operators, quotes
syn match p6Operator display "<=>"
" hyperoperators
syn match p6Operator display "\%(>>\|»\)[^[:alnum:][:blank:]]"
syn match p6Operator display "[^[:alnum:][:blank:]]\%(«\|<<\)"

" =<> is an operator, not a quote
syn region p6Iterate
    \ matchgroup=p6Operator
    \ start="=<<\@!"
    \ end=">"
    \ oneline
    \ display
    \ contains=p6Variable

" Regexes

syn cluster p6Regexen
    \ add=@p6Interp
    \ add=p6Closure
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
syn region p6Closure
    \ start="\(\(rule\(\_s\+\w\+\)\{0,1}\|s\|rx\)\_s*\)\@<!{"
    \ end="}"
    \ matchgroup=p6Error
    \ end="[\])]"
    \ contains=TOP
    \ fold
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
    \ contains=@p6Interp
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="\z(\W\)"
    \ skip="\\\z1"
    \ end="\z1"
    \ contained
    \ contains=@p6Interp
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="\["
    \ skip="\\]"
    \ end="]"
    \ contained
    \ contains=@p6Interp
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="{"
    \ skip="\\}"
    \ end="}"
    \ contained
    \ contains=@p6Interp
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="<"
    \ skip="\\>"
    \ end=">"
    \ contained
    \ contains=@p6Interp
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
    \ start="^=\ze\S\+\>"
    \ end="^\ze\(\s*$\|=\k\)"
    \ contains=p6PodAbbrType
    \ keepend

syn region p6PodAbbrType
    \ matchgroup=p6PodType
    \ start="\S\+\>"
    \ end="^\ze\(\s*$\|=\k\)"
    \ contained
    \ contains=p6PodAbbr

syn region p6PodAbbr
    \ start=""
    \ end="^\ze\(\s*$\|=\k\)"
    \ contained
    \ contains=@p6PodAmbient

" Directives
syn region p6PodDirectRegion
    \ matchgroup=p6PodCommand
    \ start="^=\(config\|use\|encoding\)\>"
    \ end="^\ze\([^=]\|=\S\|$\)"
    \ contains=p6PodDirectTypeRegion
    \ keepend

syn region p6PodDirectTypeRegion
    \ matchgroup=p6PodType
    \ start="\S\+"
    \ end="^\ze\([^=]\|=\S\|$\)"
    \ contained
    \ contains=p6PodDirectConfigRegion

syn region p6PodDirectConfigRegion
    \ matchgroup=p6PodConfig
    \ start=""
    \ end="^\ze\([^=]\|=\S\|$\)"
    \ contained
    \ contains=p6PodConfig,p6PodExtraConfig

" Paragraph blocks
syn region p6PodParaRegion
    \ matchgroup=p6PodCommand
    \ start="^=for\>"
    \ end="^\ze\(\s*\|=\S\)"
    \ contains=p6PodParaTypeRegion

syn region p6PodParaTypeRegion
    \ matchgroup=p6PodType
    \ start="\S\+"
    \ end="^\ze\(\s*$\|=\S\)"
    \ contained
    \ keepend
    \ contains=p6PodPara,p6PodParaConfigRegion

syn region p6PodParaConfigRegion
    \ matchgroup=p6PodConfig
    \ start=""
    \ end="^\ze\([^=]\|=\S\)"
    \ contained
    \ contains=p6PodConfig,p6PodExtraConfig

syn region p6PodPara
    \ start="^[^=]"
    \ end="^\ze\(\s*$\|=\S\)"
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
    \ start="\S\+"
    \ end="^\ze=end\>"
    \ contained
    \ contains=p6PodDelim,p6PodDelimConfigRegion

syn region p6PodDelimConfigRegion
    \ matchgroup=p6PodConfig
    \ start=""
    \ end="^\ze\([^=]\|=\S\|$\)"
    \ contained
    \ contains=p6PodConfig,p6PodExtraConfig

syn region p6PodDelim
    \ start="^"
    \ end="^\ze=end\>"
    \ contained
    \ contains=@p6PodNested,@p6PodAmbient

syn region p6PodDelimEndRegion
    \ matchgroup=p6PodType
    \ start="\(^=end\>\)\@<="
    \ end="\S\+"

" Special things one may find in Pod prose
syn cluster p6PodAmbient
    \ add=p6PodFormat
    \ add=p6PodVerbatim

" These may appear inside delimited blocks
syn cluster p6PodNested
    \ add=p6PodAbbrRegion
    \ add=p6PodDirectRegion
    \ add=p6PodParaRegion
    \ add=p6PodDelimRegion
    \ add=p6PodDelimEndRegion

" Pod formatting codes
syn region p6PodFormat
    \ start="\u<[^<]"me=e-1
    \ end=">"
    \ contained
    \ oneline
    \ contains=p6PodFormat

syn region p6PodFormat
    \ start="\u«[^«]"me=e-1
    \ end="»"
    \ contained
    \ oneline
    \ contains=p6PodFormat

syn region p6PodFormat
    \ start="\u<<\s"
    \ end="\s>>"
    \ contained
    \ oneline
    \ contains=p6PodFormat

syn match p6PodFormat      "Z<>"                contained
syn match p6PodFormat      "E<\(\d\+\|\I\i*\)>" contains=p6PodEscape
syn match p6PodEscape      "\I\i*>"me=e-1       contained
syn match p6PodEscape      "\d\+>"me=e-1        contained
syn match p6PodConfig      ":[^#]*"             contained
syn match p6PodExtraConfig "^="                 contained
syn match p6PodVerbatim    "^\s.*"              contained

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

    HiLink p6InterpString    p6String
    HiLink p6LiteralString   p6String
    HiLink p6SubNonBracket   p6String
    HiLink p6SubBracket      p6String
    HiLink p6TransNonBracket p6String
    HiLink p6EscapedQuote    p6StringSpecial
    HiLink p6EscapedArrow    p6StringSpecial
    HiLink p6CharClass       p6StringSpecial
    HiLink p6RegexSpecial    p6StringSpecial

    HiLink p6Property        Tag
    HiLink p6Attention       Todo
    HiLink p6Type            Type
    HiLink p6Error           Error
    HiLink p6BlockLabel      Label
    HiLink p6Normal          Normal
    HiLink p6Number          Number
    HiLink p6String          String
    HiLink p6Regex           String
    HiLink p6Repeat          Repeat
    HiLink p6Keyword         Keyword
    HiLink p6Module          Keyword
    HiLink p6DeclareRoutine  Keyword
    HiLink p6VarStorage      Keyword
    HiLink p6FlowControl     Special
    HiLink p6Comment         Comment
    HiLink p6Shebang         PreProc
    HiLink p6ClosureTrait    PreProc
    HiLink p6Operator        Operator
    HiLink p6Contextualizer  Operator
    HiLink p6Routine         Function
    HiLink p6Quote           Delimiter
    HiLink p6TypeConstraint  PreCondit
    HiLink p6Exception       Exception
    HiLink p6Variable        Identifier
    HiLink p6RuleCall        Identifier
    HiLink p6Conditional     Conditional
    HiLink p6StringSpecial   SpecialChar

    HiLink p6PodPara         p6Pod
    HiLink p6PodAbbr         p6Pod
    HiLink p6PodDelim        p6Pod
    HiLink p6PodExtraConfig  p6PodCommand

    HiLink p6Pod             Comment
    HiLink p6PodCommand      Keyword
    HiLink p6PodType         Type
    HiLink p6PodConfig       Function
    HiLink p6PodFormat       Special
    HiLink p6PodVerbatim     SpecialComment

    delcommand HiLink
endif

" Syncing to speed up processing
syn sync maxlines=100
syn sync match p6SyncPod grouphere  p6PodAbbrRegion    "^=\S\+\>"
syn sync match p6SyncPod grouphere  p6PodDirectRegion  "^=\(config\|use\|encoding\)\>"
syn sync match p6SyncPod grouphere  p6PodParaRegion    "^=for\>"
syn sync match p6SyncPod grouphere  p6PodDelimRegion   "^=begin\>"
syn sync match p6SyncPod groupthere p6PodDelimRegion   "^=end\>"

let b:current_syntax = "perl6"
