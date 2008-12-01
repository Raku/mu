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
"   * syntax for reading from stdin: =<> or from arbitrary file handles:
"     =<$fh>
"   * List initialization via the @a = <foo bar> construct brakes when there
"     is a newline between '<' and '>'
"   * The regex regex_name { ... } syntax for regexes/tokens seems to be
"     unsupported
"   * Improve POD formatting codes support (S<>, etc) 
"   * Implement a "perl_string_as_statement" feature like perl.vim does

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
syn keyword p6Declarator      macro sub submethod method is but does trusts
syn keyword p6Declarator      multi only rule token regex category
syn keyword p6ScopeDeclarator let my our state temp has proto
syn keyword p6Conditional     if else elsif unless  
syn keyword p6Repeat          for loop repeat while until
syn keyword p6FlowControl     when next last redo given return default
syn keyword p6FlowControl     exit make
syn keyword p6ClosureTrait    BEGIN CHECK INIT START FIRST ENTER LEAVE KEEP
syn keyword p6ClosureTrait    UNDO NEXT LAST PRE POST END rw signature
syn keyword p6ClosureTrait    returns of parsed cached readonly ref copy
syn keyword p6ClosureTrait    inline tighter looser equiv assoc deep also
syn keyword p6Exception       die fail try CATCH CONTROL warn
syn keyword p6Property        constant prec key value irs ofs ors pos export
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
syn keyword p6Function        substr index rindex
syn keyword p6Function        grep map sort join split reduce min max reverse
syn keyword p6Function        truncate zip cat roundrobin classify first 
syn keyword p6Function        keys values pairs defined delete exists elems
syn keyword p6Function        end kv arity assuming gather take pick
syn keyword p6Function        any all none one wrap shape classify
syn keyword p6Function        callsame callwith nextsame nextwith
syn keyword p6Function        pop push shift splice unshift floor ceiling
syn keyword p6Function        abs exp log log10 rand sign sqrt sin cos tan
syn keyword p6Function        round srand roots cis unpolar polar atan2
syn keyword p6Function        p5chop chop p5chomp chomp lc lcfirst uc ucfirst
syn keyword p6Function        capitalize normalize pack unpack quotemeta comb
syn keyword p6Function        samecase sameaccent chars nfd nfc nfkd nfkc
syn keyword p6Function        printf sprintf caller evalfile run runinstead 
syn keyword p6Function        nothing want bless chr ord list item gmtime 
syn keyword p6Function        localtime time gethost getpw chroot getlogin
syn keyword p6Function        kill fork wait perl context graphs codes bytes
syn keyword p6Function        print open read write readline say seek close
syn keyword p6Function        opendir readdir slurp pos fmt vec
syn keyword p6Function        eval operator undef undefine sleep
syn keyword p6Function        infix postfix prefix circumfix postcircumfix
syn keyword p6Operator        x xx div mod also leg cmp
syn keyword p6Operator        eq ne lt le gt ge eqv ff fff true not Z minmax
syn keyword p6Operator        X XeqvX and andthen or xor orelse extra

" more operators (not very smart, allows any combination)
syn match p6Operator display "\%(+\|-\|/\|\*\|\~\|?\||\|\\\|=\|\^\|!\|%\)"
syn match p6Operator display "\%(&\|<\|>\|,\|\.\|;\|:\|\[\S\+\]\)"
" these require end-of-word on the right side
syn match p6Operator display "\<\%(!eqv\|X\~X\|X\*X\)\>"
" these require end-of-word on both sides
syn match p6Operator display "\<\%(xx=\|p5=>\)"

" misc
syn match p6Normal  display "\w*::\w\+"
syn match p6Comment display "#.*" contains=p6Attention
syn match p6Shebang display "\%^#!.*"

" Variables, arrays, and hashes with ordinary \w+ names
"syn match p6Type         display "¢[:.*^?]\?[[:alpha:]_]\w*"
syn match p6VarPlain     display "\(\w\@<!::?\?\|[$@%][!.*^?]\?\)[[:graph:]_¢]\w*"
syn match p6VarException display "\$![[:alpha:]]\@!"

" { ... } construct
syn region p6InterpExpression
    \ matchgroup=p6Variable
    \ start="{"
    \ skip="\\}"
    \ end="}"
    \ contained
    \ contains=TOP

syn cluster p6Interp
    \ add=p6VarPlain
    \ add=p6InterpExpression
    \ add=p6VarException
    \ add=p6InterpClosure

" FIXME: This ugly hack will show up later on. Once again, don't try to fix it.
syn region p6ParenExpression
    \ start="\(<\s*\)\@<!("
    \ end=")"
    \ transparent

syn region p6BracketExpression
    \ start="\["
    \ end="]"
    \ transparent

" contextualizers
syn region p6ExplicitContext
    \ matchgroup=p6Contextualizer
    \ start="\$("
    \ start="@("
    \ start="%("
    \ start="&("
    \ start="@@("
    \ end=")"
    \ transparent
    \ contains=TOP

" Double-quoted, qq, qw, qx, `` strings
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start=+"+
    \ skip=+\\"+
    \ end=+"+
    \ contains=@p6Interp
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start="«"
    \ end="»"
    \ contains=@p6Interp
syn region p6InterpString
    \ matchgroup=p6Quote
    \ start="<<"
    \ end=">>"
    \ contains=@p6Interp

" Punctuation-delimited strings
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

" Single-quoted, q, '' strings
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="'"
    \ skip="\\'"
    \ end="'"
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="<<\@!\(.*>\)\@="
    \ end=">\@<!>"
" special hack for $<etc>
syn region p6LiteralString
    \ matchgroup=p6Quote
    \ start="\$<\(.*>\)\@="
    \ end=">\@<!>"

" Punctuation-delimited strings
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

" => and p5=> autoquoting
syn match p6LiteralString display "\w\+\ze\s\+p5=>"
syn match p6LiteralString display "\w\+\ze\(p5\)\@<!=>"
syn match p6LiteralString display "\w\+\ze\s\+=>"
syn match p6LiteralString display "\w\+p5\ze=>"

" this is an infix operator, not a quote
syn match p6Operator display "\s\zs<=>"

" :key<val>
syn match p6LiteralString display ":\@<=\w\+"

" Sexeger!
syn cluster p6Regexen
    \ add=@p6Interp
    \ add=p6Closure
    \ add=p6Comment
    \ add=p6CharClass
    \ add=p6RuleCall
    \ add=p6TestExpr
    \ add=p6RegexSpecial

" Here's how we get into regex mode
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

" This is in operator, not a regex
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
    HiLink p6Module          p6Keyword
    HiLink p6Declarator      p6Keyword
    HiLink p6ScopeDeclarator p6Keyword
    HiLink p6FlowControl     p6Keyword
    HiLink p6VarPlain        p6Variable
    HiLink p6VarException    p6Exception

    HiLink p6Attention       Todo
    HiLink p6Type            Type
    HiLink p6Property        Type
    HiLink p6Error           Error
    HiLink p6Normal          Normal
    HiLink p6Number          Number
    HiLink p6String          String
    HiLink p6Regex           String
    HiLink p6Repeat          Repeat
    HiLink p6Keyword         Keyword
    HiLink p6Comment         Comment
    HiLink p6CharClass       Special
    HiLink p6Shebang         PreProc
    HiLink p6ClosureTrait    PreProc
    HiLink p6Operator        Operator
    HiLink p6Contextualizer  Operator
    HiLink p6Function        Function
    HiLink p6Quote           Delimiter
    HiLink p6Exception       Exception
    HiLink p6Variable        Identifier
    HiLink p6RuleCall        Identifier
    HiLink p6Conditional     Conditional
    HiLink p6RegexSpecial    SpecialChar

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
