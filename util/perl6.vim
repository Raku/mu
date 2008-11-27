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

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Recommended formatting options (see pugs::hack)
setlocal autoindent expandtab smarttab shiftround shiftwidth=4 softtabstop=4

" Billions of keywords
syn keyword p6Attn            ACHTUNG ATTN ATTENTION FIXME NB contained 
syn keyword p6Attn            todo Todo TODO TBD WTF XXX contained
" XXX subset declares a type, but not a module, how should it be classified?
syn keyword p6Module          module class role use require package enum
syn keyword p6Module          grammar subset
syn keyword p6Declarator      macro sub submethod method is but does trusts
syn keyword p6Declarator      multi only rule token regex category
syn keyword p6ScopeDeclarator let my our state temp has constant proto
syn keyword p6FlowControl     if else elsif unless  
syn keyword p6FlowControl     for foreach loop repeat while until when next
syn keyword p6FlowControl     last redo given not or and andthen orelse xor
syn keyword p6FlowControl     return default exit make
syn keyword p6ClosureTrait    BEGIN CHECK INIT START FIRST ENTER LEAVE KEEP
syn keyword p6ClosureTrait    UNDO NEXT LAST PRE POST END rw signature
syn keyword p6ClosureTrait    returns of parsed cached readonly ref copy
syn keyword p6ClosureTrait    inline tighter looser equiv assoc deep also
syn keyword p6Exception       die fail try CATCH CONTROL warn
syn keyword p6Property        constant prec key value irs ofs ors pos export
syn keyword p6Property        float int str true false int1 int2 int4 int8
syn keyword p6Property        int16 int32 int64 uint1 uint2 uint4 uint8
syn keyword p6Property        uint16 uint32 uint64 num16 num32 num64
syn keyword p6Property        complex16 complex32 complex64 complex128 buf8
syn keyword p6Property        buf16 buf32 buf64
syn keyword p6Property        WHAT HOW
syn keyword p6Type            Array Bool Class Code Hash Int IO Num NumRange 
syn keyword p6Type            Str StrRange Sub Role Rule Rat Complex Any
syn keyword p6Type            Scalar List
syn keyword p6Number          NaN Inf
syn keyword p6Function        substr index rindex
syn keyword p6Function        grep map sort join split reduce min max reverse
syn keyword p6Function        truncate zip cat roundrobin classify first 
syn keyword p6Function        keys values pairs defined delete exists elems
syn keyword p6Function        end kv arity assuming gather take pick
syn keyword p6Function        any all none one wrap
syn keyword p6Function        callsame callwith nextsame nextwith
syn keyword p6Function        pop push shift splice unshift  
syn keyword p6Function        abs exp log log10 rand sign sqrt sin cos tan
syn keyword p6Function        floor ceil round srand roots cis unpolar polar
syn keyword p6Function        p5chop chop p5chomp chomp lc lcfirst uc ucfirst
syn keyword p6Function        capitalize normalize pack unpack quotemeta comb
syn keyword p6Function        nfd nfc nfkd nfkc
syn keyword p6Function        printf sprintf caller evalfile run runinstead 
syn keyword p6Function        nothing want bless chr ord list item gmtime 
syn keyword p6Function        localtime time gethost getpw chroot getlogin
syn keyword p6Function        kill fork wait perl context
syn keyword p6FunctionIO      print open read write readline say seek close
syn keyword p6FunctionIO      opendir readdir slurp
syn keyword p6FunctionSpecial eval operator undef undefine 
syn keyword p6FunctionSpecial infix postfix prefix cirumfix postcircumfix
syn keyword p6Compare         eq ne lt le gt ge cmp == != < <= > >=

syn match p6Normal  "\w*::\w\+"
syn match p6Comment "#.*" contains=p6Attn
syn match p6Shebang "^#!.*"

" Variables, arrays, and hashes with ordinary \w+ names
syn match p6Type           "¢[:\.*^?]\?[a-zA-Z_]\w*"
syn match p6VarPlain       "\(::?\|[$@%][\!\.*^?]\?\)[a-zA-Z_]\w*"
syn match p6VarException   "\$![a-zA-Z]\@!"
syn match p6VarCapture     "\$[0-9\/]"
syn match p6VarPunctuation "\$\d\+"
syn match p6Invoke         "\(&\|[.:]/\)[a-zA-Z_]\w*"

syn cluster p6Interp
    \ add=p6VarPlain
    \ add=p6InterpExpression
    \ add=p6VarPunctuation
    \ add=p6VarException
    \ add=p6InterpClosure

" { ... } construct
syn region p6InterpExpression
    \ matchgroup=p6Variable
    \ start="{"
    \ skip="\\}"
    \ end="}"
    \ contains=TOP
    \ contained

" FIXME: This ugly hack will show up later on. Once again, don't try to fix it.
syn region p6ParenExpression
    \ start="\(<\s*\)\@<!("
    \ end=")"
    \ transparent

syn region p6BracketExpression
    \ start="\["
    \ end="]"
    \ transparent

" Double-quoted, qq, qw, qx, `` strings
syn region p6InterpString
    \ start=+"+
    \ skip=+\\"+
    \ end=+"+
    \ contains=@p6Interp
syn region p6InterpString
    \ start="«"
    \ end="»"
    \ contains=@p6Interp
syn region p6InterpString
    \ start="<<"
    \ end=">>"
    \ contains=@p6Interp

" Punctuation-delimited strings
syn region p6InterpString
    \ start="\<q[qwx]\(:\(!\?[A-Za-z0-9]\((\w\+)\)\?\)\+\)\?\s*\z([^a-zA-Z0-9:#_ ]\)"
    \ skip="\\\z1"
    \ end="\z1"
    \ contains=@p6Interp
syn region p6InterpString
    \ start="\<q[qwx]\(:\(!\?[A-Za-z0-9]\((\w\+)\)\?\)\+\)\?\s*{"
    \ skip="\\}"
    \ end="}"
    \ contains=@p6Interp
syn region p6InterpString
    \ start="\<q[qwx]\(:\(!\?[A-Za-z0-9]\((\w\+)\)\?\)\+\)\?\s*("
    \ skip="\\)"
    \ end=")"
    \ contains=@p6Interp
syn region p6InterpString
    \ start="\<q[qwx]\(:\(!\?[A-Za-z0-9]\((\w\+)\)\?\)\+\)\?\s*\["
    \ skip="\\]"
    \ end="]"
    \ contains=@p6Interp
syn region p6InterpString
    \ start="\<q[qwx]\(:\(!\?[A-Za-z0-9]\((\w\+)\)\?\)\+\)\?\s*<"
    \ skip="\\>"
    \ end=">"
    \ contains=@p6Interp

" Single-quoted, q, '' strings
syn region p6LiteralString
    \ start="'"
    \ skip="\\'"
    \ end="'"
syn region p6LiteralString
    \ start="<<\@!\(.*>\)\@="
    \ end=">\@<!>"

" Punctuation-delimited strings
syn region p6LiteralString
    \ start="\<q\(:\(!\?[A-Za-z0-9]\((\w\+)\)\?\)\+\)\?\s*\z([^a-zA-Z0-9:#_ ]\)"
    \ skip="\\\z1"
    \ end="\z1"
syn region p6LiteralString
    \ start="\<q\(:\(!\?[A-Za-z0-9]\((\w\+)\)\?\)\+\)\?\s*{"
    \ skip="\\}"
    \ end="}"
syn region p6LiteralString
    \ start="\<q\(:\(!\?[A-Za-z0-9]\((\w\+)\)\?\)\+\)\?\s*("
    \ skip="\\)"
    \ end=")"
syn region p6LiteralString
    \ start="\<q\(:\(!\?[A-Za-z0-9]\((\w\+)\)\?\)\+\)\?\s*\["
    \ skip="\\]"
    \ end="]"
syn region p6LiteralString
    \ start="\<q\(:\(!\?[A-Za-z0-9]\((\w\+)\)\?\)\+\)\?\s*<"
    \ skip="\\>"
    \ end=">"

" Numbers
syn match p6Number "\<\(\d*\.\d\+\|\d\+\.\d*\|\d\+\)\(e\d\+\)\{0,1}"
syn match p6Number "\<0o[0-7]\+"
syn match p6Number "\<0x[0-9a-fA-F]\+"

" => Operator
syn match p6InterpString "\w\+\s*=>"he=e-2

" :key<val>
syn match p6InterpString ":\w\+\(\s*\.\)\?\(<[^>]*>\)\?"hs=s+1

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
    \ start="\<\(m\|mm\|rx\)\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*\z([^a-zA-Z0-9_:(]\)"
    \ skip="\\\z1"
    \ end="\z1"
    \ contains=@p6Regexen

" m:[] m:{} and m:<>
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<\(m\|mm\|rx\)\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*\["
    \ skip="\\]"
    \ end="]"
    \ contains=@p6Regexen
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<\(m\|mm\|rx\)\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*{"
    \ skip="\\}"
    \ end="}"
    \ contains=@p6Regexen
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<\(m\|mm\|rx\)\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*<"hs=e
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
    \ start="\<s\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*\z([^a-zA-Z0-9_:(]\)"
    \ skip="\\\z1"
    \ end="\z1"me=e-1
    \ contains=@p6Regexen
    \ nextgroup=p6SubNonBracket
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<s\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*\["
    \ skip="\\]"
    \ end="]\_s*"
    \ contains=@p6Regexen
    \ nextgroup=p6SubBracket
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<s\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*{"
    \ skip="\\}"
    \ end="}\_s*"
    \ contains=@p6Regexen
    \ nextgroup=p6SubBracket
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<s\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*<"
    \ skip="\\>"
    \ end=">\_s*"
    \ contains=@p6Regexen
    \ nextgroup=p6SubBracket
syn region p6Regex
    \ matchgroup=p6Keyword
    \ start="\<tr\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*\z([^a-zA-Z0-9_:(]\)"
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
    \ contains=@p6Interp
    \ contained
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="\z(\W\)"
    \ skip="\\\z1"
    \ end="\z1"
    \ contains=@p6Interp
    \ contained
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="\["
    \ skip="\\]"
    \ end="]"
    \ contains=@p6Interp
    \ contained
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="{"
    \ skip="\\}"
    \ end="}"
    \ contains=@p6Interp
    \ contained
syn region p6SubBracket
    \ matchgroup=p6Keyword
    \ start="<"
    \ skip="\\>"
    \ end=">"
    \ contains=@p6Interp
    \ contained
syn region p6TransNonBracket
    \ matchgroup=p6Keyword
    \ contained
    \ start="\z(\W\)"
    \ skip="\\\z1"
    \ end="\z1"

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
"    \ contains=TOP
"    \ contained
syn region p6TestExpr
    \ start="<\(?\|!\)?{"
    \ end="}\s*>"
    \ contains=TOP
    \ contained

" Hash quoting (sortof a hack)
" syn match p6InterpString "{\s*\w\+\s*}"ms=s+1,me=e-1

syn match p6Normal "//"

hi link p6InterpString    p6String
hi link p6LiteralString   p6String
hi link p6SubNonBracket   p6String
hi link p6SubBracket      p6String
hi link p6TransNonBracket p6String
hi link p6Module          p6Keyword
hi link p6Compare         p6Keyword
hi link p6Declarator      p6Keyword
hi link p6ScopeDeclarator p6Keyword
hi link p6FlowControl     p6Keyword
hi link p6Function        p6Keyword
hi link p6FunctionIO      p6Keyword
hi link p6Pattern         p6Keyword
hi link p6VarPlain        p6Variable
hi link p6VarPunctuation  p6Variable
hi link p6VarCapture      p6Variable

hi link p6Attn            Todo
hi link p6Type            Type
hi link p6Invoke          Type
hi link p6Property        Type
hi link p6RegexSpecial    Type
hi link p6Error           Error
hi link p6Normal          Normal
hi link p6Number          Number
hi link p6String          String
hi link p6Regex           String
hi link p6Comment         Comment
hi link p6Exception       Special
hi link p6VarException    Special
hi link p6FunctionSpecial Special
hi link p6CharClass       Special
hi link p6Shebang         PreProc
hi link p6ClosureTrait    PreProc
hi link p6Keyword         Statement
hi link p6Variable        Identifier
hi link p6RuleCall        Identifier

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
    \ contains=p6PodAbbr
    \ contained

syn region p6PodAbbr
    \ start=""
    \ end="^\ze\(\s*$\|=\k\)"
    \ contains=@p6PodAmbient
    \ contained

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
    \ contains=p6PodDirectConfigRegion
    \ contained

syn region p6PodDirectConfigRegion
    \ matchgroup=p6PodConfig
    \ start=""
    \ end="^\ze\([^=]\|=\S\|$\)"
    \ contains=p6PodConfig,p6PodExtraConfig
    \ contained

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
    \ contains=p6PodPara,p6PodParaConfigRegion
    \ contained
    \ keepend

syn region p6PodParaConfigRegion
    \ matchgroup=p6PodConfig
    \ start=""
    \ end="^\ze\([^=]\|=\S\)"
    \ contains=p6PodConfig,p6PodExtraConfig
    \ contained

syn region p6PodPara
    \ start="^[^=]"
    \ end="^\ze\(\s*$\|=\S\)"
    \ contains=@p6PodAmbient
    \ contained
    \ extend

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
    \ contains=p6PodDelim,p6PodDelimConfigRegion
    \ contained

syn region p6PodDelimConfigRegion
    \ matchgroup=p6PodConfig
    \ start=""
    \ end="^\ze\([^=]\|=\S\|$\)"
    \ contains=p6PodConfig,p6PodExtraConfig
    \ contained

syn region p6PodDelim
    \ start="^"
    \ end="^\ze=end\>"
    \ contains=@p6PodNested,@p6PodAmbient
    \ contained

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
    \ contains=p6PodFormat
    \ oneline
    \ contained

syn region p6PodFormat
    \ start="\u«[^«]"me=e-1
    \ end="»"
    \ contains=p6PodFormat
    \ oneline
    \ contained

syn region p6PodFormat
    \ start="\u<<\s"
    \ end="\s>>"
    \ contains=p6PodFormat
    \ oneline
    \ contained

syn match p6PodFormat      "Z<>"                contained
syn match p6PodFormat      "E<\(\d\+\|\I\i*\)>" contains=p6PodEscape
syn match p6PodEscape      "\I\i*>"me=e-1       contained
syn match p6PodEscape      "\d\+>"me=e-1        contained
syn match p6PodConfig      ":[^#]*"             contained
syn match p6PodExtraConfig "^="                 contained
syn match p6PodVerbatim    "^\s.*"              contained

hi link p6PodPara         p6Pod
hi link p6PodAbbr         p6Pod
hi link p6PodDelim        p6Pod
hi link p6PodExtraConfig  p6PodCommand

hi link p6Pod             Comment
hi link p6PodCommand      Keyword
hi link p6PodType         Constant
hi link p6PodConfig       Identifier
hi link p6PodFormat       Special
hi link p6PodVerbatim     Special

" Syncing to speed up processing
syn sync maxlines=100
syn sync match p6SyncPod grouphere  p6PodAbbrRegion    "^=\S\+\>"
syn sync match p6SyncPod grouphere  p6PodDirectRegion  "^=\(config\|use\|encoding\)\>"
syn sync match p6SyncPod grouphere  p6PodParaRegion    "^=for\>"
syn sync match p6SyncPod grouphere  p6PodDelimRegion   "^=begin\>"
syn sync match p6SyncPod groupthere p6PodDelimRegion   "^=end\>"

let b:current_syntax = "perl6"
