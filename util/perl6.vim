" Vim syntax file
" Language: 	Perl 6
" Maintainer:	Luke Palmer <fibonaci@babylonia.flatirons.org>
" Last Change:	2002 Jun 12
"
" This is a big undertaking. Perl 6 is the sort of language that only Perl
" can parse. But I'll do my best to get vim to.

" Die if there's already a defined syntax
if exists("b:current_syntax")
  finish
endif

" Billions of keywords
syn keyword p6Module		module class use require
syn keyword p6KeyDecl		sub is but multi returns
syn keyword p6KeyScopeDecl	my our local let temp
syn keyword p6KeyFlow		for foreach loop while until if unless when
syn keyword p6KeyFlow		given next last redo or and err xor return not
syn keyword p6KeyException	die fail try CATCH
syn keyword p6KeyIO		print open read write readline say seek
syn keyword p6KeyProperty	constant prec key value kv irs ofs ors pos int export
syn keyword p6KeyProperty	float str true false rw
syn keyword p6KeyProperty	Int Str Num Hash Array Code Rule Class NumRange
syn keyword p6KeyProperty	StrRange Role Bool
syn keyword p6KeyFunc		map sort split reduce keys grep values truncate
syn keyword p6KeyFunc		defined exists
syn keyword p6KeySpecial	operator undef
syn keyword p6KeyCompare	eq ne lt le gt ge

syn match p6KeyIO "-[rwxoRWXOezsfdlpSbctugkTBMAC]"

" Comments
syn match p6Comment "#.*"

" POD
syn region p6POD start="^=[a-z]" end="^=cut"

" Variables, arrays, and hashes with ordinary \w+ names
syn match p6VarPlain "[$@%][a-zA-Z_]\w*"
syn match p6VarPlain "[$@%][:.][a-zA-Z_]\w*"
syn match p6VarPlain "\$\^\w\+"
syn match p6VarException "\$!"
syn match p6VarPunct	"\$\d\+"

syn cluster p6Interp contains=p6VarPlain,p6InterpExpression,p6VarPunct,p6VarException,p6InterpClosure

" $( ... ) construct
syn region p6InterpExpression contained matchgroup=p6Variable start=+\$(+ skip=+\\)+ end=+)+ contains=TOP

" FIXME: This ugly hack will show up later on. Once again, don't try to fix it.
syn region p6ParenExpression start="\(<\s*\)\@<!(" end=")" matchgroup=p6Error end="[\]}]" transparent
syn region p6BracketExpression start="\[" end="]" matchgroup=p6Error end="[})]" transparent

" Double-quoted, qq, qw, qx, `` strings
syn region p6InterpString start=+"+ skip=+\\"+ end=+"+ contains=@p6Interp
syn region p6InterpString start=+`+ skip=+\\`+ end=+`+ contains=@p6Interp
" \w-delimited strings
syn region p6InterpString start="q[qwx]\s\+\z([a-zA-Z0-9_]\)" skip="\\\z1" end="\z1" contains=@p6Interp
" Punctuation-delimited strings
syn region p6InterpString start="q[qwx]\s*\z([^a-zA-Z0-9_ ]\)" skip="\\\z1" end="\z1" contains=@p6Interp
syn region p6InterpString start="q[qwx]\s*{" skip="\\}" end="}" contains=@p6Interp
syn region p6InterpString start="q[qwx]\s*(" skip="\\)" end=")" contains=@p6Interp
syn region p6InterpString start="q[qwx]\s*\[" skip="\\]" end="]" contains=@p6Interp
syn region p6InterpString start="q[qwx]\s*<" skip="\\>" end=">" contains=@p6Interp

" Single-quoted, q, '' strings
syn region p6LiteralString start=+'+ skip=+\\'+ end=+'+
" \w-delimited strings
syn region p6LiteralString start="q\s\+\z([a-zA-Z0-9_]\)" skip="\\\z1" end="\z1"
" Punctuation-delimited strings
syn region p6LiteralString start="q\s*\z([^a-zA-Z0-9_ ]\)" skip="\\\z1" end="\z1"
syn region p6LiteralString start="q\s*\[" skip="\\]" end="]"
syn region p6LiteralString start="q\s*(" skip="\\)" end=")"
syn region p6LiteralString start="q\s*{" skip="\\}" end="}"
syn region p6LiteralString start="q\s*<" skip="\\>" end=">"

" Numbers
syn match  p6Number "\<\(\d*\.\d\+\|\d\+\.\d*\|\d\+\)\(e\d\+\)\{0,1}"
syn match  p6Number "\<0o[0-7]\+"
syn match  p6Number "\<0x[0-9a-fA-F]\+"
syn keyword p6Number NaN Inf

" => Operator
syn match  p6InterpString "\w\+\s*=>"he=e-2
" :key<val>
syn match  p6InterpString ":\w\+\(\s*\.\)\?\(<[^>]*>\)\?"hs=s+1


" Sexeger!
syn cluster p6Regexen contains=@p6Interp,p6Closure,p6Comment,p6CharClass,p6RuleCall,p6TestExpr,p6RegexSpecial

" Here's how we get into regex mode
" Standard /.../
syn region p6Regex matchgroup=p6Keyword start="\(\w\_s*\)\@<!/" start="\(\(\<split\|\<grep\)\s*\)\@<=/" skip="\\/" end="/" contains=@p6Regexen
" m:/.../
syn region p6Regex matchgroup=p6Keyword start="\<\(m\|rx\)\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*\z([^a-zA-Z0-9_:(]\)" skip="\\\z1" end="\z1" contains=@p6Regexen
" m:[] m:{} and m:<>
syn region p6Regex matchgroup=p6Keyword start="\<\(m\|rx\)\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*\[" skip="\\]" end="]" contains=@p6Regexen
syn region p6Regex matchgroup=p6Keyword start="\<\(m\|rx\)\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*{" skip="\\}" end="}" contains=@p6Regexen
syn region p6Regex matchgroup=p6Keyword start="\<\(m\|rx\)\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*<"hs=e skip="\\>" end=">" contains=@p6Regexen


" rule { }
syn region p6Regex start="rule\(\_s\+\w\+\)\{0,1}\_s*{"hs=e end="}" contains=@p6Regexen

" Closure (FIXME: Really icky hack, also doesn't support :blah modifiers)
" However, don't do what you might _expect_ would work, because it won't.
" And no variant of it will, either.  I found this out through 4 hours from
" miniscule tweaking to complete redesign.  This is the only way I've found!
syn region p6Closure start="\(\(rule\(\_s\+\w\+\)\{0,1}\|s\|rx\)\_s*\)\@<!{" end="}" matchgroup=p6Error end="[\])]"  contains=TOP   fold


" s:///, tr:///,  and all variants
syn region p6Regex matchgroup=p6Keyword start="\<s\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*\z([^a-zA-Z0-9_:(]\)" skip="\\\z1" end="\z1"me=e-1 nextgroup=p6SubNonBracket contains=@p6Regexen
syn region p6Regex matchgroup=p6KeyWord start="\<s\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*\[" skip="\\]" end="]\_s*" nextgroup=p6SubBracket contains=@p6Regexen
syn region p6Regex matchgroup=p6Keyword start="\<s\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*{" skip="\\}" end="}\_s*" nextgroup=p6SubBracket contains=@p6Regexen
syn region p6Regex matchgroup=p6Keyword start="\<s\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*<" skip="\\>" end=">\_s*" nextgroup=p6SubBracket contains=@p6Regexen
syn region p6Regex matchgroup=p6Keyword start="\<tr\_s*\(\_s*:\_s*[a-zA-Z0-9_()]\+\)*\_s*\z([^a-zA-Z0-9_:(]\)" skip="\\\z1" end="\z1"me=e-1 nextgroup=p6TransNonBracket

" This is kinda tricky. Since these are contained, they're "called" by the
" previous four groups. They just pick up the delimiter at the current location
" and behave like a string.

syn region p6SubNonBracket matchgroup=p6Keyword contained start="\z(\W\)" skip="\\\z1" end="\z1" contains=@p6Interp
syn region p6SubBracket matchgroup=p6Keyword contained start="\z(\W\)" skip="\\\z1" end="\z1" contains=@p6Interp
syn region p6SubBracket matchgroup=p6Keyword contained start="\[" skip="\\]" end="]" contains=@p6Interp
syn region p6SubBracket matchgroup=p6Keyword contained start="{" skip="\\}" end="}" contains=@p6Interp
syn region p6SubBracket matchgroup=p6Keyword contained start="<" skip="\\>" end=">" contains=@p6Interp
syn region p6TransNonBracket matchgroup=p6Keyword contained start="\z(\W\)" skip="\\\z1" end="\z1"


syn match p6RuleCall contained "<\s*!\{0,1}\s*\w\+"hs=s+1
syn match p6CharClass contained "<\s*!\{0,1}\s*\[\]\{0,1}[^]]*\]\s*>"
syn match p6CharClass contained "<\s*!\{0,1}\s*-\{0,1}\(alpha\|digit\|sp\|ws\|null\)\s*>"
syn match p6CharClass contained "\\[HhVvNnTtEeRrFfWwSs]"
syn match p6CharClass contained "\\[xX]\(\[[0-9a-f;]\+\]\|\x\+\)"
syn match p6CharClass contained "\\0\(\[[0-7;]\+\]\|\o\+\)"
syn region p6CharClass contained start="\\[QqCc]\[" end="]" skip="\\]"
syn match p6RegexSpecial contained "\\\@<!:\{1,3\}"
syn match p6RegexSpecial contained "<\s*\(cut\|commit\)\s*>"
"syn match p6RegexSpecial contained "\\\@<![+*|]"
syn match p6RegexSpecial contained ":="
syn region p6CharClass contained start=+<\s*!\{0,1}\s*\z(['"]\)+ skip=+\\\z1+ end=+\z1\s*>+
syn region p6TestExpr contained start="<\s*!\{0,1}\s*(" end=")\s*>" contains=TOP


" Hash quoting (sortof a hack)
" syn match p6InterpString "{\s*\w\+\s*}"ms=s+1,me=e-1

syn match p6Normal "//"


hi link p6Normal        Normal
hi link p6Regex         String
hi link p6SubNonBracket p6String
hi link p6SubBracket    p6String
hi link p6TransNonBracket p6String
hi link p6CharClass     Special
hi link p6RuleCall      Identifier
hi link p6RegexSpecial  Type

hi link p6Error         Error
hi link p6Module        p6Keyword
hi link p6KeyCompare    p6Keyword
hi link p6KeyDecl       p6Keyword
hi link p6KeyScopeDecl  p6Keyword
hi link p6KeyFlow       p6Keyword
hi link p6KeyException  Special
hi link p6KeyIO         p6Keyword
hi link p6KeyProperty   Type
hi link p6KeyFunc       p6Keyword
hi link p6KeySpecial    Special
hi link p6KeyType       Type

hi link p6Pattern       p6Keyword
hi link p6VarPlain      p6Variable
hi link p6VarPunct      p6Variable
hi link p6InterpString  p6String
hi link p6LiteralString p6String

hi link p6Keyword  Statement
hi link p6Number   Number
hi link p6Comment  Comment
hi link p6POD      Comment
hi link p6Variable Identifier
hi link p6VarException Special
hi link p6String   String

let b:current_syntax = "perl6"
