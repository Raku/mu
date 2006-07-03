" Vim Compiler File
" Mangled and partially updated for new ghci by Gaal Yahas for Pugs
" :so this file and ":make ghci", quickfix should work
" Original metadata:
" Compiler:	GHCi
" Maintainer:	Claus Reinke <C.Reinke@ukc.ac.uk>
" Last Change:	25 July 2002
" ------------------------------ paths & quickfix settings first
"
if exists("current_compiler") && current_compiler == "ghci"
" finish
endif
let current_compiler = "ghci"
echo current_compiler

" okay, let's go searching for that executable..
" try to set path and makeprg (for quickfix mode) as well
"
if has("win32")
  if executable("C:\\ghc\\ghc-5.04\\bin\\ghci")
    setlocal makeprg=echo\ :q\ \\\|\ C:\\ghc\\ghc-5.04\\bin\\ghci\ % 
    let g:haskell_path = "C:\\ghc\\ghc-5.04\\bin\\"
  endif
endif

" if you have the source code for ghc's prelude and libraries,
" you'll want to add it's root directory to the path. the same
" goes for any other Haskell libraries you're using. 
" unfortunately, with ghc, we can't assume that we've got any sources..
"
"if has("unix") && isdirectory("/usr/local/share/hugs")
"  set path+=/usr/local/share/hugs/lib/**
"elseif has("win32") && exists("g:haskell_path") && g:haskell_path != ""
"  execute "set path+=".escape(g:haskell_path,'\ ')."lib\\\\**"
"endif

if executable("ghci")
  "setlocal makeprg=echo\ :q\ \\\|\ ghci\ % 
  let g:haskell_path = ""
endif

if exists("g:haskell_path") " gotcha!
  let g:haskell=g:haskell_path."ghci "
else
  echo "sorry, can't find ghci - please set path by hand!"
  finish
endif

" quickfix mode: 
" ignore banner and empty lines, 
" fetch file/line-info from error message
setlocal errorformat=%-GCompiling%.%#,
                    \%E%f:%l:%c:\ %m,
                    \%E%f:%l:%c:%m,
                    \%E%f:%l:%c:,
                    \%+C\ \ %#%m,
                    \%+C\ \ %#%m
                    "\%+GFailed,\ %m,
                    "\%+GOk,\ %m,
                    "\%+I%.%#
                    "\%-G\\s%#,
                    "\%+I%.%#,
                    "\%C\\s%#
"%-I\ \ \ ___\ \ \ \ \ \ \ \ \ ___\ _,

" oh, wouldn't you guess it - ghci reports (partially) to stderr..
setlocal shellpipe=2>

" ------------------------- but ghci can do a lot more for us..
"
if exists("g:haskell_functions")
"  finish
endif
let g:haskell_functions = "ghci"

" avoid hit-enter prompts
set cmdheight=3

" os-specific external tools, and hugs editor settings
if has("win32")
  let s:grep="find"
  let s:ngrep="find /v"
  let s:esc='<>%'
  let s:editor=""
else
  let s:grep="grep"
  let s:ngrep="grep -v"
  let s:esc='$<>()'
  let s:editor="-E\"xterm -e vim +%d %s\" "
  " let s:editor="-E\"gvim +%d %s\" "
endif

" ------------------------------  for insert-mode completion
" (and to make gf work on imported modules, in combination with path)
set include=^import
set includeexpr=substitute(v:fname,\"$\",\".hs\",\"\")

" ------------------------------------------ key bindings
" browse current (_B,_e) or imported module (_i)
" insert resulting type infos as comment at top of file (_b)
" or as module export header (_e)
" or as module import list (_i)
" insert type declaration in new line before current (_t)
" find definition of current selection (_f)
" XXX FIXME - need to unbreak _i and _b that had bitrotted/never worked on " unix? -- gaal
"map _i 0/[A-Z]/<CR>:noh<CR>"bye:call Browse(@b)<CR>$a ( )"bP:call NoBanner()<cr>k$:call Format()<CR>
map _B :call Browse("")<CR>:call New()<CR>"bP:call NoBanner()<cr>
"map _b :call Browse("")<CR>1GO{--}1G"bp:call NoBanner()<cr>
map _e :call Browse("")<CR>1G:call Module()<CR>1GwgUl"bp:call NoBanner()<cr>k$:call Format()<CR>
map _I 0/[A-Z]/<CR>:noh<CR>"bye:call Browse(@b)<CR>:call New()<CR>"bp:call NoBanner()<cr>
map _t "ty:call AddTypeDecl(@t)<CR>
map _T "ty:call ShowType(@y)<cr>
map _f "fy:call Find(@f)<CR>

" --------------------------------------------------- menus
if has("menu")
  if exists("b:haskell_menu")
    aunmenu Haskell(GHCi)
  endif

  100amenu Haskell(GHCi).-Change-                    :
  amenu Haskell(GHCi).AddImportDecls<tab>_i       _i
  amenu Haskell(GHCi).AddModuleHeader<tab>_e      _e
  amenu Haskell(GHCi).AddTypeDecl<tab>{visual}_t  :call AddTypeDecl(expand("<cword>"))<cr>
  amenu Haskell(GHCi).-Browse-                    :
  amenu Haskell(GHCi).BrowseCurrent<tab>_B        _B
  amenu Haskell(GHCi).BrowseImport<tab>_I         _I
  amenu Haskell(GHCi).ShowType                    :call ShowType(expand("<cword>"))<cr>
  amenu Haskell(GHCi).ShowInfo                    :call ShowInfo(expand("<cword>"))<cr>
  amenu Haskell(GHCi).FindDecl<tab>{visual}_f     viw_f
  amenu Haskell(GHCi).DeclarationsMenu            :call DeclarationsMenu()<cr>
  amenu Haskell(GHCi).-Set\ Options-              :
  amenu Haskell(GHCi).ToggleAutoType(off)         :call ToggleAutoType()<cr>
  let b:haskell_menu = "yes"

  " update delarations menu
  " au BufRead *.hs nested call DeclarationsMenu()
  " au BufWritePost *.hs nested call DeclarationsMenu()

  " create a menu with all declarations in current module,
  " as returned by browse (Problem: how to locate the items?
  " LocalFind is unreliable, Find tends to open new windows..)
  function! DeclarationsMenu()
    if exists("b:haskell_declarations_menu")
      aunmenu Declarations
    endif
    110amenu Declarations.Refresh :call DeclarationsMenu()<cr>
    let output = Browse("")
    while output != ""
      let line   = matchstr(output,"^[^\n]\*\n")
      let output = substitute(output,"^[^\n]*\n","","")
      let ident  = matchstr(line,'\S\+')
      let menuitem = "amenu Declarations.".ident.' :call LocalFind("'.ident.'")<cr>'
      "let menuitem = "amenu Declarations.".ident.' :call Find("'.ident.'")<cr>'
      "put =menuitem
      execute menuitem
    endwhile
    let b:haskell_declarations_menu = "yes"
  endfunction

  " try to locate a definition in the current file, without
  " asking hugs to start another editor instance
  " todo: literate style with \begin{code} .. :-(
  function! LocalFind(what)
    let literate = fnamemodify(expand("%"),":e") == "lhs"
    normal 1G
    let pat  = '\<'.a:what.'\>'
    if literate
      let line = search('^>\s\*'.pat)
    else
      let line = search('^'.pat)
    endif
    if line == 0
      call search(pat)
    endif
    nohlsearch
  endfunction
endif

" automatic type info, don't update too often while each call 
" goes to a fresh haskell interpreter.. can be irritating, so 
" default is off
let s:autotype = "off"
function! ToggleAutoType()
  if s:autotype == "on"
    au! CursorHold
    let s:autotype = "off"
    aunmenu Haskell(GHCi).ToggleAutoType(on)
    amenu Haskell(GHCi).ToggleAutoType(off) :call ToggleAutoType()<cr>
  else
    set updatetime=3000
    au CursorHold *.hs nested call ShowType(expand("<cword>"))
    let s:autotype = "on"
    aunmenu Haskell(GHCi).ToggleAutoType(off)
    amenu Haskell(GHCi).ToggleAutoType(on) :call ToggleAutoType()<cr>
  endif
endfunction

" if I ever figure out how to create a hidden buffer,
" we could replace the automatic type-info with a lookup
" there (as in the emacs haskell mode). would be a lot more
" efficient/useable.. we'd need to keep the buffer updated 
" and it would have to include type declarations for
" all imported modules, too.
function! ShowDeclType(thing)
  if a:thing != ""
    let thisModule = fnamemodify(expand("%"),":t:r")
    let name       = thisModule . "-Types"
    let thisbnr    = bufnr("%")
    let thatbnr    = bufnr(name)
    execute "buffer ".thatbnr
    let lnr  = search(a:thing)
    let line = getline(lnr)
    execute "buffer ".thisbnr
    echo line
  endif
endfunction

" show type declaration of thing, as given by haskell interpreter
" (as we wan't a declaration, not just the type, we use :i here)
"
function! ShowType(thing)
  if a:thing != ""
    echo "------------------------------"
    echo Type("echo :i",a:thing," | ".s:grep." \"::\"") 
  endif
endfunction

" show info for thing, as given by haskell interpreter
"
function! ShowInfo(thing)
  if a:thing != ""
    echo "------------------------------"
    echo Type("echo :i",a:thing," | ".s:ngrep." \"Leaving GHCi\"") 
  endif
endfunction

" add new line with type dec, as given by haskell interpreter
"
function! AddTypeDecl(thing)
  if a:thing != ""
    call Type("echo :i",a:thing," | ".s:grep." \"::\"") 
    put! t
  endif
endfunction

" ask about type of/info for current selection
" cmd:      "echo :i" or "echo :t"
" thing:    the selection
" postcmd:  typically a filter
"
function! Type(cmd,thing,postcmd)
  let output=system(a:cmd.escape(" ".a:thing,s:esc)." | ".g:haskell.expand("%").a:postcmd)
  let output=substitute(output,"[^>]*> ","","")
  let @t = output
  return output
endfunction

" load current file into GHCi,
" try to extract file name from info for definition of current selection
"
function! Find(what)
  if a:what != ""
    let output  =Type("echo :i",a:what," | ".s:grep." \"defined at\"") 
    if match(output,"defined at") != -1
      let location=substitute(output,'^.* defined at \(\S\+\):\(\d\+\)\D','+\2 \1','')
      echo location
      let editCall = 'edit '.location
      exe editCall
    else
      echo "no source location found"
      call ShowInfo(a:what)
    endif
  endif
endfunction

" create a new scratch buffer
"
function! New()
  new
  set buftype=nofile
  set bufhidden=delete
  set noswapfile
endfunction

" try to get rid of GHCi's intro banner and exit message
function! NoBanner()
  "let start = line(".")
  "let end   = search('Ok, modules loaded')
  "exe "g/Leaving GHCi/d"
  "exe start . "," . end . "d"
endfunction

" invoke the haskell interpreter's browse function
"
function! Browse(module)
  if a:module == ""
    let haskellCmd="echo :browse *Main "
  else
    let haskellCmd="echo :browse ".a:module." "
  endif
  " let output=system(haskellCmd." | ".g:haskell.expand("%")." | ".s:grep." \"::\"")
  let output=system(haskellCmd." | ".g:haskell.expand("%"))
  let output=substitute(output,"^.*Ok, modules loaded[^\n]*\n","","") 
  let output=substitute(output,"*[^>]*> ","","") 
  let output=substitute(output,"*[^>]*> Leaving GHCi.\n","","") 
  let @b=substitute(output,"^\s*\n","","")
  return @b
endfunction

" create default module header from current file name
"
function! Module()
  call append(0,"")
  call append(0," ) where")
  call append(0,"module ".fnamemodify(expand("%"),":t:r")." (")
endfunction

" try to make some sense of GHCi's browse output; modify buffer between
" matching parentheses to get rid of stuff that doesn't belong into an
" import/export list and to line up the type declarations, as comments
function! Format()
  let start = line(".")
  let end   = searchpair('(','',')','n')
  let m=0
  let l0 = line(".")+1
  let l=l0
  let m1=stridx(getline(l),"::")
  while l < end
    if m1 > m
      let m=m1
    endif
    let l=l+1
    let m1=stridx(getline(l),"::")
  endw
  let l=l0
  let m1=stridx(getline(l),"::")
  while l < end
    if l > l0
      let prefix = " ,"
    else
      let prefix = "  "
    endif
    let it=getline(l)
    let it=substitute(it,"class ","{- class -} ","")
    let it=substitute(it,"where {","{- where { -} ","")
    if m1 > -1
      call setline(l,prefix.substitute(it,"::",X(m-m1," ")."-- ::",""))
    else
      call setline(l,prefix.it)
    endif
    let l=l+1
    let m1=stridx(getline(l),"::")
  endw
endfunction

" return n copies of s
"
function! X(n,s)
  let r=""
  let c=a:n
  while c > 0
    let r=r.a:s
    let c=c-1
  endw
  return r
endfunction

