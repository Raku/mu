module ASTParser where
{-----------------------
"Words. Nothing but sweet, sweet words that turn into bitter orange wax in my ears."
-Fry, Futurama

This module parses yaml files created by Larry Wall's MADSKILLS parser.
It is designed to be used with ASTTranslate, but feel free to use it elsewhere
if it's useful.

Parsec is used for parsing, which should come standard with GHC.

See ASTTranslate.hs for more.

Originally created by Sage LaTorra for Summer of Code 2006.
------------------------}
import Text.ParserCombinators.Parsec
import IO hiding (try)
import ASTUtil
import ASTDefinition

{-------------
nodeNamer is parsec parser that parses nodes,
recursivley parsing child nodes. It has two distinct cases,
one for nodes with kids, one for all other nodes.
---------------}
nodeNamer :: Int -> Parser P5AST
nodeNamer indent = do
    choice[(count indent space),(count 0 space)] --The count 0 space option accounts for nodes after the chomp modifier on a uni block, since the uni block will consume everything up to the '-'
    choice [hereDoc indent, withKids indent , noKids indent , blank indent]

blank :: Int -> Parser P5AST
blank indent = do
    try (string "- ''")
    newline
    return (LiteralNode Junk "1" "")

withKids :: Int -> Parser P5AST
withKids indent = do
    try (string "- !perl/P5AST::") <?> "P5AST decleration";
    name <- manyTill anyChar space
    newline
    spaces
    string "Kids: " <?> "Kids"
    modifier <- manyTill anyChar newline
    kids <- case modifier of
        "[]"    -> (newline <?> "newline") >> return []
        _       -> many . try $ nodeNamer (indent+4)
    let con = case name of
            "attrlist"      -> Attrlist
            "bindop"        -> Bindop
            "cfor"          -> Cfor
            "condmod"       -> Condmod
            "condstate"     -> Condstate
            "doblock"       -> Doblock
            "format"        -> Format
            "listelem"      -> Listelem
            "nothing"       -> PNothing
            "op_aassign"    -> Op_aassign
            "op_abs"        -> Op_abs
            "op_add"        -> Op_add
            "op_aelem"      -> Op_aelem
            "op_alarm"      -> Op_alarm
            "op_and"        -> Op_and
            "op_andassign"  -> Op_andassign
            "op_anonhash"   -> Op_anonhash
            "op_anonlist"   -> Op_anonlist
            "op_aslice"     -> Op_aslice
            "op_atan2"      -> Op_atan2
            "op_av2arylen"  -> Op_av2arylen
            "op_backtick"   -> Op_backtick
            "op_binmode"    -> Op_binmode
            "op_bit_and"    -> Op_bit_and
            "op_bit_or"     -> Op_bit_or
            "op_bit_xor"    -> Op_bit_xor
            "op_bless"      -> Op_bless
            "op_caller"     -> Op_caller
            "op_chdir"      -> Op_chdir
            "op_chmod"      -> Op_chmod
            "op_chomp"      -> Op_chomp
            "op_chop"       -> Op_chop
            "op_chown"      -> Op_chown
            "op_chr"        -> Op_chr
            "op_chroot"     -> Op_chroot
            "op_close"      -> Op_close
            "op_closedir"   -> Op_closedir
            "op_complement" -> Op_complement
            "op_concat"     -> Op_concat
            "op_cond_expr"  -> Op_cond_expr
            "op_const"      -> Op_const
            "op_crypt"      -> Op_crypt
            "op_defined"    -> Op_defined
            "op_delete"     -> Op_delete
            "op_die"        -> Op_die
            "op_divide"     -> Op_divide
            "op_dofile"     -> Op_dofile
            "op_dor"        -> Op_dor
            "op_dorassign"  -> Op_dorassign
            "op_each"       -> Op_each
            "op_egrent"     -> Op_egrent
            "op_entereval"  -> Op_entereval
            "op_enteriter"  -> Op_enteriter
            "op_entersub"   -> Op_entersub
            "op_entertry"   -> Op_entertry
            "op_enterwrite" -> Op_enterwrite
            "op_eof"        -> Op_eof
            "op_epwent"     -> Op_epwent
            "op_eq"         -> Op_eq
            "op_exec"       -> Op_exec
            "op_exists"     -> Op_exists
            "op_exp"        -> Op_exp
            "op_fcntl"      -> Op_fcntl
            "op_fileno"     -> Op_fileno
            "op_flip"       -> Op_flip
            "op_flop"       -> Op_flop
            "op_formline"   -> Op_formline
            "op_ftatime"    -> Op_ftatime
            "op_ftbinary"   -> Op_ftbinary
            "op_ftblk"      -> Op_ftblk
            "op_ftchr"      -> Op_ftchr
            "op_ftctime"    -> Op_ftctime
            "op_ftdir"      -> Op_ftdir
            "op_fteexec"    -> Op_fteexec
            "op_fteowned"   -> Op_fteowned
            "op_fteread"    -> Op_fteread
            "op_ftewrite"   -> Op_ftewrite
            "op_ftfile"     -> Op_ftfile
            "op_ftis"       -> Op_ftis
            "op_ftlink"     -> Op_ftlink
            "op_ftmtime"    -> Op_ftmtime
            "op_ftsize"     -> Op_ftsize
            "op_ftsock"     -> Op_ftsock
            "op_ftsuid"     -> Op_ftsuid
            "op_fttext"     -> Op_fttext
            "op_fttty"      -> Op_fttty
            "op_ftzero"     -> Op_ftzero
            "op_glob"       -> Op_glob
            "op_goto"       -> Op_goto
            "op_ge"         -> Op_ge
            "op_gelem"      -> Op_gelem
            "op_getc"       -> Op_getc
            "op_getppid"    -> Op_getppid
            "op_ghbyaddr"   -> Op_ghbyaddr
            "op_gmtime"     -> Op_gmtime
            "op_gpwnam"     -> Op_gpwnam
            "op_gpwent"     -> Op_gpwent
            "op_gpwuid"     -> Op_gpwuid
            "op_ggrent"     -> Op_ggrent
            "op_ggrnam"     -> Op_ggrnam
            "op_ggrgid"     -> Op_ggrgid
            "op_grepstart"  -> Op_grepstart
            "op_grepwhile"  -> Op_grepwhile
            "op_gt"         -> Op_gt
            "op_gv"         -> Op_gv
            "op_helem"      -> Op_helem
            "op_hex"        -> Op_hex
            "op_hslice"     -> Op_hslice
            "op_i_add"      -> Op_i_add
            "op_i_divide"   -> Op_i_divide
            "op_i_eq"       -> Op_i_eq
            "op_i_lt"       -> Op_i_lt
            "op_i_modulo"   -> Op_i_modulo
            "op_i_multiply" -> Op_i_multiply
            "op_i_ncmp"     -> Op_i_ncmp
            "op_i_ne"       -> Op_i_ne
            "op_i_negate"   -> Op_i_negate
            "op_i_postdec"  -> Op_i_postdec
            "op_i_postinc"  -> Op_i_postinc
            "op_i_predec"   -> Op_i_predec
            "op_i_preinc"   -> Op_i_preinc
            "op_i_subtract" -> Op_i_subtract
            "op_index"      -> Op_index
            "op_int"        -> Op_int
            "op_ioctl"      -> Op_ioctl
            "op_iter"       -> Op_iter
            "op_join"       -> Op_join
            "op_keys"       -> Op_keys
            "op_kill"       -> Op_kill
            "op_last"       -> Op_last
            "op_lc"         -> Op_lc
            "op_lcfirst"    -> Op_lcfirst
            "op_le"         -> Op_le
            "op_leave"      -> Op_leave
            "op_leaveloop"  -> Op_leaveloop
            "op_leavetry"   -> Op_leavetry
            "op_left_shift" -> Op_left_shift
            "op_length"     -> Op_length
            "op_lineseq"    -> Op_lineseq
            "op_link"       -> Op_link
            "op_list"       -> Op_list
            "op_localtime"  -> Op_localtime
            "op_log"        -> Op_log
            "op_lslice"     -> Op_lslice
            "op_lstat"      -> Op_lstat
            "op_lt"         -> Op_lt
            "op_mapstart"   -> Op_mapstart
            "op_mapwhile"   -> Op_mapwhile
            "op_match"      -> Op_match
            "op_method"     -> Op_method
            "op_mkdir"      -> Op_mkdir
            "op_modulo"     -> Op_modulo
            "op_msgctl"     -> Op_msgctl
            "op_msgget"     -> Op_msgget
            "op_msgrcv"     -> Op_msgrcv
            "op_msgsnd"     -> Op_msgsnd
            "op_multiply"   -> Op_multiply
            "op_ncmp"       -> Op_ncmp
            "op_ne"         -> Op_ne
            "op_negate"     -> Op_negate
            "op_next"       -> Op_next
            "op_not"        -> Op_not
            "op_null"       -> Op_null
            "op_oct"        -> Op_oct
            "op_open"       -> Op_open
            "op_open_dir"   -> Op_open_dir
            "op_or"         -> Op_or
            "op_orassign"   -> Op_orassign
            "op_ord"        -> Op_ord
            "op_pack"       -> Op_pack
            "op_padav"      -> Op_padav
            "op_padhv"      -> Op_padhv
            "op_padsv"      -> Op_padsv
            "op_pipe_op"    -> Op_pipe_op
            "op_pop"        -> Op_pop
            "op_pos"        -> Op_pos
            "op_postdec"    -> Op_postdec
            "op_postinc"    -> Op_postinc
            "op_pow"        -> Op_pow
            "op_predec"     -> Op_predec
            "op_preinc"     -> Op_preinc
            "op_print"      -> Op_print
            "op_prototype"  -> Op_prototype
            "op_prtf"       -> Op_prtf
            "op_push"       -> Op_push
            "op_pushmark"   -> Op_pushmark
            "op_qr"         -> Op_qr
            "op_quotemeta"  -> Op_quotemeta
            "op_rand"       -> Op_rand
            "op_range"      -> Op_range
            "op_read"       -> Op_read
            "op_readdir"    -> Op_readdir
            "op_readline"   -> Op_readline
            "op_readlink"   -> Op_readlink
            "op_redo"       -> Op_redo
            "op_ref"        -> Op_ref
            "op_refgen"     -> Op_refgen
            "op_regcomp"    -> Op_regcomp
            "op_regcreset"   -> Op_regcreset
            "op_rename"     -> Op_rename
            "op_repeat"     -> Op_repeat
            "op_require"    -> Op_require
            "op_reset"      -> Op_reset
            "op_return"     -> Op_return
            "op_reverse"    -> Op_reverse
            "op_rewinddir"  -> Op_rewinddir
            "op_right_shift"-> Op_right_shift
            "op_rindex"     -> Op_rindex
            "op_rmdir"      -> Op_rmdir
            "op_rv2av"      -> Op_rv2av
            "op_rv2cv"      -> Op_rv2cv
            "op_rv2gv"      -> Op_rv2gv
            "op_rv2hv"      -> Op_rv2hv
            "op_rv2sv"      -> Op_rv2sv
            "op_sassign"    -> Op_sassign
            "op_scalar"     -> Op_scalar
            "op_schomp"     -> Op_schomp
            "op_schop"      -> Op_schop
            "op_scmp"       -> Op_scmp
            "op_scope"      -> Op_scope
            "op_seek"       -> Op_seek
            "op_select"     -> Op_select
            "op_seq"        -> Op_seq
            "op_setpgrp"    -> Op_setpgrp
            "op_setpriority"-> Op_setpriority
            "op_sge"        -> Op_sge
            "op_sgrent"     -> Op_sgrent
            "op_sgt"        -> Op_sgt
            "op_shift"      -> Op_shift
            "op_shmctl"     -> Op_shmctl
            "op_shmget"     -> Op_shmget
            "op_shmread"    -> Op_shmread
            "op_shmwrite"   -> Op_shmwrite
            "op_sle"        -> Op_sle
            "op_sleep"      -> Op_sleep
            "op_slt"        -> Op_slt
            "op_sne"        -> Op_sne
            "op_sort"       -> Op_sort
            "op_splice"     -> Op_splice
            "op_split"      -> Op_split
            "op_sprintf"    -> Op_sprintf
            "op_spwent"     -> Op_spwent
            "op_sqrt"       -> Op_sqrt
            "op_srand"      -> Op_srand
            "op_stat"       -> Op_stat
            "op_stringify"  -> Op_stringify
            "op_stub"       -> Op_stub
            "op_study"      -> Op_study
            "op_subst"      -> Op_subst
            "op_substr"     -> Op_substr
            "op_subtract"   -> Op_subtract
            "op_symlink"    -> Op_symlink
            "op_syscall"    -> Op_syscall
            "op_sysopen"    -> Op_sysopen
            "op_sysread"    -> Op_sysread
            "op_sysseek"    -> Op_sysseek
            "op_system"     -> Op_system
            "op_syswrite"   -> Op_syswrite
            "op_tie"        -> Op_tie
            "op_tied"       -> Op_tied
            "op_time"       -> Op_time
            "op_tms"        -> Op_tms
            "op_trans"      -> Op_trans
            "op_truncate"   -> Op_truncate
            "op_uc"         -> Op_uc
            "op_ucfirst"    -> Op_ucfirst
            "op_umask"      -> Op_umask
            "op_undef"      -> Op_undef
            "op_unlink"     -> Op_unlink
            "op_unpack"     -> Op_unpack
            "op_unshift"    -> Op_unshift
            "op_unstack"    -> Op_unstack
            "op_untie"      -> Op_untie
            "op_utime"      -> Op_utime
            "op_values"     -> Op_values
            "op_vec"        -> Op_vec
            "op_wantarray"  -> Op_wantarray
            "op_warn"       -> Op_warn
            "op_xor"        -> Op_xor
            "package"       -> Package
            "parens"        -> Parens
            "peg"           -> Peg
            "preplus"       -> Preplus
            "quote"         -> Quote
            "statement"     -> Statement
            "sub"           -> Sub
            "ternary"       -> Ternary
            "use"           -> Use
            _               -> UnknownAbs
    return $AbstractNode con kids

noKids :: Int -> Parser P5AST
noKids indent = do
    try (string "- !perl/p5::") <?> "p5 decleration"
    name <- manyTill anyChar space
    manyTill anyToken newline
    spaces
    string "enc: "
    enc <- manyTill anyChar newline <?> "enc string"
    spaces
    string "uni: "
    --Uniblock deals with the various types of yaml blocks
    uni <- uniBlock (indent + 4) <?> "uni string/block"
    let con = case name of
            "closer"        -> Closer
            "closequote"    -> Closequote
            "declarator"    -> Declarator
            "junk"          -> Junk
            "label"         -> Label
            "opener"        -> Opener
            "openquote"     -> Openquote
            "operator"      -> Operator
            "punct"         -> Punct
            "remod"         -> Remod
            "sigil"         -> Sigil
            "text"          -> Text
            "token"         -> Token
            _               -> UnknownLit 
    return $ LiteralNode con enc uni

hereDoc :: Int -> Parser P5AST
hereDoc indent = do
    try (string "- !perl/P5AST::heredoc ") <?> "Heredoc decleration";
    newline
    spaces
    string "doc: !perl/P5AST::"
    doc <- manyTill anyChar space
    newline
    spaces
    string "Kids: "
    newline
    kids <- many (try (nodeNamer (indent+6)))
    spaces
    string "end: !perl/p5::closequote "
    newline
    spaces
    string "enc: "
    endenc <- manyTill anyChar newline
    spaces
    string "uni: "
    enduni <- uniBlock (indent + 6) 
    spaces
    string "start: !perl/p5::openquote "
    newline
    spaces
    string "enc: "
    startenc <- manyTill anyChar newline
    spaces
    string "uni: "
    startuni <- uniBlock (indent + 6)
    return (Heredoc (LiteralNode Openquote startenc startuni) (LiteralNode Closequote endenc enduni) kids)
    
    
    

    
{-
Uniblock handles the various types of yaml blocks used, those being a literal string (i.e. "..." or even just ...)
A block "|\n ..." or a block with a chomp modifier "|+\n ..."
-}
uniBlock :: Int -> Parser String
uniBlock indent = choice
    [do try $ string "|\n"
        uni <- manyTill (manyTill anyToken newline) (try(newline)) <?> "uni block";
        return (unlines (map (drop indent) uni))
    ,do try $ string "|+"
        newline;
        uni <- manyTill (manyTill anyToken newline) (try(choice [try(do{count (indent-4) space; notFollowedBy (char ' ')}), try(do{count (indent-8) space; notFollowedBy (char ' ')})])) <?> "uni block with chomp modifier"; -- A block with the chomp modifier ends when there's a line with too few spaces (which means another node (or the end part of a heredoc) is starting.
        return (unlines (map (drop indent) uni))
    ,do try $ string "|-"
        newline;
        uni <- manyTill (manyTill anyToken newline) (try(choice [try(do{count (indent-4) space; notFollowedBy (char ' ')}), try(do{count (indent-8) space; notFollowedBy (char ' ')})])) <?> "uni block with chomp modifier"; -- A block with the chomp modifier ends when there's a line with too few spaces (which means another node (or the end part of a heredoc) is starting.
        return (unlines (map (drop indent) uni))
    ,do try $ string "\""
        uni <- manyTill anyToken (try(string("\"\n")))
        --return uni
        return $ if ((length (lines uni))==1) then (makeLiterals uni) else (makeLiterals (joinString ((head (lines (getRidOfExtraSlashes uni))):(map (drop indent) (tail (lines (getRidOfExtraSlashes uni)))))))
    ,do uni <- manyTill anyToken newline <?> "uni string"
        --If the field is in quotes, strip the quotes by stripping the first character, 
        --reversing the string, stripping the first character, then reversing again
        return $ if (head uni `elem` "\"'")
            then (makeLiterals (reverse (tail (reverse (tail uni)))))
            else uni
    ]

--A wrapper for nodeNamer, to handle the junk at the beginning of the file.
parseInput :: Parser [P5AST]
parseInput = choice [do{try(manyTill anyToken (string "Kids: \n")); names <- many (nodeNamer 2); eof; return names}, return []]

getRidOfExtraSlashes :: String -> String
getRidOfExtraSlashes [] = []
getRidOfExtraSlashes inSt = if (and [((head inSt)=='\\'), ((head (tail inSt))=='\n')]) then ('\n':(getRidOfExtraSlashes (drop 2 inSt))) else ((head inSt):(getRidOfExtraSlashes (tail inSt)))

{-Function to handle escaped characters in a string scanned from input
For example, if the string "blah\n" is scanned, it ends up being represented as
"blah\\n". This function parses that newline into a literal newline.-}
makeLiterals :: String -> String
makeLiterals [] = []
makeLiterals inSt = if ((head inSt)=='\\') then if (head (tail inSt) == '"') then ('\"':(makeLiterals(drop 2 inSt))) else
                                                   if (head (tail inSt) == 'n') then ('\n':(makeLiterals(drop 2 inSt))) else
                                                     if (head (tail inSt) == 't') then ('\t':(makeLiterals(drop 2 inSt))) else
                                                       if(head (tail inSt) == '\n') then ('\n':(makeLiterals(drop 2 inSt))) else
                                                         if (head (tail inSt) == '\\') then ('\\':(makeLiterals(drop 2 inSt))) else 
                                                           ((makeLiterals(tail inSt)))
                      else ((head inSt):(makeLiterals (tail inSt)))

{- No longer a big big messy function to print all the different node types, 
now a slim function to print everything to a file.
Only two cases: LiteralNode and AbstractNode.
For a literal node, print the uni field.
For an abstract node, recursivley call printTree on the kids (if there are any).
All output is to a file
-}
printTree :: Handle -> P5AST -> String -> IO ()
{------------ Uncomment this section to help find Unknown Nodes
printTree outFile (LiteralNode UnknownLit _ uni) = do{ hPutStr outFile "UnknownLit";
                                                       hPutStr outFile uni}
printTree outFile (AbstractNode UnknownAbs kids) = do{ hPutStr outFile "UnknownAbs";
                                                       printTree outFile (head kids);
                                                       printTree outFile (AbstractNode P5AST (tail kids))}
-------------------------------------------------------------}
printTree outFile (LiteralNode atype _ uni) options = if (and [(atype==UnknownLit),('u' `elem` options)]) then do{ hPutStr outFile "UnknownLit"; putStrLn "UNKNOWN: UnknownLit"; hPutStr outFile uni} else (hPutStr outFile uni)

printTree outFile (AbstractNode atype []) options = if (and [(atype==UnknownAbs),('u' `elem` options)]) then do{ hPutStr outFile "UnknownAbs"; putStrLn "UNKNOWN: UnknownAbs"; hPutStr outFile ""} else (hPutStr outFile "")

printTree outFile (AbstractNode atype kids) options = if (and [(atype==UnknownAbs),('u' `elem` options)]) then do{ hPutStr outFile "UnknownAbs"; putStrLn "UNKNOWN: UnknownAbs"; printTree outFile (head kids) options; printTree outFile (AbstractNode P5AST (tail kids)) options} else do{ printTree outFile (head kids) options; printTree outFile (AbstractNode P5AST (tail kids)) options}

printTree outFile (Heredoc start end kids) options = do printTree outFile start options
                                                        hPutStr outFile ";\n"
                                                        printTree outFile (AbstractNode P5AST kids) options
                                                        printTree outFile end options

