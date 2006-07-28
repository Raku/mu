module ASTDefinition where
{------------------------
This module defines the AST as used by ASTParser and ASTTranslate (and ASTUtil also, for that matter).
It's really a very simple structure, as described bellow. It is designed to be used with ASTTranslate, 
but feel free to use it elsewhere if it's useful.

See ASTTranslate.hs for more.

Originally created by Sage LaTorra for Summer of Code 2006.
-------------------------}


{------------------
The P5AST structure represents the abstract syntax tree
of a perl 5 program. It is made up of two types of nodes:
AbstractNodes (!perl/P5AST in the yaml files) just have a nodetype and kids (0+)
LiteralNodes (!perl/p5 in the yaml files) have two strings, for the enc and uni fields.
-------------------}

data P5AST
    = AbstractNode AbsType [P5AST]
    | LiteralNode LitType String String
    | Heredoc P5AST P5AST  [P5AST] -- That is, Heredoc Start End Doc
    deriving (Show, Eq, Read)

{----------------
These are the types for AbstractNodes. This list may not be exhaustive,
but it covers enough basic cases to handle the tree of TestInit.pm
-----------------}
data AbsType
    = P5AST
    | Attrlist
    | Bindop
    | Cfor
    | Condmod
    | Condstate
    | Doblock
    | Format
    | Listelem
    | PNothing
    | Op_aassign
    | Op_abs
    | Op_add
    | Op_aelem
    | Op_alarm
    | Op_and
    | Op_andassign
    | Op_anonhash
    | Op_anonlist
    | Op_aslice
    | Op_atan2
    | Op_av2arylen
    | Op_backtick
    | Op_binmode
    | Op_bit_and
    | Op_bit_or
    | Op_bit_xor
    | Op_bless
    | Op_caller
    | Op_chdir
    | Op_chmod
    | Op_chomp
    | Op_chop
    | Op_chown
    | Op_chr
    | Op_chroot
    | Op_close
    | Op_closedir
    | Op_complement
    | Op_concat
    | Op_cond_expr
    | Op_const
    | Op_crypt
    | Op_defined
    | Op_delete
    | Op_die
    | Op_divide
    | Op_dofile
    | Op_dor
    | Op_dorassign
    | Op_each
    | Op_egrent
    | Op_entereval
    | Op_enteriter
    | Op_entersub
    | Op_entertry
    | Op_enterwrite
    | Op_eof
    | Op_epwent
    | Op_eq
    | Op_exec
    | Op_exists
    | Op_exp
    | Op_fcntl
    | Op_fileno
    | Op_flip
    | Op_flop
    | Op_formline
    | Op_ftatime
    | Op_ftbinary
    | Op_ftblk
    | Op_ftchr
    | Op_ftctime
    | Op_ftdir
    | Op_fteexec
    | Op_fteowned
    | Op_fteread
    | Op_ftewrite
    | Op_ftfile
    | Op_ftis
    | Op_ftlink
    | Op_ftmtime
    | Op_ftsize
    | Op_ftsock
    | Op_ftsuid
    | Op_fttext
    | Op_fttty
    | Op_ftzero
    | Op_goto
    | Op_gt
    | Op_gv
    | Op_helem
    | Op_hex
    | Op_hslice
    | Op_i_add
    | Op_i_divide
    | Op_i_eq
    | Op_i_lt
    | Op_i_modulo
    | Op_i_multiply
    | Op_i_ncmp
    | Op_i_ne
    | Op_i_negate
    | Op_i_postdec
    | Op_i_postinc
    | Op_i_predec
    | Op_i_preinc
    | Op_i_subtract
    | Op_index
    | Op_int
    | Op_ioctl
    | Op_iter
    | Op_join
    | Op_keys
    | Op_kill
    | Op_last
    | Op_lc
    | Op_lcfirst
    | Op_le
    | Op_leave
    | Op_leaveloop
    | Op_leavetry
    | Op_left_shift
    | Op_length
    | Op_lineseq
    | Op_link
    | Op_list
    | Op_localtime
    | Op_log
    | Op_lslice
    | Op_lstat
    | Op_lt
    | Op_mapstart
    | Op_mapwhile
    | Op_match
    | Op_method
    | Op_mkdir
    | Op_modulo
    | Op_msgctl
    | Op_msgget
    | Op_msgrcv
    | Op_msgsnd
    | Op_multiply
    | Op_ncmp
    | Op_ne
    | Op_negate
    | Op_next
    | Op_not
    | Op_null
    | Op_oct
    | Op_open
    | Op_open_dir
    | Op_or
    | Op_orassign
    | Op_ord
    | Op_pack
    | Op_padav
    | Op_padhv
    | Op_padsv
    | Op_pipe_op
    | Op_pop
    | Op_pos
    | Op_postdec
    | Op_postinc
    | Op_pow
    | Op_predec
    | Op_preinc
    | Op_print
    | Op_prototype
    | Op_prtf
    | Op_push
    | Op_pushmark
    | Op_qr
    | Op_quotemeta
    | Op_rand
    | Op_range
    | Op_read
    | Op_readdir
    | Op_readline
    | Op_readlink
    | Op_redo
    | Op_ref
    | Op_refgen
    | Op_regcomp
    | Op_regcreset
    | Op_rename
    | Op_repeat
    | Op_require
    | Op_reset
    | Op_return
    | Op_reverse
    | Op_rewinddir
    | Op_right_shift
    | Op_rindex
    | Op_rmdir
    | Op_rv2av
    | Op_rv2cv
    | Op_rv2gv
    | Op_rv2hv
    | Op_rv2sv
    | Op_sassign
    | Op_scalar
    | Op_schomp
    | Op_schop
    | Op_scmp
    | Op_scope
    | Op_seek
    | Op_select
    | Op_seq
    | Op_setpgrp
    | Op_setpriority
    | Op_sge
    | Op_sgrent
    | Op_sgt
    | Op_shift
    | Op_shmctl
    | Op_shmget
    | Op_shmread
    | Op_shmwrite
    | Op_sle
    | Op_sleep
    | Op_slt
    | Op_sne
    | Op_sort
    | Op_splice
    | Op_split
    | Op_sprintf
    | Op_spwent
    | Op_sqrt
    | Op_srand
    | Op_stat
    | Op_stringify
    | Op_stub
    | Op_study
    | Op_subst
    | Op_substr
    | Op_subtract
    | Op_symlink
    | Op_syscall
    | Op_sysopen
    | Op_sysread
    | Op_sysseek
    | Op_system
    | Op_syswrite
    | Op_tie
    | Op_tied
    | Op_time
    | Op_tms
    | Op_trans
    | Op_truncate
    | Op_uc
    | Op_ucfirst
    | Op_umask
    | Op_undef
    | Op_unlink
    | Op_unpack
    | Op_unshift
    | Op_unstack
    | Op_untie
    | Op_utime
    | Op_values
    | Op_vec
    | Op_wantarray
    | Op_warn
    | Op_xor
    | Package
    | Parens
    | Peg
    | Preplus
    | Quote
    | Statement
    | Sub
    | Ternary
    | Use
    | UnknownAbs
    deriving (Show, Eq, Read)

{----------------
These are the types for LiteralNodes. This list may not be exhaustive,
but it covers enough basic cases to handle the tree of TestInit.pm
-----------------}
data LitType
    = Closequote
    | Closer
    | Declarator
    | Junk
    | Label
    | Opener
    | Openquote
    | Operator
    | Punct
    | Remod
    | Sigil
    | Text
    | Token
    | UnknownLit
    deriving (Show, Eq, Read)