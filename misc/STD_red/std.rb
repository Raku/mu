#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
# A ruby transliteration of src/perl6/STD.pm
# See README.
#
# STD_red issues
#  Some Match ruls are "foo__bar", where bar was a #= comment.  This is wrongish.
#

require 'prelude'

class Perl < Grammar
    attr_accessor :ws_from, :ws_to

    def _TOP; _UNIT( $env_vars[:unitstopper] || "_EOS" ); end

    def_precedence :hyper           ,{ :transparent=>1                         }
    def_precedence :term            ,{ :prec=>"z="                             }
    def_precedence :methodcall      ,{ :prec=>"y="                             }
    def_precedence :autoincrement   ,{ :prec=>"x="                             }
    def_precedence :exponentiation  ,{ :prec=>"w=", :assoc=>:right, :assign=>1 }
    def_precedence :symbolic_unary  ,{ :prec=>"v="                             }
    def_precedence :multiplicative  ,{ :prec=>"u=", :assoc=>:left,  :assign=>1 }
    def_precedence :additive        ,{ :prec=>"t=", :assoc=>:left,  :assign=>1 }
    def_precedence :replication     ,{ :prec=>"s=", :assoc=>:left,  :assign=>1 }
    def_precedence :concatenation   ,{ :prec=>"r=", :assoc=>:left,  :assign=>1 }
    def_precedence :junctive_and    ,{ :prec=>"q=", :assoc=>:list,  :assign=>1 }
    def_precedence :junctive_or     ,{ :prec=>"p=", :assoc=>:list,  :assign=>1 }
    def_precedence :named_unary     ,{ :prec=>"o=",                            }
    def_precedence :nonchaining     ,{ :prec=>"n=", :assoc=>:non               }
    def_precedence :chaining        ,{ :prec=>"m=", :assoc=>:chain, :bool=>1   }
    def_precedence :tight_and       ,{ :prec=>"l=", :assoc=>:left,  :assign=>1 }
    def_precedence :tight_or        ,{ :prec=>"k=", :assoc=>:left,  :assign=>1 }
    def_precedence :conditional     ,{ :prec=>"j=", :assoc=>:right,            }
    def_precedence :item_assignment ,{ :prec=>"i=", :assoc=>:right             }
    def_precedence :loose_unary     ,{ :prec=>"h=",                            }
    def_precedence :comma           ,{ :prec=>"g=", :assoc=>:list,             }
    def_precedence :list_infix      ,{ :prec=>"f=", :assoc=>:list,  :assign=>1 }
    def_precedence :list_assignment ,{ :prec=>"i=", :sub=>"e=", :assoc=>:right }
    def_precedence :list_prefix     ,{ :prec=>"e=",                            }
    def_precedence :loose_and       ,{ :prec=>"d=", :assoc=>:left,  :assign=>1 }
    def_precedence :loose_or        ,{ :prec=>"c=", :assoc=>:left,  :assign=>1 }
    def_precedence :LOOSEST         ,{ :prec=>"a=!",                           }
    def_precedence :terminator      ,{ :prec=>"a=", :assoc=>:list              }
    SLOOSEST = HLOOSEST[:prec]

    #R role PrecOp
    def precop_method(m,defaults)
        if not defaults[:transparent]
            mO = m[:O]
            mO = m[:O] = {} if not mO
            defaults.each{|k,v| mO[k] = v if not mO.key? k }
        end
        return m;
    end
    #R XXX I'm unsure what make() (in expect_term, and elsewhwere) should be doing.
    def make(m,overwrite)
        overwrite.each{|k,v| m[k] = v }
    end

    #R things like "class Term does PrecOp[|%term] {}" are folded into
    #R def_precedence above.

    $env_vars.scope_enter(:endsym,:unitstopper,:endstmt,:endargs)
    $env_vars[:endsym] = "null"
    $env_vars[:unitstopper] = "_EOS"
    $env_vars[:endstmt] = -1
    $env_vars[:endargs] = -1
    #R helper
    def scan_unitstopper
        us = $env_vars[:unitstopper];
        ((if us == '_EOS'; @scanner.eos?
          else; raise "assert: known unitstopper: #{us}"
          end) or
         permit_partial_parse or
         panic("Can't understand next input--giving up"))
    end

    #R XXX TODO - the non-simple constraints are not being applied.
    token_category :category
    token_category :sigil
    token_category :twigil
    token_category :special_variable
    token_category :version
    token_category :module_name
    token_category :term
    token_category :quote, 'nofat'
    token_category :prefix
    token_category :infix
    token_category :postfix
    # def_precedence_alias is what "is defequiv" means.
    def_precedence_alias :prefix,  :symbolic_unary
    def_precedence_alias :infix,   :additive
    def_precedence_alias :postfix, :autoincrement
    token_category :dotty,  'unspacey'
    token_category :circumfix
    token_category :postcircumfix
    token_category :regex_metachar
    token_category :regex_backslash
    token_category :regex_assertion
    token_category :regex_quantifier
    token_category :regex_mod_internal
    token_category :quote_mod
    token_category :q_backslash
    token_category :qq_backslash
    token_category :trait_verb,        'nofat_space'
    token_category :trait_auxiliary,   'nofat_space'
    token_category :type_declarator,      'nofat'
    token_category :scope_declarator,     'nofat'
    token_category :package_declarator,   'nofat'
    token_category :plurality_declarator, 'nofat'
    token_category :routine_declarator,   'nofat'
    token_category :regex_declarator,     'nofat'
    token_category :statement_prefix,     'nofat'
    token_category :statement_control, 'nofat_space'
    token_category :statement_mod_cond,   'nofat'
    token_category :statement_mod_loop,   'nofat'
    token_category :infix_prefix_meta_operator
    token_category :infix_postfix_meta_operator
    token_category :infix_circumfixfix_meta_operator
    token_category :postfix_prefix_meta_operator
    token_category :prefix_postfix_meta_operator
    token_category :prefix_circumfix_meta_operator
    #R categories added:
    token_category :terminator
    token_category :infix_circumfix_meta_operator

    def unspacey; unsp;true end
    def nofat_space
        before(/\s|\#/) and
        nofat #R# before{nofat} # nofat is already a zero-length assertion.
    end

    # Lexical routines

    def nofat
        # make sure we're at end of a non-autoquoted identifier
        # regex nofat { <!before » \h* <.unsp>? '=>' > <!before \w> }
        (not before{ scan(/»/) and scan(/[ \t]*/) and (unsp;true) and scan(/=>/) } and
         not before(/\w/))
    end


    if RUBY_VERSION =~ /\A(1\.9|2\.)/ # have look-behind
        $have_lookbehind = true
        eval %q{ def wsp__after_and_before_ws; scan(/(?<=\w)(?=\w)/) end }
    else
        $have_lookbehind = false
        eval %q{ def wsp__after_and_before_ws; scan(/(?=\w)/) and after(/\w/)  end }
    end

    #R ws, renamed wsp to make life easier.
    def wsp
        pos == ws_to and return true
        #R# || <?after \w> <?before \w> ::: <!>        # must \s+ between words
        #R# after(/\w/) and before(/\w/) and return false
        wsp__after_and_before_ws and return false
        @ws_from = pos
        starTOK{
            unsp or
            let_pos{ vws and heredoc } or
            unv
        }
        @ws_to = pos
        true
    end
    def unsp
        scan(/\\(?=\s|\#)/) and starTOK{ vws or unv }
    end
    def unsp?; (unsp;true); end
    def vws
        scan(/[\r\n\f]/) and (moreinput;true)
    end
    def moreinput
        send($env_vars[:moreinput]) if $env_vars.defined?(:moreinput);
    end

    def unv
        let_pos{
            scan(/[ \t]+/) or
            (@scanner.bol? and pod_comment) or
            let_pos{
               scan(/\#/) and bracketed and
               (not(@scanner.bol?) or panic("Can't use embedded comments in column 1"))
            } or
            (scan(/\#.*/u))
        }
    end

    def ident; scan(/[[:alpha:]_]\w*/); end
    
    def pod_comment
        @scanner.bol? and scan(/=/) and unsp? and
        (let_pos{
             scan(/begin[ \t]*/) and
             #wsp and #R XXX causes problems with "=begin\n=head...".
             (id= ident;true) and
             scan(/(?:.|\n)*?\n=end\b/u) and #R XXX doesn't accept unsp between '=' and 'end'.
             (not(id) or (wsp and scan(/#{id}.*/u)))
         } or
         scan(/.*/u))
    end

    # Top-level rules
    
    def _UNIT (_unitstopper =nil)
        $env_vars.scope_enter(:unitstopper)
        $env_vars[:unitstopper] = _unitstopper || "_EOS"
        # UNIT: do {
        v = comp_unit()
        $env_vars.scope_leave
        v
    end

    def comp_unit
        $env_vars.scope_enter(:begin_compunit,:endstmt,:endargs)
        $env_vars[:begin_compunit] = 1
        $env_vars[:endstmt] = -1
        $env_vars[:endargs] = -1
        b = pos
        wsp
        _sl = statementlist
        wsp
        scan_unitstopper #R panic() is in scan_unitstopper
        wsp
        $env_vars.scope_leave
        _match_from(b,{:statementlist=>_sl},:comp_unit)
    end

    def pblock
        b = pos
        l=s=nil
        quesTOK{ l= _lambda and s= signature }; bl= block or return false
        h={:block=>bl};_hkv(h,:lambda,l);_hkv(h,:signature,s)
        _match_from(b,h,:pblock)
    end

    def _lambda; scan(/->|<->/); end

    def block
        b=pos
        let_pos{ scan(/\{/) and sl= statementlist and _block_rest and
                 _match_from(b,{:statementlist=>sl},:block) }
    end

    #R QUESTION regexp_block lacks block's \h*.  Intentional?
    def regex_block
        let_pos{
            b = pos
            scan(/\{/) and r= regex('}') and _block_rest and
            _match_from(b,{:regex=>r},:regex_block) }
    end

    def _block_rest
        ( scan(/\}/) or panic("Missing right brace") ) and
          #R QUESTION <?before < ,: >> typo?
          ( let_pos{ scan(/[ \t]*/) and unsp? and before(/[,:]/) } or
            let_pos{
                unv; before(/\n/) and wsp and
                #R XXX BAD should be let
                ($env_vars[:endstmt] = ws_from;true)
            } or
            #R XXX BAD should be let
            ($env_vars[:endargs] = pos) )
    end


    def statementlist
        starRULE{ s= statement and eat_terminator and s }
    end

    def semilist
        starRULE{ s= statement and eat_terminator and s }
    end

    def label
        let_pos{ id = ident and scan(/:(?=\s)/) and wsp }
        #R ...missing... bookkeeping - needed?
    end


    def _hkv(h,k,v)
        h[k] = v if v and (v.instance_of?(Array) ? (not v.empty?) : true)
        h
    end

    def statement
        $env_vars.scope_enter(:endstmt)
        $env_vars[:endstmt] = -1;
        (label_ = control_ = expr_ = mod_loop_ = mod_cond_ =
         loopx_ = condx_ = mod_condloop_ = modexpr_ = nil)
        let_pos{
            b = pos
            wsp
            label_= starTOK{ label }
            (begin
                 let_pos{
                     (( control_= statement_control or
                        before(/;/) )
                      )
                 }
             end or
             begin
                 let_pos{
                     b1 = nil
                     endstmt = $env_vars[:endstmt]; endargs = $env_vars[:endargs]
                     expr_= _EXPR and
                     (b1 = pos;true) and
                     (let_pos{
                          before{ stdstopper_no_mod } #R XXX NONSPEC KLUDGE _no_mod - STD.pm bug? workaround.
                      } or
                      let_pos{
                          $env_vars[:endstmt] = endstmt; $env_vars[:endargs] = endargs
                          mod_loop_= statement_mod_loop #R XXX NONSPEC bug? workaround # and loopx= _EXPR
                      } or
                      let_pos{
                          $env_vars[:endstmt] = endstmt; $env_vars[:endargs] = endargs
                          endstmt1 = $env_vars[:endstmt]; endargs1 = $env_vars[:endargs]
                          mod_cond_= statement_mod_cond and #R XXX NONSPEC bug? workaround # condx= _EXPR and
                          (let_pos{
                               before{ stdstopper_no_mod } #R XXX NONSPEC KLUDGE _no_mod - STD.pm bug? workaround.
                           } or
                           let_pos{
                               $env_vars[:endstmt] = endstmt1; $env_vars[:endargs] = endargs1
                               mod_condloop_= statement_mod_loop #R XXX NONSPEC bug? workaround # and loopx= _EXPR
                           } or
                           false)
                      } or
                      false)
                     #R NONSPEC  modexpr missing
                     #R# (modexpr_= b1 != pos ? _match_from(b1,{},:statement__modexpr) : nil; true)
                 }
             end) and
            begin
                h = {}
                _hkv(h,:label,label_)
                _hkv(h,:control,control_)
                _hkv(h,:expr,expr_)
                _hkv(h,:mod_loop,mod_loop_)
                _hkv(h,:mod_cond,mod_cond_)
                _hkv(h,:loopx,loopx_)
                _hkv(h,:condx,condx_)
                _hkv(h,:mod_condloop,mod_condloop_)
                _hkv(h,:modexprp,modexpr_)
                m = _match_from(b,h,:statement)
            end
        } || ($env_vars.scope_leave; false) #R XXX fails to reset endargs
    end

    def eat_terminator
        ( scan(/;/) or
          ($env_vars[:endstmt] == ws_from) or 
          before{ terminator } or
          @scanner.eos? or @scanner.check(/\n/) or
          panic("Statement not terminated properly"))
    end

    def_tokens_rest :statement_control,false,%w{ use no },%q{
      e=nil
      nofat_space and wsp and
      mn = module_name and wsp and (e=_EXPR and wsp;true) and
      (h={:module_name=>mn};_hkv(h,:EXPR,e);_match_from(start,h,:<sym>))
    }
    def_tokens_rest :statement_control,false,%w{ if }, %q{
      nofat_space and wsp and
      e=_EXPR and wsp and pb=pblock and wsp and
      ei=starRULE{ b1=pos; scan(/elsif/) and nofat_space and wsp and
                   e1=_EXPR and wsp and pb1=pblock and wsp and
                   _match_from(b1,{:elsif_expr=>e1,:elsif_block=>pb1},:elsif) } and
      el=quesRULE{ b1=pos; scan(/else/) and nofat_space and wsp and
                   pb1=pblock and wsp and
                   _match_from(b1,{:pblock=>pb1},:if__else) } and
      (h={:if_expr=>e,:if_block=>pb,:elsif=>ei};_hkv(h,:else,el);
       _match_from(start,h,:if))
    }

    def_tokens_rest :statement_control,false,%w{ unless while until  for given when },%q{
      nofat_space and wsp and
      e=_EXPR and wsp and pb=pblock and wsp and
      _match_from(start,{:expr=>e,:block=>pb},:<sym>)
    }
    def_tokens_rest :statement_control,false,%w{ repeat },%q{
      nofat_space and wsp and
      ((wu= scan(/while|until/) and wsp and e=_EXPR and wsp and bk=block and wsp and
        _match_from(start,{'0'=>wu,:wu_expr=>e,:wu_block=>bk},:repeat)) or
       (bk=block and wsp and wu=scan(/while|until/) and wsp and e=_EXPR and wsp and
        _match_from(start,{'0'=>wu,:expr_wu=>e,:block_wu=>bk},:repeat)))
    }
    def_tokens_rest :statement_control,false,%w{ loop },%q{
      nofat_space and wsp and
      ((scan(/\(/) and wsp and
        e1= _EXPR and wsp and scan(/;/) and wsp and
        e2= _EXPR and wsp and scan(/;/) and wsp and
        e3= _EXPR and wsp and scan(/\)/) and wsp and
        (h={};_hkv(h,:loop_e1,e1);_hkv(h,:loop_e2,e2);_hkv(h,:loop_e3,e3);
         eee= _match_from(start,h,:loop__eee)
       );true) and
       bk= block and wsp and
       (h={:loop_block=>bk};_hkv(h,:loop_eee,eee);
        _match_from(start,h,:loop)))
    }

    def_tokens_rest :statement_control,false,%w{
      default BEGIN CHECK INIT END START ENTER LEAVE KEEP UNDO FIRST NEXT LAST
      PRE POST CATCH CONTROL },
      %q{
        nofat_space and wsp and
        bk=block and wsp and
        _match_from(start,{:block=>bk},:<sym>)
      }

    def_tokens_rest :term,false,%w{
      BEGIN CHECK INIT START ENTER FIRST },
      %q{
        wsp and
        bk=block and wsp and
        _match_from(start,{:block=>bk},:<sym>)
      }

    def modifier_expr; wsp and e=_EXPR and wsp and e; end
    def_tokens_rest :statement_mod_cond,false,%w{ if unless when },%q{
      nofat and wsp and
      me=modifier_expr and wsp and
      _match_from(start,{:mod_<sym>=>me},:<sym>)
    }
    def_tokens_rest :statement_mod_loop,false,%w{ while until for given },%q{
      nofat and wsp and
      me=modifier_expr and wsp and
      _match_from(start,{:mod_<sym>=>me},:<sym>)
    }
    
    #R reordered.  depreciated needs to come first.
    def_token_full :module_name,false,'depreciated',/(?=v6-alpha)/,%q{
      scan(/v6-alpha/) and
       _match_from(start,{},:depreciated)
    }
    def_token_full :module_name,false,'normal',/(?=\w)/,%q{
      (n=name and na= starTOK{ colonpair }) and
      (h={:name=>n};_hkv(h,:colonpair,na)
       _match_from(start,h,:normal))
    }

    def whatever; scan(/\*/); end

    def_tokens_rest :version,false,%w{ v },%q{ scan(/ \d+ ( \. (\d+ | \*) )* \+?/x) }

    ###################################################

    def pre
        precircum_ = prepost_ = nil
        b = pos
        let_pos{
            ((prefix_= prefix) or 
             (precircum_= prefix_circumfix_meta_operator)
             ) and
            (prepost_= starTOK{ prefix_postfix_meta_operator }) and
            wsp and
            (o = (prefix_||precircum_)[:O];
             h={:O=>o};
             _hkv(h,:prefix,prefix_);
             _hkv(h,:precircum,precircum_);
             _hkv(h,:prepost,prepost_);
             _match_from(b,h,:pre))
        }
    end

    def expect_term
        let_pos{
            b = pos
            pre_=adv=nil
            return false if before{ stdstopper } 
            (noun_= noun or
             (pre_= plusTOK{ pre } and noun_= noun)) and
            # also queue up any postfixes, since adverbs could change things
            post_= starTOK{ post } and
            wsp and
            (adv_= adverbs;true) and
            (h={};
             _hkv(h,:noun,noun_)
             _hkv(h,:pre,pre_)
             _hkv(h,:post,post_)
             _hkv(h,:adverbs,adv_)
             _match_from(b,h,:expect_term))
        }
    end

    def adverbs
        b = pos
        colonpair_=nil
        not before{ stdstopper } and
        (plusTOK{ _cp = colonpair and (colonpair_ ||= []; colonpair_.push(_cp);true) and wsp } and
         ( prop = $env_vars[:prevop] or
           panic('No previous operator visible to adverbial pair ('+colonpair_.inspect+')');
           prop.adverb(colonpair_); true ) and
         (h={:colonpair=>colonpair_};_match_from(b,h,:adverbs)))
    end

    def noun
        (fatarrow || package_declarator || scope_declarator || plurality_declarator ||
         routine_declarator || regex_declarator || type_declarator || circumfix ||
         dotty || subcall || variable || value || capterm || sigterm || 
         statement_prefix || term ||  #R NONSPEC out of order
         colonpair)
    end
    
    def fatarrow
        let_pos{
            b = pos
            (key= ident and scan(/[ \t]*/) and
             scan(/\=>/) and wsp and
             val = _EXPR(Hitem_assignment)) and
            _match_from(b,{:key=>key,:val=>val},:fatarrow)
        }
    end

    def colonpair
        b = pos; id1=id2=pc1=pc2=si=tw=dsn=nil
        let_pos{
        (scan(/:/) and b1 = pos and
         (let_pos{ scan(/!/) and id1= ident } or
          ( id2= ident and (unsp; pc1= postcircumfix;true)) or
          pc2= postcircumfix or
          let_pos{ si= sigil and (tw= twigil;true) and dsn= desigilname }) and
         (false_ = id1 ? _match_from(b1,{:ident=>id1},:colonpair__false) : nil;
          value_ = id2 ? _match_from(b1,{:ident=>id2,:postcircumfix=>pc1},:colonpair__value) : nil;
          structural_ = pc2;
          varname_ = si ? _match_from(b1,{:sigil=>si,:twigil=>tw,:desigilname=>dsn},:colonpair__varname) : nil;
          h={};
          _hkv(h,:false,false_)
          _hkv(h,:value,value_)
          _hkv(h,:structural,structural_)
          _hkv(h,:varname,varname_)
          _match_from(b,h,:colonpair)))
        }
    end

    def quotepair
        b = pos; id1=id2=pc1=n=nil
        let_pos{
            scan(/:/) and b1 = pos and
            (let_pos{ scan(/!/) and id1= ident } or
             (id2= ident and (unsp; before(/\(/) and pc1= postcircumfix;true)) or
             #R NONSPEC spec doesn't have n and suffix named, so this is speculative.
             (n= scan(/\d+/) and suf= scan(/[a-z]+/))) and
            (false_ = id1 ? _match_from(b1,{:ident=>id1},:quotepair__false) : nil;
             value_ = id2 ? _match_from(b1,{:ident=>id2,:postcircumfix=>pc1},:quotepair__value) : nil;
             nth_ = suf ? _match_from(b1,{:n=>n,:suffix=>suf},:quotepair__nth) : nil;
             h={};
             _hkv(h,:false,false_)
             _hkv(h,:value,value_)
             _hkv(h,:nth,nth_)
             _match_from(b,h,:quotepair))
        }
    end

    def expect_tight_infix(loosest)
        let_pos {
            ((not before{ scan(/\{/) or _lambda }) and # presumably a statement control block
             ei= expect_infix and
             #R# { $<O> := $<expect_infix><O> }  #R not required as ei is passed on directly.
             (ei[:O][:prec] > loosest) and
             ei)
        }
    end

    def expect_infix
        b = pos; i=ipost=ipre=icirc=nil; op=nil
        not(before{infixstopper}) and
        ((let_pos{
              i= infix and
              ($env_vars.scope_enter(:opS)   # (used in infix_postfix_meta_operator)
               $env_vars[:opS] = i
               ipost= starTOK{ infix_postfix_meta_operator }) and # may modify $+op
              (op = $env_vars[:opS])
          } or
          (ipre= infix_prefix_meta_operator) or
          (icirc= infix_circumfix_meta_operator)) and
         (h={};
          _hkv(h,:infix,i)
          _hkv(h,:infix_postfix_meta_operator,ipost)
          _hkv(h,:infix_prefix_meta_operator,ipre)
          _hkv(h,:infix_circumfix_meta_operator,icirc)
          h[:O] = op ? op[:O] : (ipre || icirc)[:O]
          h[:sym] = (i || ipre || icirc)[:sym]
          _match_from(b,h,:expect_infix)))
    end

    def_tokens_rest :dotty,:methodcall,%w{ .+ .* .? .= .^ .: .:: },%q{ #R NONSPEC ADDED .::
      unspacey and
      op= methodop and
      _match_from(start,{:sym=>'<sym>',:methodop=>op,:O=>op[:O]},:'<sym>') #R NONSPEC name(s)?
    }
    def_tokens_rest :dotty,:methodcall,%w{ . },%q{
      unspacey and
      op= dottyop
      #R XXX requires an elf update
      #R# _match_from(start,{:sym=>'<sym>',:dottyop=>op,:O=>op[:O]},:'<sym>')
    }
    def_tokens_rest :dotty,:methodcall,%w{ ! },%q{
      unspacey and
      op= methodop and
      _match_from(start,{:sym=>'<sym>',:methodop=>op,:O=>op[:O]},:'<sym>')
    }
    def dottyop #R [:O] copying unnecessary as no Match is created.
        methodop or postop
    end

    def post
        let_pos{
            b=pos
            d=postop_=nil;ppmo=[]
            not(before{stdstopper}) and
            # last whitespace didn't end here (or was zero width)
            (pos != ws_to or ws_to == ws_from) and
            before{unspacey} and 
            starTOK{
                let_pos{
                    quesTOK{scan(/\./) and unsp?} and
                    op= postfix_prefix_meta_operator and (ppmo.push op;true) and
                    unsp?
                }
            } and
            (d= dotty or postop_= postop) and
            (h={};
             h[:O] = (d || postop_)[:O]
             _hkv(h,:postfix_prefix_meta_operator,ppmo)
             _hkv(h,:dotty,d)
             _hkv(h,:postop,postop_)
             _match_from(b,h,:post))
        }
    end

    #R XXX TODO I currently don't understand the [LIST] issue.  And so likely dont support it.

    # Note: backtracks, or we'd never get to parse [LIST] on seeing [+ and such.
    # (Also backtracks if on \op when no \op infix exists.)
    def_token_full :prefix_circumfix_meta_operator,false,'reduce',/\[/,%q{
       let_pos{
           b = pos
           scan(/\[(?=\S*\])/) or return false
           ei=nil
           (let_pos{ ei= expect_infix and scan(/\]/) } or
            let_pos{ scan(/\\\\/) and ei= expect_infix and scan(/\]/) }) and
           (o = ei[:O]
            (not(o[:assoc] == :non) or
             panic("Can't reduce a non-associative operator"))
            (not(o[:prec] == Hconditional[:prec]) or
             panic("Can't reduce a conditional operator"))
            _match_from(b,{:expect_infix=>ei,:O=>o},:reduce))
       }
    }

    def_tokens_simple :prefix_postfix_meta_operator,false,%w{ « }
    def_tokens_simple :prefix_postfix_meta_operator,false,%w{ << }
    def_tokens_simple :postfix_prefix_meta_operator,false,%w{ » }
    def_tokens_simple :postfix_prefix_meta_operator,false,%w{ >> }

    def_tokens_rest :infix_prefix_meta_operator,:chaining,%w{ ! },%q{
      i=nil
      lex1(:negation) and
      let_pos{ (not before(/!/)) and i= infix } and
      (o = i[:O]
       (o[:assoc] == :chain or
        o[:assoc] and o[:bool] or
        panic("Only boolean infix operators may be negated"))
       (o[:hyper] and
        panic("Negation of hyper operator not allowed"))
       _match_from(start,{:infix=>i,:O=>o},:<sym>))
    }

    def lex1(s)
        false and #R XXX UNIMPLEMENTED - STD.pm being refactored
        panic("Nested #{s} metaoperators not allowed")
        true
    end

    def_token_full :infix_circumfix_meta_operator,:list_infix,'X X',/X/,%q{
      lex1(:cross) and let_pos{ i= infix and scan(/X/) } and
      _match_from(start,{:infix=>i,:O=>i[:O]},:'<sym>')
    }

    def_token_full :infix_circumfix_meta_operator,:hyper,'« »',/(?=«|»|<<|>>)/,%q{
      i=nil
      lex1(:hyper) and
      (let_pos{ scan(/«|»/) and i= infix and scan(/«|»/) } or
       let_pos{ scan(/<<|>>/) and i= infix and scan(/<<|>>/) }) and
      _match_from(start,{:infix=>i,:O=>i[:O]},:'<sym>')
    }

    def_tokens_rest :infix_postfix_meta_operator,:item_assignment,%w{ = },%q{
       lex1(:assignment) and
       (op = $env_vars[:opS]
        o = op[:O]
        (o[:prec] > Hitem_assignment[:prec] or
         panic("Can't make assignment op of operator looser than assignment"))
        (not(o[:assoc] == :chain) or
         panic("Can't make assignment op of boolean operator"))
        (not(o[:assoc] == :non) or
         panic("Can't make assignment op of non-associative operator"))
        _match_from(start,{:O=>o},:'<sym>'))
    }
    
    def_tokens_rest :postcircumfix,:methodcall,%w{ ( },%q{ sl=semilist and scan(/\)/) and sl }
    def_tokens_rest :postcircumfix,:methodcall,%w{ [ },%q{ sl=semilist and scan(/\]/) and sl }
    def_tokens_rest :postcircumfix,:methodcall,%w{ \{ },%q[ sl=semilist and scan(/\}/) and sl ]
    def_tokens_rest :postcircumfix,:methodcall,%w{ < },%q{ w=anglewords('>') and scan(/>/) and w }
    def_tokens_rest :postcircumfix,:methodcall,%w{ << },%q{ w=shellwords('>>') and scan(/>>/) and w }
    def_tokens_rest :postcircumfix,:methodcall,%w{ « },%q{ w=shellwords('»') and scan(/»/) and w }
    
    def postop
        #R We pass though, and so don't have to set [:O].
        (( postfix ) or
         ( postcircumfix ))
    end

    def methodop
        b = pos; id=v=q=sl=al=nil
        ((id= ident or
          (before(/\$|\@/) and v= variable) or
          (before(/[\'\"]/) and q= quote and (q =~ /\W/ or panic("Useless use of quotes")))) and
         unsp? and
         (let_pos{ scan(/\./); unsp; scan(/\(/) and sl= semilist and scan(/\)/) } or
          let_pos{ scan(/\:/) and before(/\s/) and (not $env_vars[:inqoute]) and al= arglist } or
          null) and
         (h={};
          _hkv(h,:ident,id)
          _hkv(h,:variable,v)
          _hkv(h,:quote,q)
          _hkv(h,:semilist,sl)
          _hkv(h,:arglist,al)
          _match_from(b,h,:methodop)))
    end

    def arglist
        $env_vars.scope_enter(:endargs)
        $env_vars[:endargs] = false #R ??? XXX "0" or "false"?
        wsp and
        v = _EXPR(Hlist_prefix)
        $env_vars.scope_leave
        v
    end

    def anglewords(stop)
        #wsp and starTOK{ (not before(/#{stop}/u)) and scan(/./u) } # XXX need to split
        wsp and scan(/(?:(?!#{stop}).)*/u) #R# Modified, to get str, not array of char.
    end

    def shellwords(stop)
        wsp and starTOK{ (not before(/#{stop}/)) and scan(/./u) } # XXX need to split
    end

    #R inlined lambda in re
    def_token_full :circumfix,:term,'{ }',/(?=\{|->|<->)/,%q{ pblock }

    def variable_decl
        b = pos
        e1=e2=nil
        (var = variable and # ( xXXX[:sigil] = var[:sigil] ) and
         quesTOK{
             %w{ @ % }.member?(var[:sigil]) and
             wsp and
             before(/[\<\(\[\{]/) and
             postcircumfix
         } and
         t= starTOK{trait} and
         wsp and
         #R XXX only first value is captured.
         #R QUESTION spec rx would seem to not interleave the = and .= ?
         quesTOK{
             ((scan(/\=/) and wsp and e1=_EXPR(var[:sigil] == '$' ? Hitem_assignment : Hlist_prefix)) or
              (scan(/\.\=/) and wsp and e2=_EXPR(Hitem_assignment)))
         }) and
         (h={};
          _hkv(h,:variable,var)
          _hkv(h,:default_value,e1) #R XXX speculative name
          _hkv(h,:default_call,e2) #R XXX speculative name
          _hkv(h,:traits,t)
          _match_from(b,h,:variable_decl))
    end

    def scoped #rule
        regex_declarator or package_declarator or
            #R XXX not backtracking properly - don't know if it needs to yet
            b=pos
            ft=v=sig=ts=nil
            (let_pos{
                 ft= starRULE{fulltypename} and wsp and
                 ( v= variable_decl or
                   let_pos{ scan(/\(/) and wsp and signature and wsp and scan(/\)/) and wsp and starRULE{trait} } or
                   plurality_declarator or
                   routine_declarator or
                   type_declarator)
             }) and
            (h={};
             _hkv(h,:fulltypename,ft)
             _hkv(h,:variable_decl,v)
             _match_from(b,h,:scoped))
    end
    def_tokens_rest :scope_declarator,false,%w{ my our state constant has },%q{b=pos; s=scoped and _match_from(b,{:scoped=>s},:<sym>) }
    def_tokens_rest :package_declarator,false,%w{ class grammar module role package },%q{b=pos; pd=package_def and _match_from(b,{:package_def=>pd},:<sym>) } #end;end
    def_tokens_rest :package_declarator,false,%w{ require },%q{ module_name and (_EXPR;true) }
    def_tokens_rest :package_declarator,false,%w{ trusts },%q{ module_name }

    #R added a .ws between module_name and block, and before module_name. XXX
    def package_def
        let_pos{
            b = pos
            (wsp and
             (mn = quesRULE{ module_name } and wsp and
              traits_= starRULE{ trait } and wsp and
              (let_pos{ $env_vars[:begin_compunit] and scan(/;/) and wsp and
                (mn.bool or panic("Compilation unit cannot be anonymous")) and
                ($env_vars[:begin_compunit] = false
                 true)} or
               (bk= block and wsp)))) and
            (h={};
             _hkv(h,:module_name,mn)
             _hkv(h,:traits,traits_)
             _hkv(h,:block,bk)
             _match_from(b,h,:package_def))
        }
    end

    def pluralized
        let_pos {
            wsp and
            (variable_decl or
             let_pos{
                 scan(/\(/) and wsp and signature and wsp and
                 scan(/\)/) and wsp and
                 ruleSTAR{trait} } or
             package_declarator or
             routine_declarator or
             regex_declarator or
             type_declarator) }
    end

    def_tokens_rest :plurality_declarator,false,%w{ multi proto only },%q{
       if not before{ wsp and scan(/sub/) } #R XXX NONSPEC ADDED
         r= routine_def and _match_from(start,{:routine_def=>r},:<sym>)
       else
         p= pluralized and _match_from(start,{:pluralized=>p},:<sym>)
       end
    }
    def_tokens_rest :routine_declarator,false,%w{ sub },%q{ routine_def }
    def_tokens_rest :routine_declarator,false,%w{ method submethod },%q{ method_def }
    def_tokens_rest :routine_declarator,false,%w{ macro },%q{ macro_def }
    def_tokens_rest :regex_declarator,false,%w{ regex token rule },%q{ regex_def }


    # Most of these special variable rules are there simply to catch old p5 brainos
    #R so most are ignored here.
    #R Ignore most of them for now.

    def_tokens_simple :special_variable,false,%w{ $/ $! $_ }


    # desigilname should only follow a sigil/twigil

    def desigilname
        ((before(/\$/) and variable) or
         (name))
    end
    
    def variable
        b = pos
        (special_variable or
         let_pos{
             sln=dsl=pc=nil
             (si = sigil and (tw = twigil;true) and
              ((si[:sym] == '&' and (sln = sublongname or return false;sln)) or
               dsl= desigilname) and
              ((tw and tw[:sym] == '.' and unsp? and before(/\(/) and pc= postcircumfix) or
               null)) and
             (h={:sigil=>si,:twigil=>tw};
              _hkv(h,:sublongname,sln)
              _hkv(h,:desigilname,dsl)
              _hkv(h,:postcircumfix,pc)
             _match_from(b,h,:variable))
         } or
         let_pos{ si= sigil and d= scan(/\d+/) and
             (h={:sigil=>si};
              _hkv(h,:desigilname,d) #R XXX non-spec
             _match_from(b,h,:variable))
         } or
         # Note: $() can also parse as contextualizer in an expression; should have same effect
         let_pos{ sigil and before(/[<\(]/) and postcircumfix } or
         # Note: any ::() are handled within <name>, and subscript must be final part.
         # A bare ::($foo) is not considered a variable, but ::($foo)::<$bar> is.
         # (The point being that we want a sigil either first or last but not both.)
         #= FOO::<$x>
         let_pos{ name and scan(/::/) and before(/[\<\{«]/) and postcircumfix }
         )
    end

    def_tokens_simple :sigil,false,%w{ $ @@ @ % & :: }
    def_tokens_simple :twigil,false,%w{ . ! ^ : * + ? = }

    def name
        ident_ = morename_ = nil
        b = pos
        (let_pos{ ident_ = ident and nofat and morename_ = starTOK{morename} } or
         morename_ = plusTOK{morename}) or return false
        h = {}
        _hkv(h,:ident,ident_)
        _hkv(h,:morename,morename_)
        _match_from(b,h,:name)
    end

    def morename
        (scan(/::/) and 
         ((ident) or
          (scan(/\(/) and _EXPR and scan(/\)/))))
    end

    def subshortname
        b=pos
        (let_pos{
             c= category and cp= plusTOK{colonpair} and
             _match_from(b,{:category=>c,:colonpair=>cp},:subshortname)
         } or
         (dsn= desigilname and
          _match_from(b,{:desigilname=>dsn},:subshortname))
         )
    end
    
    def sublongname
        subshortname and (sigterm;true)
    end

    def subcall
        # XXX should this be sublongname?
        let_pos{b=pos; n=subshortname and unsp? and (scan(/\./);true) and scan(/\(/) and l=semilist and scan(/\)/) and _match_from(b,{:subshortname=>n,:semilist=>l},:subcall) }
    end

    def value; quote || number || version || fulltypename; end

    def typename
        b=pos
        let_pos{ n = name and is_type(n.str) and 
            # parametric type?
            unsp? and
            quesTOK{ before(/\[/) and postcircumfix } and
            (h={};
             _hkv(h,:name,n)
             _match_from(b,h,:typename))
        }
    end

    def fulltypename #R regex XXX
        b = pos
        tn= typename and
            #R# quesRX{ wsp and scan(/of/) and wsp and fulltypename }
            rest= quesRULE{ scan(/of/) and wsp and fulltypename } and
            (a=[tn]
             a += rest[0][:typename] if not rest.empty?
             h={:typename=>a};
             _match_from(b,h,:fulltypename))
    end

    def number; dec_number || integer || rad_number; end
    def integer
        _match_pat %r{
            0 ( b [01]+           ( _ [01]+ )*
              | o [0-7]+         ( _ [0-7]+ )*
              | x [0-9a-fA-F]+ ( _ [0-9a-fA-F]+ )*
              | d \d+               ( _ \d+)*
              | \d+(_\d+)*
              )
            | \d+(_\d+)*
        }x,'integer'
    end
    def radint; integer or (let_pos{ r = rad_number and r[:intpart] and not r[:fracpart] }); end
    def dec_number; scan(/\d+(?:_\d+)*
                           (?: (?: \. \d+(?:_\d+)* (?: [Ee] [+\-]? \d+ )?)
                         |                         (?: [Ee] [+\-]? \d+ ))/x); end
    def rad_number
        let_pos{
            b=pos
            radix_=intpart=fracpart=base=exp=pc=nil
            scan(/:/) and radix_ = scan(/\d+/) and unsp? and
            ( ( scan(/</) and
                intpart = scan(/[0-9a-zA-Z_]+/) and #R XXX NONSPEC ADDED _
                (fracpart = scan(/\.[0-9a-zA-Z_]+/);true) and  #R XXX NONSPEC ADDED _
                (scan(/\*/) and base = radint and scan(/\*\*/) and exp = radint;true) and
                scan(/>/)) or
              ( before(/\[/) and pc= postcircumfix ) or
              ( before(/\(/) and pc= postcircumfix )) and
            (h={}
             _hkv(h,:radix,radix_)
             _hkv(h,:intpart,intpart)
             _hkv(h,:fracpart,fracpart)
             _hkv(h,:base,base)
             _hkv(h,:exp,exp)
             _hkv(h,:postcircumfix,pc)
             _match_from(b,h,:rad_number))
        }
    end
    def octint; scan(/[0-7]+/); end
    def hexint; scan(/[0-9a-fA-F]+/); end




    $herestub_queue = []

    def q_herestub(lang)
        (xXXX[:delimstr] = quotesnabber() or  # force raw semantics on /END/ marker
         return false)
        hs = Herestub.new($xXXX[:delimstr][:delimited][:q][:text], # XXX or some such
                          xXXX,
                          lang);
        $herestub_queue.push hs
    end

    class Herestub
        attr_accessor :delim,:orignode,:lang
        def initialize; @delim,@orignode,@lang=delim,orignode,lang; end
    end

    def theredoc
        @scanner.bol? and scan(/[ \t]*?/) and eat($env_vars[:delim]) and scan(/[ \t]*$\n?/)
    end

    # XXX be sure to temporize @herestub_queue on reentry to new line of heredocs

    def heredoc()
        here = self
        while herestub = $herestub_queue.shift
            $env_vars.scope_enter(:delim)
            $env_vars[:delim] = herestub.delim
            lang = herestub.lang
            doc = nil
            wsS = ""
            here = here.q_unbalanced_rule(lang, method(:theredoc)).xMATCHIFY;
            if here.bool
                if wsS != ""
                    wsequiv = wsS;
                    wsequiv.gsub!(/\A( *)\t/,"$1        "); # per spec
                    here[:text][0].sub!(/\A/,"\n"); # so we don't match ^^ after escapes
                    here[:text].each{|s__|
                        s__.gsub!(/\n(#{wsS}||[ \t]*)/){
                            white = $1;
                            if white == wsS
                                '';
                            else
                                white.sub!(/\A(\t+)/){ #R QUESTION shouldn't it be ^ instead of \A?
                                    #R XXX TODO
                                    #R ' ' x ($0.chars * (COMPILING::<$?TABSTOP> // 8))
                                };
                                white.sub!(/\A#{wsequiv}/,'') ? white : '';
                            end
                        }
                    }
                    here[:text][0].sub!(/^\n/,'')
                end
                herestub.orignode[:doc] = here;
            else
                panic("Ending delimiter $delim not found");
            end
            $env_vars.scope_leave
        end
        return here;
    end


    def self.def_quote(name,args)
        left_sym, = name.split(/\s+/)
        def_token_full :quote,false,name,Regexp.new(Regexp.quote(left_sym))," quotesnabber(#{args})"
    end
    #R# XXX NONSPEC last "close" argument to quotesnabber is non-spec.
    def_quote "' '"  ,%q{':q',"'"}
    def_quote '" "'  ,%q{':qq','"'}
    def_quote '« »'  ,%q{':qq',':ww','»'}
    def_quote '<< >>',%q{':qq',':ww','>>'}
    def_quote '< >'  ,%q{':q',':w','>'}
    def_quote '/ /'  ,%q{':regex','/'}

    # handle composite forms like qww
    def_tokens_rest :quote,false,%w{ qq q },%q{ nofat and  qm = quote_mod and quotesnabber(':<sym>',qm) }

    def_tokens_simple :quote_mod,false,%w{ w ww x to s a h f c b }

    def_tokens_rest :quote,false,%w{ rx m },%q{
      nofat and mod= starTOK{ quotepair } and q= quotesnabber(':regex') and
      (h={}
       _hkv(h,:quotepair,mod)
       _hkv(h,:quotesnabber,q)
       _match_from(start,h,:regex))
    }
    def_tokens_rest :quote,false,%w{ mm },%q{ nofat and quotesnabber(':regex', ':s') }
    def_tokens_rest :quote,false,%w{ s },%q{ nofat and pat=quotesnabber(':regex') and finish_subst(pat) }
    def_tokens_rest :quote,false,%w{ ss },%q{ nofat and pat=quotesnabber(':regex', ':s') and finish_subst(pat) }
    def_tokens_rest :quote,false,%w{ tr },%q{ nofat and pat=quotesnabber(':trans') and finish_subst(pat) }

    def finish_subst(pat)
        $env_vars.scope_enter(:thisop)
        u =(
            # bracketed form
            (pat[:delim] == 2 and ((wsp and infix and
                                    ($env_vars[:thisop][:prec] == Hitem_assignment[:prec] or
                                     panic("Bracketed subst must use some form of assignment")) and
                                    repl=_EXPR(Hitem_assignment)) or :failed)) or
            # unbracketed form
            (repl=q_unbalanced(qlang('Q',':qq'), pat[:delim][0])))
        $env_vars.scope_leave
        u = false if u == :failed
        u
    end

    def finish_trans(pat)
        u =(
            # bracketed form
            (pat[:delim] == 2 and ((wsp and 
                                    repl==q_pickdelim(qlang('Q',':tr'))) or :failed)) or
            # unbracketed form
            (repl=q_unbalanced(qlang('Q',':tr'), pat[:delim][0])))
        u
    end

    # The key observation here is that the inside of quoted constructs may
    # be any of a lot of different sublanguages, and we have to parameterize
    # which parse rule to use as well as what options to feed that parse rule.

    $qlang = {}
    def qlang(*pedigreeA)
        pedigreeS = pedigreeA.inspect
        $qlang[pedigreeS] ||= QLang.create(pedigreeA)
    end

    class QLang
        attr_accessor :option,:tweaker,:parser,:escrule
        def initialize(option,tweaker,parser,escrule)
            @option,@tweaker,@parser,@escrule=option,tweaker,parser,escrule
            initialize_escapes
        end
        attr_accessor :escapes
        def initialize_escapes(*a)
            # super(*a)
            @escapes = []
            if @option
                @escapes.push('\\') if @option[:b]
                @escapes.push('$') if @option[:s]
                @escapes.push('@') if @option[:a]
                @escapes.push('%') if @option[:h]
                @escapes.push('&') if @option[:f]
                @escapes.push('{') if @option[:c]
            end
        end
        def escset; @escapes end
        def self.create(pedigree)
            q = QLang.new(nil,nil,nil,nil)
            base = pedigree.shift
            eval("q.init_root_of_#{base}")
            pedigree.each{|mod|
                ma = mod.match(/^:(!)?(\w+)$/) or raise "bug"
                all,neg,name = ma.to_a
                q.tweak(name => (neg ? false : 1))
            }
            q
        end
        def init_root_of_Q
            @option  = {}
            @tweaker = :the_Q_tweaker
            @parser  = :q_pickdelim
            @escrule = :quote_escapes
        end
        def tweak(opt)
            send(@tweaker,opt)
        end
        
        def the_Q_tweaker(opt)
            k,v = opt.to_a[0]
            ks = k.to_s
            if %w{ q }.member? ks
                v or panic("Can't turn :q back off");
                (not @option.empty?) and panic("Too late for :q");
                @option = {:b=>1, :s=>false, :a=>false, :h=>false, :f=>false, :c=>false};
            elsif %w{ qq }.member? ks
                v or panic("Can't turn :qq back off");
                (not @option.empty?) and panic("Too late for :qq");
                @option = {:b=>1, :s=>1, :a=>1, :h=>1, :f=>1, :c=>1};
            elsif %w{ b s a h f c  x w ww }.member? ks
                @option[k] = v
            elsif %w{ to }.member? ks
                @parser = :q_heredoc
                @option[k] = v
            elsif %w{ regex }.member? ks
                @tweaker = :the_RX_tweaker
                @parser = :rx_pickdelim
                @option = {}
                @escrule = :regex_metachar
            elsif %w{ trans }.member? ks
                @tweaker = :the_TR_tweaker
                @parser = :tr_pickdelim
                @option = {}
                @escrule = :trans_metachar
            elsif %w{ code }.member? ks
                @tweaker = :the_RX_tweaker
                @parser = :rx_pickdelim
                @option = {}
                @escrule = :regex_metachar
            else
                panic("Unrecognized quote modifier: #{k}")
            end
        end
        def the_RX_tweaker(opt)
            k,v = opt.to_a[0]
            ks = k.to_s
            if %w{ g i ii b bb c p ov ex s }.member? ks
                @option[k] = v
            elsif %w{ bytes codes graphs langs }.member? ks
                @option[:UNILEVEL] = ks
            elsif %w{ rw ratchet keepall panic }.member? ks
                @option[k] = v
            elsif %w{ P5 }.member? ks
                # XXX probably wrong
                @option[k] = v
                @tweaker = :the_P5RX_tweaker
                @parser = :p5rx_pickdelim
                @escrule = :p5regex_metachar
            elsif %w{ nth x }.member? ks
                @option[k] = v
            elsif ma= ks.match(/^(\d+)([a-z]+)$/)
                all,n,a= ma.to_a
                if a == 'x'
                    @option[a] = n
                elsif %w{ st nd rd th }.member? a
                    @option['nth'] = n
                else
                    #R added
                    panic("Unrecognized nth-like regex modifier: #{k}")
                end
            else
                panic("Unrecognized regex modifier: #{k}");
            end
        end
        def the_P5RX_tweaker(opt)
            k,v = opt.to_a[0]
            if %w{ g i s m }.member? k.to_s
                @option[k] = v
            else
                panic("Unrecognized Perl 5 regex modifier: #{k}")
            end
        end
        def the_TR_tweaker(opt)
            k,v = opt.to_a[0]
            if %w{ c d s }.member? k.to_s
                @option[k] = v
            else
                panic("Unrecognized transliteration modifier: #{k}")
            end
        end
    end

    #R# This is a hack version of quotesnabber.
    def quotesnabber(kind,*a)
        b1 = pos #R off by one
        close = a.pop
        if not close
            close = "'" if kind == ':q'
            close = '"' if kind == ':qq'
            if kind == ':regex'
                return false if not scan(/\//)
                b1 += 1
                close = '/'
            end
        end
        word = a[0]
        if close == "'"
            s= scan(/(?:[^\'\\]|\\.)*\'/) or panic("Error in quotesnabber")
            s.slice!(-1,1)
            _match_from(b1-1,{:text=>s},:q)
        elsif close == '"'
            s= scan(/(?:[^\"\\]|\\.)*\"/) or panic("Error in quotesnabber")
            s.slice!(-1,1)
            _match_from(b1-1,{:text=>s},:qq)
        elsif kind == ':regex'
            close == '/' or panic("STD_red bug")
            s= scan(/(?:[^\/\\]|\\.)*\//) or panic("Error in quotesnabber")
            s.slice!(-1,1)
            _match_from(b1-1,{:text=>s},:regex)
        elsif close == '>'
            s= scan(/(?:[^\>\\]|\\.)*\>/) or panic("Error in quotesnabber")
            s.slice!(-1,1)
            _match_from(b1-1,{:text=>s},:q_w)
        elsif kind == ':qq' and word == ':ww'
            if close == '>>'
                s= scan(/(?:[^\>\\]|\\.|>(?!\>))*>>/) or panic("Error in quotesnabber")
                s.slice!(-2,2)
                _match_from(b1-1,{:text=>s},:qq_ww)
            elsif close == '»'
                s= scan(/(?:[^»\\]|\\.)*»/) or panic("Error in quotesnabber")
                s.slice!(-1,1)
                _match_from(b1-1,{:text=>s},:qq_ww)
            else; raise "bug"
            end
        else
            p kind, close
            raise "bug: #{kind} #{close}"
        end
    end


    def quotesnabber__non_fake(*qA)
        $env_vars.scope_enter(:delim)
        $env_vars[:delim] = '' 
        v = ((not before(/\w/)) and nofat and #R XX? ::
         wsp and
         starTOK{ q = quotepair and qA.push(q) and wsp } and
         # Dispatch to current lang's subparser.
         ( lang = qlang('Q',*qA) and false ) #R XXX I don't understand this yet.
         # {{
         # my $lang = qlang('Q', @q);
         # $<delimited> := $lang.parser.($lang);  # XXX probably wrong
         # $<delim> = $delim;
         #}}
             )
        $env_vars.scope_leave
        v
    end

    # XXX should eventually be derived from current Unicode tables.
    if "\u0028" != "("
        #R while unicode in ruby 1.8 can sortof kindof be supported,
        #R using additional libraries, the real answer is... use 1.9.
        Open2close = {
            "\x0028" => "\x0029", "\x003C" => "\x003E", "\x005B" => "\x005D",
            "\x007B" => "\x007D", "\x00AB" => "\x00BB" }
    else 
        Open2close =  {
            "\u0028" => "\u0029", "\u003C" => "\u003E", "\u005B" => "\u005D",
            "\u007B" => "\u007D", "\u00AB" => "\u00BB", "\u0F3A" => "\u0F3B",
            "\u0F3C" => "\u0F3D", "\u169B" => "\u169C", "\u2039" => "\u203A",
            "\u2045" => "\u2046", "\u207D" => "\u207E", "\u208D" => "\u208E",
            "\u2208" => "\u220B", "\u2209" => "\u220C", "\u220A" => "\u220D",
            "\u2215" => "\u29F5", "\u223C" => "\u223D", "\u2243" => "\u22CD",
            "\u2252" => "\u2253", "\u2254" => "\u2255", "\u2264" => "\u2265",
            "\u2266" => "\u2267", "\u2268" => "\u2269", "\u226A" => "\u226B",
            "\u226E" => "\u226F", "\u2270" => "\u2271", "\u2272" => "\u2273",
            "\u2274" => "\u2275", "\u2276" => "\u2277", "\u2278" => "\u2279",
            "\u227A" => "\u227B", "\u227C" => "\u227D", "\u227E" => "\u227F",
            "\u2280" => "\u2281", "\u2282" => "\u2283", "\u2284" => "\u2285",
            "\u2286" => "\u2287", "\u2288" => "\u2289", "\u228A" => "\u228B",
            "\u228F" => "\u2290", "\u2291" => "\u2292", "\u2298" => "\u29B8",
            "\u22A2" => "\u22A3", "\u22A6" => "\u2ADE", "\u22A8" => "\u2AE4",
            "\u22A9" => "\u2AE3", "\u22AB" => "\u2AE5", "\u22B0" => "\u22B1",
            "\u22B2" => "\u22B3", "\u22B4" => "\u22B5", "\u22B6" => "\u22B7",
            "\u22C9" => "\u22CA", "\u22CB" => "\u22CC", "\u22D0" => "\u22D1",
            "\u22D6" => "\u22D7", "\u22D8" => "\u22D9", "\u22DA" => "\u22DB",
            "\u22DC" => "\u22DD", "\u22DE" => "\u22DF", "\u22E0" => "\u22E1",
            "\u22E2" => "\u22E3", "\u22E4" => "\u22E5", "\u22E6" => "\u22E7",
            "\u22E8" => "\u22E9", "\u22EA" => "\u22EB", "\u22EC" => "\u22ED",
            "\u22F0" => "\u22F1", "\u22F2" => "\u22FA", "\u22F3" => "\u22FB",
            "\u22F4" => "\u22FC", "\u22F6" => "\u22FD", "\u22F7" => "\u22FE",
            "\u2308" => "\u2309", "\u230A" => "\u230B", "\u2329" => "\u232A",
            "\u23B4" => "\u23B5", "\u2768" => "\u2769", "\u276A" => "\u276B",
            "\u276C" => "\u276D", "\u276E" => "\u276F", "\u2770" => "\u2771",
            "\u2772" => "\u2773", "\u2774" => "\u2775", "\u27C3" => "\u27C4",
            "\u27C5" => "\u27C6", "\u27D5" => "\u27D6", "\u27DD" => "\u27DE",
            "\u27E2" => "\u27E3", "\u27E4" => "\u27E5", "\u27E6" => "\u27E7",
            "\u27E8" => "\u27E9", "\u27EA" => "\u27EB", "\u2983" => "\u2984",
            "\u2985" => "\u2986", "\u2987" => "\u2988", "\u2989" => "\u298A",
            "\u298B" => "\u298C", "\u298D" => "\u298E", "\u298F" => "\u2990",
            "\u2991" => "\u2992", "\u2993" => "\u2994", "\u2995" => "\u2996",
            "\u2997" => "\u2998", "\u29C0" => "\u29C1", "\u29C4" => "\u29C5",
            "\u29CF" => "\u29D0", "\u29D1" => "\u29D2", "\u29D4" => "\u29D5",
            "\u29D8" => "\u29D9", "\u29DA" => "\u29DB", "\u29F8" => "\u29F9",
            "\u29FC" => "\u29FD", "\u2A2B" => "\u2A2C", "\u2A2D" => "\u2A2E",
            "\u2A34" => "\u2A35", "\u2A3C" => "\u2A3D", "\u2A64" => "\u2A65",
            "\u2A79" => "\u2A7A", "\u2A7D" => "\u2A7E", "\u2A7F" => "\u2A80",
            "\u2A81" => "\u2A82", "\u2A83" => "\u2A84", "\u2A8B" => "\u2A8C",
            "\u2A91" => "\u2A92", "\u2A93" => "\u2A94", "\u2A95" => "\u2A96",
            "\u2A97" => "\u2A98", "\u2A99" => "\u2A9A", "\u2A9B" => "\u2A9C",
            "\u2AA1" => "\u2AA2", "\u2AA6" => "\u2AA7", "\u2AA8" => "\u2AA9",
            "\u2AAA" => "\u2AAB", "\u2AAC" => "\u2AAD", "\u2AAF" => "\u2AB0",
            "\u2AB3" => "\u2AB4", "\u2ABB" => "\u2ABC", "\u2ABD" => "\u2ABE",
            "\u2ABF" => "\u2AC0", "\u2AC1" => "\u2AC2", "\u2AC3" => "\u2AC4",
            "\u2AC5" => "\u2AC6", "\u2ACD" => "\u2ACE", "\u2ACF" => "\u2AD0",
            "\u2AD1" => "\u2AD2", "\u2AD3" => "\u2AD4", "\u2AD5" => "\u2AD6",
            "\u2AEC" => "\u2AED", "\u2AF7" => "\u2AF8", "\u2AF9" => "\u2AFA",
            "\u2E02" => "\u2E03", "\u2E04" => "\u2E05", "\u2E09" => "\u2E0A",
            "\u2E0C" => "\u2E0D", "\u2E1C" => "\u2E1D", "\u3008" => "\u3009",
            "\u300A" => "\u300B", "\u300C" => "\u300D", "\u300E" => "\u300F",
            "\u3010" => "\u3011", "\u3014" => "\u3015", "\u3016" => "\u3017",
            "\u3018" => "\u3019", "\u301A" => "\u301B", "\u301D" => "\u301E",
            "\uFD3E" => "\uFD3F", "\uFE17" => "\uFE18", "\uFE35" => "\uFE36",
            "\uFE37" => "\uFE38", "\uFE39" => "\uFE3A", "\uFE3B" => "\uFE3C",
            "\uFE3D" => "\uFE3E", "\uFE3F" => "\uFE40", "\uFE41" => "\uFE42",
            "\uFE43" => "\uFE44", "\uFE47" => "\uFE48", "\uFE59" => "\uFE5A",
            "\uFE5B" => "\uFE5C", "\uFE5D" => "\uFE5E", "\uFF08" => "\uFF09",
            "\uFF1C" => "\uFF1E", "\uFF3B" => "\uFF3D", "\uFF5B" => "\uFF5D",
            "\uFF5F" => "\uFF60", "\uFF62" => "\uFF63",
        }
    end


    # assumes whitespace is eaten already

    def peek_delimiters ()
        peek_brackets || (x = self.orig.slice(0,1); [x,x])
    end

    def peek_brackets ()
        b = pos
        if scan(/(?=\s)/)
            return false #R XXX added workaround
            panic("Whitespace not allowed as delimiter");
        # XXX not defined yet
        #    <?before <+isPe> > {
        #        self.panic("Use a closing delimiter for an opener is reserved");
        #    }
        elsif strt = scan(/(.)\1*/u) #R XXX huh?
            char = strt.match(/(.)\1*/u)[1]
            return false if not Open2close[char] #R XXX added workaround
            #R XXX QUESTION without a more selective check, <bracketed> fails below.
            #R perhaps R should panic() rather than die().
            rightbrack = Open2close[char] or raise "No matching close delimiter";
            stop = rightbrack * strt.length;
            strt = Regexp.new(Regexp.quote(strt)) #R# NOTE returning regexp
            stop = Regexp.new(Regexp.quote(stop)) #R# NOTE returning regexp
            return strt, stop;
        else
            raise "assert fail: can't get here"
        end
    end

    def bracketed (lang=nil)
        lang ||= qlang("Q")
        start,stop = peek_brackets()
        q=q_balanced(lang, start, stop)        
    end

    def q_pickdelim (lang)
        start,stop = peek_delimiters()
        if start == stop
            q_= q_unbalanced(lang, stop)
        else
            q_= q_balanced(lang, start, stop)
        end
    end

    def rx_pickdelim (lang)
        (let_pos{ (start,stop = peek_delimiters()) and (
                                                        if start == stop
                                                            q_= q_unbalanced(lang, stop)
                                                        else
                                                            q_= q_balanced(lang, start, stop)
                                                        end) } or
         ((stop = scan(/\S/) or panic("Regex delimiter must not be whitespace")) and
          rx_=regex(stop)) )
    end

    def tr_pickdelim (lang)
        (let_pos{( (start,stop = peek_delimiters()) and
                   scan(start) and
                   tr_= transliterator(stop))} or
         ((stop = scan(/\S/) or panic("tr delimiter must not be whitespace")) and
          tr_=transliterator(stop)) )
    end

    #R def transliterator(stop)
    #R   # XXX your ad here
    #R end

    def q_balanced (lang, start, stop, esc=nil)
        esc ||=  lang.escset
        b = pos
        start or return false
        scan(start) or return false
        while not before(stop)
            triple = /#{stop}#{stop}#{stop}/
            if before(triple)
                # XXX triple rule should just be in escapes to be customizable
                #R STD.pm no longer supports stop arg, but still uses it here.
                #R# dequote_ = _EXPR(HLOOSEST,triple) or return fail_at(b)
                dequote_ = _EXPR(HLOOSEST) or return fail_at(b)
            elsif before(start)
                subtext_ = q_balanced(lang, start, stop, esc) or fail_at(b)
            elsif before(esc)
                e=q_escape(lang) or return fail_at(b)
            else
                scan(/./um) or break
            end
        end
        scan(stop) or return fail_at(b)
        _match_from(b,{},'q_unbalanced_rule')
    end


    #R this differs from q_unbalanced() only in whether stop is rul or string.
    #R our convention is method name symbol, or regexp.
    #R XXX TODO the convention for esc... is a non-working work in progress.
    def q_unbalanced_rule (lang, stop, esc=nil)
        esc ||=  lang.escset
        b = pos
        while not before{ send(stop) }
            if before(esc)
                e=q_escape(lang) or return fail_at(b)
            else
                scan(/./um) or break
            end
        end
        send(stop) or return fail_at(b)
        _match_from(b,{},'q_unbalanced_rule')
    end

    def q_unbalanced (lang, stop, esc=nil)
        esc ||=  lang.escset
        b = pos
        while not before(stop)
            if before(esc)
                e=q_escape(lang) or return fail_at(b)
            else
                scan(/./um) or break
            end
        end
        scan(stop) or return fail_at(b)
        _match_from(b,{},'q_unbalanced')
    end


    # We get here only for escapes in escape set, even though more are defined.
    def q_escape (lang)
        lang[:escrule].call(self)
    end


    def quote_escapes
        (lex_pos{scan(/\\/) and qq_backslash} or
         (before(/\{/) and block) or
         (before(/$/) and variable and (extrapost;true)) or
         lex_pos{ variable and extrapost } or
         scan(/.|\n/u))
    end

    # Note, backtracks!  So expect_postfix mustn't commit to anything permanent.
    def extrapost
        $env_vars.scope_enter(:inquote)
        $env_vars[:inquote] = 1
        starRX(proc{ expect_postfix }){ after(/[\]\}\>\)]/) }
        $env_vars.scope_leave
    end

    def multisig
        rul{
            scan(/:?\(/) and wsp and
            s= signature and wsp and
            scan(/\)/) and wsp and
            starRULE{
                scan(/\|/) and wsp and
                scan(/:?\(/) and wsp and
                signature and wsp and scan(/\)/)
            } and
            s
        }
    end

    def routine_def
        rul{
            b=pos
            #R# i= quesRULE{ident} and wsp and 
            i= nil
            #R NONSPEC ADDED subshortname
            #R NONSPEC this is all implementation mechanism...
            (if i= subshortname and i[:category]
                cat = i[:category][:sym].to_sym
                cp0 = i[:colonpair][0]
                v = cp0[:value]
                n = (v and v[:ident]) || cp0[:structural][:kludge_name]
                prec = :additive #R XXX NONSPEC default precedence
                class << self;self;end.def_tokens_simple(cat,prec,[n])
            end; true) and wsp and
            s= quesRULE{multisig} and wsp and
            t= starRULE{trait} and wsp and
            k= block and
            (h={};
             _hkv(h,:ident,i)
             _hkv(h,:multisig,s)
             _hkv(h,:trait,t)
             _hkv(h,:block,k)
             _match_from(b,h,:routine_def))
        }
    end

    def method_def
        rul{
            b=pos
            i=s=nil
            (let_pos{i= ident and wsp and s= quesRULE{multisig}} or
              let_pos{before{sigil and scan(/\.[\[\{\(]/)} and sigil and postcircumfix }) and wsp and
            t= starRULE{trait} and wsp and
            k= block and
            (h={};
             _hkv(h,:ident,i)
             _hkv(h,:multisig,s)
             _hkv(h,:trait,t)
             _hkv(h,:block,k)
             _match_from(b,h,:method_def))
        }
    end

    def regex_def
        rul{
            b=pos
            s=[]
            i= quesRULE{ident} and wsp and
            t= starRULE{trait} and wsp and
            ( #R XXX NONSPEC ADDED 'token f :P5 {a}'
             a=nil
             plusTOK{ _cp = colonpair and (a ||= []; a.push(_cp);true) and wsp }
             true) and wsp and
            quesRULE{ scan(/:?\(/) and wsp and si= signature and si.push(si) and wsp and scan(/\)/) } and wsp and
            k= regex_block and
            (h={};
             _hkv(h,:ident,i[0])
             _hkv(h,:adverbs,a) #R XXX NONSPEC ADDED
             _hkv(h,:signature,s)
             _hkv(h,:trait,t)
             _hkv(h,:regex_block,k)
             _match_from(b,h,:regex_def))
        }
    end

    # XXX redundant with routine_def?
    def macro_def
        rul{ quesRULE{ident} and wsp and 
            quesRULE{multisig} and wsp and
            starRULE{trait} and wsp and
            block }
    end


    def trait
        rul{ trait_verb or trait_auxiliary }
    end

    def_rules_rest :trait_verb,%w{ is },%q{b=pos; i=ident and (pc=postcircumfix;true) and
      (h={:ident=>i};_hkv(h,:postcircumfix,pc);_match_from(b,h,:is)) }
    def_rules_rest :trait_verb,%w{ will },%q{ ident and wsp and block }
    def_rules_rest :trait_verb,%w{ of returns },%q{ fulltypename }
    def_rules_rest :trait_verb,%w{ handles },%q{ _EXPR }

    def capterm
        let_pos{ scan(/\\\(/) and c= capture and scan(/\)/) and c }
    end

    def capture
        b=pos
        e=nil
        wsp and (e= _EXPR and wsp;true) and #R XXX NONSPEC optional, for \().
            (h={}
             _hkv(h,:EXPR,e)
             _match_from(b,h,:capture))
    end

    def sigterm
        let_pos{ scan(/:\(/) and signature and scan(/\)/) }
    end

    def signature
        $env_vars.scope_enter(:zone)
        $env_vars[:zone] = 'posreq'
        v = rul{
            b = pos; ft=nil
            parsep = starRULE{
                p= parameter and wsp and
                (sep= scan(/,|:|;;|;/) or before(/-->|\)|\{/)) and
                (if sep and sep != ','
                     p.hash[:followed_by_separator] = sep; end #R NONSPEC
                 p)
            }
            wsp
            parsep and quesRULE{ scan(/-->/) and wsp and ft= fulltypename } and
            (h={:parsep=>parsep};
             _hkv(h,:fulltypename,ft)
             _match_from(b,h,:signature))
        }
        $env_vars.scope_leave
        v
    end

    def_rules_rest :type_declarator,%w{ subset },%q{ name and wsp and quesRULE{ scan(/of/) and wsp and fulltypename } and scan(/where/) and wsp and _EXPR }

    def type_constraint
        rul{ value or
            (scan(/where/) and _EXPR(Hchaining)) }
    end

    def post_constraint
        rul{ multisig or
            (scan(/where/) and _EXPR(Hchaining)) }
    end

    def param_var
        let_pos{
            b=pos
            cap=si=tw=id=pc=nil
            (cap= scan(/\|/) and wsp;true) and #R XXX NONSPEC ADDED (|$c) S06.html#Argument_list_binding
            (si=sigil and (tw=twigil;true) and
             (# Is it a longname declaration?
              (before{ si.str == '&' and ident } and
               (id= sublongname or return false;id)) or
              # Is it a shaped array or hash declaration?
              (let_pos{
                   (si.str == '@' or si.str == '%')  and
                   (id= ident;true) and wsp and before(/[\<\(\[\{]/) and
                   pc= postcircumfix
               }) or
              # ordinary parameter name
              (id=ident) or
              # bare sigil?          
              (null))) and
            (h={};
             _hkv(h,:capture,cap) #R XXX NONSPEC ADDED
             _hkv(h,:sigil,si)
             _hkv(h,:twigil,tw)
             _hkv(h,:ident,id)
             _hkv(h,:postcircumfix,pc)
             _match_from(b,h,:param_var))
        }
    end

    def parameter
        quantS = nil
        v = (
             b=pos
             pv=quantchar_=dv=nil
             tc= starTOK{ type_constraint } and 
             (let_pos{
                  quantchar_ = scan(/\*/) and pv = param_var and
                  slurp_=[quantchar_,pv] and quantS = '*'
              } or
              ((let_pos{
                    quantchar_= scan(/:/) and
                    (let_pos{ name_= ident and scan(/\(/) and param_var and scan(/\)/) } or
                     let_pos{ pv= param_var and name_ = pv[:ident] }) and
                    quantS = '*' and
                    named_ = true #R ...
                } or
                let_pos{
                    pv= param_var and quantS = '!'
                }) and
               quesTOK{
                   quantchar_ = scan(/[\?!]/) and quantS = quantchar_
               })) and
               starTOK{ trait } and
               starTOK{ post_constraint } and
               quesTOK{
                 (dv= default_value and
                  (case quantchar_
                   when '!'
                       panic("Can't put a default on a required parameter")
                   when '*'
                       panic("Can't put a default on a slurpy parameter")
                   end
                   quantS = '?'))
             } and
             (h={};
              _hkv(h,:param_var,pv)
              _hkv(h,:quantchar,quantchar_)
              _hkv(h,:type_constraint,tc)
              _hkv(h,:default_value,dv)
              _match_from(b,h,:parameter)
              )) or return false
        # enforce zone constraints
        case quantS
        when '!'
            case $env_vars[:zone]
            when :posopt
                panic("Can't use required parameter in optional zone")
            when :var
                panic("Can't use required parameter in variadic zone")
            else
                STDERR.print "Ignoring alleged zone violation.\n" if not $quiet
                #raise 'bug?' #R XXX
            end
        when '?'
            case $env_vars[:zone]
            when :posreq
                $env_vars[:zone] = :posopt
            when :var
                panic("Can't use optional positional parameter in variadic zone")
            else
                STDERR.print "Ignoring alleged zone violation.\n" if not $quiet
                #R# raise 'bug?' #R XXX but I'm not sure why this isn't working.
            end
        when '*'
            $env_vars[:zone] = :var
        else
            p quantS
            raise 'bug?'
        end
        v
    end

    def default_value
        rul{ scan(/\=/) and wsp and _EXPR(Hitem_assignment) }
    end

    def_tokens_rest :statement_prefix,false,%w{ do try gather contend async lazy },%q{
      s= statement and
      _match_from(start,{:statement=>s},:<sym>) }


    ## term
    def_tokens_rest :term,:term,%w{ undef },%q{ scan(/undef/) and scan(/[ \t]*/) and nofat }
    def_tokens_simple :term,:term,%w{ self * }

    def self.sigil_speed_hack_re; /\$|@@|@|%|&|::/; end
    def_token_full :circumfix,:term,'sigil',sigil_speed_hack_re,%q{ scan(/\(/) and sl= semilist and scan(/\)/) and sl }

    def_token_full :circumfix,:term,'typename',//,%q{ typename and scan(/\(/) and sl= semilist and scan(/\)/) and sl }

    def_tokens_rest :circumfix,:term,%w{ ( },%q{let_pos{ t = statementlist and scan(/\)/) and t }}
    def_tokens_rest :circumfix,:term,%w{ [ },%q{let_pos{ t = statementlist and scan(/\]/) and t }}
    def_tokens_rest :circumfix,:term,%w{ < },%q{let_pos{ t = anglewords('>') and scan(/>/) and t }}
    def_tokens_rest :circumfix,:term,%w{ << },%q{let_pos{ t = shellwords('>>') and scan(/>>/) and t }}
    def_tokens_rest :circumfix,:term,%w{ « },%q{let_pos{ t = shellwords('»') and scan(/»/) and t }}
    #R# these . and -> are <obs>.  I'm not sure why they were translated.
    #R def_tokens_simple :infix,:methodcall,%w{ . }
    #R def_tokens_simple :postfix,:methodcall,%w{ -> }
    def_tokens_simple :postfix,:autoincrement,%w{ ++ -- i }
    def_tokens_simple :prefix,:autoincrement,%w{ ++ -- }
    def_tokens_simple :infix,:exponentiation,%w{ ** }
    def_tokens_simple :prefix,:symbolic_unary,%w{ ! + - ~ ? = * ** ~^ +^ ?^ ^ | \\ } #R NONSPEC ADDED \\
    def_tokens_simple :infix,:multiplicative,%w{ * / % +& +< << >> +> ~&> ~< ~> ?& div mod } #R NONSPEC ADDED ?& div mod
    def_tokens_simple :infix,:additive,%w{ + - +| +^ ~| ~^ ?| ?^ }
    def_tokens_simple :infix,:replication,%w{ x xx }
    def_tokens_simple :infix,:concatenation,%w{ ~ }
    def_tokens_simple :infix,:junctive_and,%w{ & }
    def_tokens_simple :infix,:junctive_or,%w{ | ^ }
    def_tokens_simple :prefix,:named_unary,%w{ rand sleep abs }
    def_tokens_simple :prefix,:named_unary,%w{ sin cos tan asin acos atan exp }
    def_tokens_simple :infix,:nonchaining,%w{ <=> cmp leg is but does .. ^.. ..^ ^..^ } #R NONSPEC ADDED leg. Moved ff per S03 + TT.
    def_tokens_simple :infix,:chaining,%w{ == != < <= > >= ~~ !~ =~ eq ne lt le gt ge =:= === eqv } #R NONSPEC ADDED eqv.
    def_tokens_simple :infix,:tight_and,%w{ && }
    def_tokens_simple :infix,:tight_or,%w{ || // min max } #R NONSPEC ADDED min max.
    def_tokens_simple :infix,:conditional,%w{  ff ^ff ff^ ^ff^ fff ^fff fff^ ^fff^ } #R NONSPEC Moved ff.
    def_tokens_rest :infix,:tight_or,%w{ ^^ },%q{
      _match_from(start,{:O=>{:assoc=>:list}},:'<sym>')  # override Tight_or's 'left' associativity
    }


    ## conditional
    def_token_full :infix,:conditional,'?? !!',/\?\?/,%q{
        wsp and
        e= _EXPR(Hconditional) and scan(/!!/) and
        _match_from(start,{:EXPR=>e},:conditional)
    }
    #R assorted cautionary panics left out

    ## assignment
    #R XXX Ignore list_assignment for now.
    def_tokens_simple :infix,:item_assignment,%w{ = }
    #R# def_tokens_rest :infix,false,%w{ = },%q{ xXXX[:sigil] == '$' ? make(xXXX,Hitem_assignment) : make(xXXX,Hlist_assignment) }

    def_tokens_simple :infix,:item_assignment,%w{ := ::= }
    # XXX need to do something to turn subcall into method call here...
    def_tokens_simple :infix,:item_assignment,%w{ .= => } #R NONSPEC ADDED  '=>' for $var => 2.
    # Note, other assignment ops generated by infix_postfix_meta_operator rule

    def_tokens_simple :prefix,:loose_unary,%w{ true not }
    def_tokens_simple :infix,:comma,%w{ , p5=> : }  #R NONSPEC added ':' for \(4:5,6)
    def_tokens_simple :infix,:list_infix,%w{ X Z minmax }

    def_token_full :term,:list_prefix,'sigil',sigil_speed_hack_re,%q{ before(/\s/) and arglist }
    def_token_full :term,:list_prefix,'typecast',//,%q{
        tn= typename and scan(/\s+/) and a= arglist and
        _begin_from(start,{:typename=>tn,:arglist=>a,:sym=>tn.str},:<sym>)
    }

    # unrecognized identifiers are assumed to be post-declared listops.
    # (XXX for cheating purposes this rule must be the last term: rule)
    def_tokens_rest :term,:list_prefix,[""],%q{
      b = pos
      a=nil
      s=ident and (let_pos{ nofat_space and a=arglist } or nofat) and
      #R# XXX ident's spec'ed name is <sym>, but that would cause us problems.
      (h={:ident=>s};
       _hkv(h,:arglist,a)
       _match_from(b,h,:listop))
    }
       
    def_tokens_simple :infix,:loose_and,%w{ and andthen }
    def_tokens_simple :infix,:loose_or,%w{ or xor orelse }
    def_tokens_before :terminator,:terminator,%w{ ; <== ==> --> ) ] \} !! <<== ==>> } #R NONSPEC ADDED <<== ==>> per S03.

    def infixstopper
        before{ scan(/\{/) or _lambda } and after(/\s/)
    end

    def stdstopper_no_mod #R XXX KLUDGE ADDED STD.pm bug workaround
        (@scanner.eos? || @scanner.check(/\n/) ||
         terminator || #R# no _mod_ # statement_mod_cond || statement_mod_loop ||
#R XXX issues with "do {}"
#         ((before(/\{/) or before{ _lambda }) and after(/\s/)) ||
         ws_from == $env_vars[:endstmt] ||
         pos == $env_vars[:endargs] ||
         false
         #R  #    | <$+unitstopper> #R?
         #R  #  scan_unitstopper
         )
    end
    def stdstopper
        (@scanner.eos? || @scanner.check(/\n/) ||
         terminator || statement_mod_cond || statement_mod_loop ||
#R XXX issues with "do {}"
#         ((before(/\{/) or before{ _lambda }) and after(/\s/)) ||
         ws_from == $env_vars[:endstmt] ||
         pos == $env_vars[:endargs] ||
         false
         #R  #    | <$+unitstopper> #R?
         #R  #  scan_unitstopper
         )
    end

    def pop(a);a.pop;end
    def push(a,e);a.push(e);end
    def shift(a);a.shift;end
    def reverse(a);a.reverse;end
    def item(h);raise "what does item do?";end
    def _EXPR(*args); let_pos{ _EXPR_raw(*args) }; end
    def _EXPR_raw(preclimH=nil)
        noisy = false #R added
        #noisy = true #R added
        preclimH ||= HLOOSEST
        
        preclimS = preclimH[:prec];
        $env_vars.scope_enter(:inquoteS,:prevopS);
        $env_vars[:inquoteS] = 0
        termstackA = []
        opstackA = []

        # (just a sentinel value)
        push opstackA, {:O=>{:prec=>"a="},:sym=>''} #R kludge

        hereS_workaround = self
        hereS = nil
        say "In EXPR, at ", hereS_workaround.pos if noisy;

        reduce = lambda {
            say "entering reduce, termstack == ", termstackA.length, " opstack == ", opstackA.length if noisy;
            opS = pop(opstackA);

            case opS[:O][:assoc].to_s
            when 'chain' 
                say "reducing chain" if noisy;
                chainA = []
                push chainA, pop(termstackA);
                #R# push chainA, opS; #R XXX - seemed odd
                while not opstackA.empty? 
                    break if opS[:O][:prec] != opstackA[-1][:O][:prec];
                    break if opS[:sym] != opstackA[-1][:sym];
                    push chainA, pop(termstackA);
                    #R# push chainA, pop(opstackA); #R XXX - seemed odd
                    pop(opstackA); #R XXX from above
                end
                push chainA, pop(termstackA);
                #R# opS[:chain] = reverse chainA;
                opS[:args] = reverse chainA; #R XXX NONSPEC normalize name of argument list.
                push termstackA, opS;
            when 'list' 
                say "reducing list" if noisy;
                listA = []
                push listA, pop(termstackA);
                while not opstackA.empty? 
                    break if opS[:sym] != opstackA[-1][:sym];
                    push listA, pop(termstackA);
                    pop(opstackA);
                end
                push listA, pop(termstackA);
                pop listA if not listA[-1] #R XXX '(2,3,)' adds a nil
                #R# opS[:list] = reverse listA;
                opS[:args] = reverse listA; #R XXX NONSPEC normalize name of argument list.
                push termstackA, opS;
            else
                say "reducing" if noisy;
                listA = []
                say "Termstack size: ",termstackA.length if noisy;
                
                if opS[:O][:assoc]
                    #R# opS[:right] = pop termstackA;
                    #R# opS[:left] = pop termstackA;
                    right = pop termstackA;
                    left = pop termstackA;
                    opS[:args] = [left,right] #R XXX NONSPEC normalize name of argument list.
                else
                    #R# opS[:arg] = pop termstackA;
                    opS[:args] = [pop(termstackA)] #R XXX NONSPEC normalize name of argument list.
                end

                push termstackA, opS;
            end
        }

        deepcopy = lambda {|o| Marshal.load(Marshal.dump(o)) } #R XXX KLUDGE

        while true 
            say "In loop, at ", hereS_workaround.pos if noisy;
            oldposS = hereS_workaround.pos
            tA = [hereS_workaround.expect_term()]  # eats ws too
            tA = [] if not tA[0] #R
            break unless tA.length > 0
            hereS = tA[0]
            break unless hereS_workaround.pos > oldposS

            # interleave prefix and postfix, pretend they're infixish
            mS = hereS
            preA = []
            preA = mS[:pre].dup if mS[:pre]
            postA = []
            postA = mS[:post].dup if mS[:post]
            while true
                if preA.length > 0
                    if postA.length > 0 and postA[0][:O][:prec] > preA[0][:O][:prec]
                        push opstackA, deepcopy.(shift(postA)) #R XXX NONSPEC .dup to avoid cycles.
                    else
                        push opstackA, deepcopy.(pop(preA)) #R XXX NONSPEC .dup to avoid cycles.
                    end
                elsif postA.length > 0
                    push opstackA, deepcopy.(shift(postA)) #R XXX NONSPEC .dup to avoid cycles.
                else
                    break
                end
            end

            push termstackA, hereS
            say "after push: #{termstackA.length}" if noisy
            #        my infixA = [hereS_workaround.expect_tight_infix(preclimS)];
            oldposS = hereS_workaround.pos
            infixA = [hereS_workaround.expect_infix()]
            infixA = [] if not infixA[0] #R ruby
            break unless infixA.length > 0
            infixS = infixA[0];
            break unless pos > oldposS

            # XXX might want to allow this in a declaration though
            if not infixS;  hereS_workaround.panic("Can't have two terms in a row"); end

            if not infixS[:sym]
                p infixS
                raise "bug "
            end

            wsp

            inO = infixS[:O]
            inprec = inO[:prec]
            if not inprec
                say "No prec case in thisop!" if noisy;
                inprec = Hterminator[:prec]
            end
            # substitute precedence for listops
            inO[:prec] = inO[:sub] if inO[:sub]

            # Does new infix (or terminator) force any reductions?
            while opstackA[-1][:O][:prec] > inprec
                reduce.call();
            end
            
            # Not much point in reducing the sentinels...
            break if inprec < SLOOSEST;
            
            # Equal precedence, so use associativity to decide.
            if opstackA[-1][:O][:prec] == inprec
                case inO[:assoc].to_s
                when 'non' ;   hereS_workaround.panic("\"#{infixS}\" is not associative")
                when 'left' ;  reduce.call()   # reduce immediately
                when 'right';  # just shift
                when 'chain';  # just shift
                when 'list'                # if op differs reduce else shift
                    reduce.call() if infixS[:sym] != opstackA[-1][:sym];
                else
                    hereS_workaround.panic("Unknown associativity \"#{$env_vars[:thisopH][:assoc]}\" for \"#{infixS}\"")
                end
            end
            push opstackA, infixS
        end
        reduce.call() while termstackA.length > 1;
        if termstackA.length > 0
            termstackA.length == 1 or hereS_workaround.panic("Internal operator parser error, termstack == #{termstackA.length}");
        end
        $env_vars.scope_leave
        return termstackA[0] #R nil if array is empty
    end


    #############################################
    ## Regex
    #############################################
    #R# NONSPEC "patterns" as a common thing to call one's children.
    #R#   It greatly reduces noise.
    #R#   Were this precedence based, rather than a precedence-chain'o rules,
    #R#   something like it would already be done.
    #R#   Perhaps call it something else?

    def regex(stop="/")
        $env_vars.scope_enter(:stop)
        $env_vars[:stop] = stop
        b=pos
        v = rul{ regex_first }
        $env_vars.scope_leave
        v and _match_from(b,{:pattern=>v},:regex)
    end

    def regex_first
        b=pos
        s=nil
        rul{scan(/(\|\|)?/) and wsp and
            s= interleaveRULE(/\|\|/){ regex_every } } and
            (s.size == 1 ? s[0] :
            _match_from(b,{:patterns=>s},:regex_first))
    end

    def regex_every
        b=pos
        s=nil
        rul{ s= interleaveRULE(/&&/){ regex_submatch } } and
            (s.size == 1 ? s[0] :
            _match_from(b,{:patterns=>s},:regex_every))
    end

    def regex_submatch
        b=pos
        s=nil
        s= rul{ interleaveRULE(/!?~~/){ regex_any } } and
            (s.size == 1 ? s[0] :
             _match_from(b,{:patterns=>s},:regex_submatch))
    end

    def regex_any
        b=pos
        s=nil
        rul{ quesRULE{ scan(/\|(?!\|)/) } and
            s= interleaveRULE(/\|(?!\|)/){ regex_all } } and
            (s.size == 1 ? s[0] :
             _match_from(b,{:patterns=>s},:regex_any))
    end

    def regex_all
        b=pos
        s=nil
        s= rul{ interleaveRULE(/\&(?!\&)/){ regex_sequence } } and
            (s.size == 1 ? s[0] :
             _match_from(b,{:patterns=>s},:regex_all))
            
    end

    def regex_sequence
        b=pos
        s= plusRULE{regex_quantified_atom} and
            (s.size == 1 ? s[0] :
             _match_from(b,{:patterns=>s},:regex_sequence))
    end

    def regex_quantified_atom
        b=pos
        ra=rq=nil
        rul{ ra= regex_atom and wsp and
            (rq= regex_quantifier
             #R# and (ra.max_width or #R XXX
             #R# panic("Can't quantify zero-width atom"))
             ;true)
        } and
            !rq ? ra :
            (h={}
             _hkv(h,:regex_atom,ra)
             _hkv(h,:regex_quantifier,rq)
             _match_from(b,h,:regex_quantified_atom))
    end

    def regex_atom
        rmc=w=nil
        b=pos
        ((wsp and
          scan(/(?!#{'\\'+$env_vars[:stop]})/) and
          (
           (rmc= regex_metachar and wsp) or
           #R As a speed/simplicity hack, eat multiple \w
           (w= scan(/\w(?:\w*(?=\w))?/) and wsp) or
           false #R XXX nonspec
           #R# panic("unrecognized metacharacter")
           )) and
         rmc ? rmc : (h={}
          _hkv(h,:char,w) # unspec
          _match_from(b,h,:regex_atom)))
    end

    def_tokens_rest :regex_metachar,false,%w{ > && & || | ] ) \\ },%q{ return false }

    regex_quantifier_re = /\*\*|\*|\+|\?/
    def_token_full :regex_metachar,false,'quant',regex_quantifier_re,%q{ panic("quantifier quantifies nothing") }

    # "normal" metachars
    def_token_full :regex_metachar,false,'{ }',/(?=\{)/,%q{
      b=pos
      bk= block and
      _match_from(b,{:block=>bk},:block) } #R NONSPEC rule name

    def_token_full :regex_metachar,false,'mod',//,%q{ regex_mod_internal }

    def_token_full :regex_metachar,false,'[ ]',/(?=\[)/,%q{
       b=pos
       scan(/\[/) and r=  regex(']') and scan(/\]/)
       (_match_from(b,{:regex=>r},:group))} #R NONSPEC rule name
    def_token_full :regex_metachar,false,'( )',/(?=\()/,%q{
       b=pos
       scan(/\(/) and r= regex(')') and scan(/\)/) and
       (_match_from(b,{:regex=>r},:capture))} #R NONSPEC rule name

    def_tokens_simple :regex_metachar,false,%w{ <( )> << >> « » }

    def_token_full :regex_metachar,false,'qw',/(?=<\s)/,%q{ quote }

    def_token_full :regex_metachar,false,'< >',/\</,%q{ unsp; r= regex_assertion and scan(/\>/) and r }
    def_tokens_rest :regex_metachar,false,%w{ \\ },%q{ regex_backslash }
    def_tokens_simple :regex_metachar,false,%w{ . ^^ ^ }
    def_tokens_simple :regex_metachar,false,%w{ $$ }
    def_tokens_rest :regex_metachar,false,%w{ $ },%q{
      before(/\s|\||\&|\)|\]|\>|\}/) or #R NONSPEC KLUDGE added \}, for eos-ness.  May need '/' too.
      @scanner.eos?
    }

    def_token_full :regex_metachar,false,"' '",/\'/,%q{ quotesnabber(":q") }
    def_token_full :regex_metachar,false,'" "',/\"/,%q{ quotesnabber(":qq") }

    def_tokens_rest :regex_metachar,false,%w{ ::: :: },%q{
      _match_from(start,{},:commit) #R NONSPEC rule name
    }
    def_token_full :regex_metachar,false,':',/:(?!\w)/,%q{
      _match_from(start,{},:commit) #R NONSPEC rule name
    }

    def_token_full :regex_metachar,false,'var',/(?!\$\$|:)/,%q{ #R ADDED ':' check, helping commit
        binding_=nil
        sym=variable and wsp and (scan(/=/) and wsp and
        binding_= regex_quantified_atom; true) and
        _match_from(b,{:variable=>sym,:binding=>binding_},:var)
    }

    def codepoint
        scan(/\[(.*?)\]/u)
    end

    def_tokens_before :q_backslash,false,%w{ qq },%q{ quote }
    def_tokens_rest :q_backslash,false,[""],%q{ scan(/./u) }

    def_tokens_rest :qq_backslash,false,%w{ c },%q{ scan(/\[[^\]\n\r]*\]/) or codepoint }

    def_tokens_simple :qq_backslash,false,%w{ \\ a b e f n r t 0 }
    def_tokens_rest :qq_backslash,false,%w{ o },%q{ octint or (scan(/\[/) and octint and starTOK{ scan(/,/) and octint } and scan(/\]/)) }
    def_tokens_rest :qq_backslash,false,%w{ x },%q{ hexint or (scan(/\[/) and hexint and starTOK{ scan(/,/) and hexint } and scan(/\]/)) }

    def self.rx_bs(letter,rest=nil)
        letter = letter.to_s
        def_token_full :regex_backslash,false,letter,/(?i:#{letter})/,rest
    end
    %w{ a b d e f h n r s t v w }.each{|letter| rx_bs letter }
    rx_bs :c,%q{ (scan(/\[ [^\]\n\r]* \]/x) or
                  codepoint) }
    rx_bs :o,%q{ octint or (scan(/\[/) and octint and starTOK{ scan(/,/) and octint} and scan(/\]/)) }
    rx_bs :x,%q{ hexint or (scan(/\[/) and hexint and starTOK{ scan(/,/) and hexint} and scan(/\]/)) }
    def_token_full :regex_backslash,false,'misc',/\W/,nil #R NONSPEC should define a $<litchar>.
    def_token_full :regex_backslash,false,'oops',//,%q{ panic("unrecognized regex backslash sequence") }

    def_tokens_rest :regex_assertion,false,%w{ ? ! . },%q{ regex_assertion }

    def_token_full :regex_assertion,false,'{ }',/\{/,%q{ block }

    def_token_full :regex_assertion,false,'variable',/(?=#{sigil_speed_hack_re})/,%q{ before{ sigil } and _EXPR(nil,HLOOSEST) }
    def_token_full :regex_assertion,false,'method',/(?=\.(?!>))/,%q{
      before(/\.(?!>)(?=\w+\()/) and #R NONSPEC second clause is to avoid <.foo> parsing as method.
      d= dotty and _match_from(start,{:dotty=>d},:method)
    }
    def_token_full :regex_assertion,false,'ident',//,%q{
      b=pos
      ra=qu=sl=e=cc=nil
      (i= ident and a= quesTOK{
       (let_pos{ scan(/\=/) and ra= regex_assertion} or
        let_pos{ scan(/\:/) and wsp and qu= q_unbalanced(qlang('Q',':qq'), '>')} or
        let_pos{ scan(/\(/) and sl= semilist and scan(/\)/)} or
        let_pos{ wsp and e= regex }
        ) }) and
       (h={}
        _hkv(h,:ident,i)
        _hkv(h,:regex_assertion,ra)
        _hkv(h,:q_unbalanced,qu)
        _hkv(h,:semilist,sl)
        _hkv(h,:regex,e) #R NONSPEC
        _hkv(h,:cclass_elem,cc) #R NONSPEC
        _match_from(b,h,:ident))
     }

    def_token_full :regex_assertion,false,'cclass',/(?=[\[+-])/,%q{ e= plusTOK{cclass_elem} and _match_from(start,{:cclass_elem=>e},:cclass) }
    def_tokens_simple :regex_assertion,false,%w{ , }
    #R# def_tokens_simple :regex_assertion,false,%w{ . } #R XXX Causes problems for <.ws>
    def_tokens_rest :regex_assertion,false,%w{ ~~ },%q{ (desigilname;true) }

    def cclass_elem
        b = pos
        pm=n=cc=nil
        let_pos {
            pm= scan(/[-+]/)
            wsp
            ((n= (name) or
              (before(/\[/) and 
               #R# bracketed(QLang(:cclass))
               cc= scan(/\[\]?([^\]\\]|\\.)*?\]/) #R KLUDGE
               )) and
             (h={}
              _hkv(h,:op,pm) #R NONSPEC is unnamed
              _hkv(h,:name,n)
              _hkv(h,:bracketed,cc)
              _match_from(b,h,:cclass_elem)))
        }
    end


    def regex_mod_arg
        let_pos{ scan(/\(/) and sl= semilist and scan(/\)/) and sl }
    end

    def_token_full :regex_mod_internal,false,'adv',//,%q{ quotepair and true }
    #R XXX     <quotepair> { $/<sym> := «: $<quotepair><key>» }

    def self.rx_rmi(name)
        name = name.to_s
        n1 = ":#{name}"
        n2 = ":!#{name}"
        def_tokens_rest :regex_mod_internal,false,[ n1 ],%q{ rma= quesTOK{regex_mod_arg} and
          (h={}
           _hkv(h,:regex_mod_arg,rma[0])
           _match_from(start,h,:regex_mod_internal))
        }
        def_tokens_simple :regex_mod_internal,false,[ n2 ]
    end
    #R NONSPEC ADDED x nth
    #R NONSPEC rule names aren't quite right
    %w{ i insensitive b basechar r ratchet s sigspace  x nth }.each{|name| rx_rmi name}
    #R# def_token_full :regex_mod_internal,false,'oops',/:!?\w/,%q{ panic("unrecognized regex modifier") }

    def_tokens_rest :regex_quantifier,false,%w{ * + ? },%q{ quantmod }
    def_tokens_rest :regex_quantifier,false,%w{ ** },%q{ quantmod and wsp and scan(/\d+(?:\.\.(?:\d+|\*))?/) or block or regex_atom }
    def quantmod; scan(/[?!:+]/); true; end

end

class Perl
    Typenames = %w{
    Bit Int Str Num Complex Bool Rat
    Exception Code Block List Seq Range Set Bag Junction Pair
    Mapping Signature Capture Blob Whatever Undef Failure
    StrPos StrLen Version P6opaque
    bit int uint buf num complex bool rat
    Scalar Array Hash KeyHash KeySet KeyBag Buf IO Routine Sub Method
    Submethod Macro Regex Match Package Module Class Role Grammar Any Object }
    HTypenames = Hash[ *Typenames.map{|n|[n,1]}.flatten ]
    #R XXX NONSPEC - added 'starts with a capital letter' test.
    #R Which probably breaks something.
    #R# def is_type(name); HTypenames.key?(name) end
    def is_type(name); HTypenames.key?(name) or name =~ /^[A-Z](?![A-Z]*$)/ end

    #def heredoc; false; end
    def method_missing(method, *args)
        #print "FAKING #{method}\n"
        STDERR.print "FAKING #{method} for #{caller.slice(0,1)}\n"
        false
    end
end

## vim: expandtab sw=4
## Local Variables:
## ruby-indent-level: 4
## End:
