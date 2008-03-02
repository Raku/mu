# A ruby transliteration of src/perl6/STD.pm
# See README.
#
# STD issues
#   /categories added/ are still missing from STD.pm.
#   The "after the }" part of block and regex_block are different.
#     #R XXX this is assumed a typo, yes?
#   Added eos? check to eat_terminator.
#   "$wsequiv ~~ s/^ (\t+) /{ ' ' x ($0 * 8) }/; # per spec", ^^ instead of ^ ?
#

require 'prelude'

class Perl < Grammar
    attr_accessor :ws_from, :ws_to

    def _TOP; _UNIT( $env_vars[:unitstopper] || "_EOS" ); end

    def_precedence :hyper           ,{ :transparent=>1                           }
    def_precedence :term            ,{ :prec=>"z="                               }
    def_precedence :methodcall      ,{ :prec=>"y="                               }
    def_precedence :autoincrement   ,{ :prec=>"x="                               }
    def_precedence :exponentiation  ,{ :prec=>"w=", :assoc=>:right, :assign=>1 }
    def_precedence :symbolic_unary  ,{ :prec=>"v="                               }
    def_precedence :multiplicative  ,{ :prec=>"u=", :assoc=>:left,  :assign=>1 }
    def_precedence :additive        ,{ :prec=>"t=", :assoc=>:left,  :assign=>1 }
    def_precedence :replication     ,{ :prec=>"s=", :assoc=>:left,  :assign=>1 }
    def_precedence :concatenation   ,{ :prec=>"r=", :assoc=>:left,  :assign=>1 }
    def_precedence :junctive_and    ,{ :prec=>"q=", :assoc=>:list,  :assign=>1 }
    def_precedence :junctive_or     ,{ :prec=>"p=", :assoc=>:list,  :assign=>1 }
    def_precedence :named_unary     ,{ :prec=>"o=",                              }
    def_precedence :nonchaining     ,{ :prec=>"n=", :assoc=>:non                }
    def_precedence :chaining        ,{ :prec=>"m=", :assoc=>:chain, :bool=>1   }
    def_precedence :tight_and       ,{ :prec=>"l=", :assoc=>:left,  :assign=>1 }
    def_precedence :tight_or        ,{ :prec=>"k=", :assoc=>:left,  :assign=>1 }
    def_precedence :conditional     ,{ :prec=>"j=", :assoc=>:right,             }
    def_precedence :item_assignment ,{ :prec=>"i=", :assoc=>:right              }
    def_precedence :loose_unary     ,{ :prec=>"h=",                              }
    def_precedence :comma           ,{ :prec=>"g=", :assoc=>:list,              }
    def_precedence :list_infix      ,{ :prec=>"f=", :assoc=>:list,  :assign=>1 }
    def_precedence :list_assignment ,{ :prec=>"i=", :sub=>"e=", :assoc=>:right }
    def_precedence :list_prefix     ,{ :prec=>"e=",                              }
    def_precedence :loose_and       ,{ :prec=>"d=", :assoc=>:left,  :assign=>1 }
    def_precedence :loose_or        ,{ :prec=>"c=", :assoc=>:left,  :assign=>1 }
    def_precedence :LOOSEST         ,{ :prec=>"a=!",                             }
    def_precedence :terminator      ,{ :prec=>"a=", :assoc=>:list               }
    SLOOSEST = HLOOSEST[:prec]

    #R module PrecOp
    def precop_method(m,defaults)
        defaults.each{|k,v| m[k] = v if not m.key? k }
        if $env_vars[:thisopH]  #R non-spec, but... needed?
            $env_vars[:thisopH][:top] = m;
            if not m.key?(:transparent)
                $env_vars[:thisopH][:prec] = m[:prec];
                $env_vars[:thisopH][:assoc] = m[:assoc];
            end
        else
            STDERR.print("The $env_vars[:thisopH] non-spec check was used.\n")
        end
        return m;
    end
    #R XXX I'm unsure what make() (in expect_term, and elsewhwere) should be doing.
    def make(m,overwrite)
        defaults.each{|k,v| m[k] = v }
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
        (if us == '_EOS'; @scanner.eos?
         else; raise "assert: known unitstopper: #{us}"
         end) or panic("Can't understand next input--giving up")
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
    token_category :trait_verb,      'nofat_space'
    token_category :trait_auxiliary, 'nofat_space'
    token_category :type_declarator,    'nofat'
    token_category :scope_declarator,   'nofat'
    token_category :package_declarator, 'nofat'
    token_category :routine_declarator, 'nofat'
    rule_category  :statement_prefix,   'nofat'
    rule_category  :statement_control, 'nofat_space'
    rule_category  :statement_mod_cond, 'nofat'
    rule_category  :statement_mod_loop, 'nofat'
    token_category :infix_prefix_meta_operator
    token_category :infix_postfix_meta_operator
    token_category :infix_circumfixfix_meta_operator
    token_category :postfix_prefix_meta_operator
    token_category :prefix_postfix_meta_operator
    token_category :prefix_circumfix_meta_operator
    #R categories added:
    token_category :terminator
    token_category :infix_circumfix_meta_operator
    token_category :plurality_declarator
    token_category :regex_declarator

    def unspacey; unsp;true end
    def nofat_space; nofat and before(/\s|\#/); end

    # Lexical routines

    #R QUESTION: why is this regex not token?
    def nofat
        # make sure we're at end of a non-autoquoted identifier
        # regex nofat { <!before » \h* <.unsp>? '=>' > <!before \w> }
        (not before{ scan(/»/) and scan(/[ \t]*/) and (unsp;true) and scan(/=>/) } and
         not before(/\w/))
    end
    #R ws, renamed wsp to make life easier.
    def wsp
        pos == ws_to and return true
        after(/\w/) and before(/\w/) and return false
        ws_from = pos
        starTOK{ unsp || let_pos{ vws and heredoc} || unv }
        ws_to = pos
    end
    def unsp
        let_pos{ scan(/\\/) and before(/\s|\#/) and starTOK{ vws || unv } }
    end
    def unsp?; (unsp;true); end
    def vws
        scan(/[\r\n\f]/) and (moreinput;true)
    end
    def moreinput
        send($env_vars[:moreinput]) if $env_vars.key?(:moreinput);
    end

    def unv
        let_pos{
            scan(/[ \t]+/) or
            (after(/^|\n/) and (pod_comment or
                                 (scan(/\#/) and ((bracketed and panic("Can't use embedded comments in column 1")) or
                                                        scan(/.*/)) ))) or
            (scan(/\#/) and (bracketed or scan(/.*/)))
        }
    end

    def ident; scan(/[[:alpha:]]\w*/); end
    
    def pod_comment
        after(/^|\n/) and scan(/=/) and unsp? and
        (let_pos{ scan(/begin/) and wsp and id = ident and
                  scan(/.*?\n=/) and unsp? and scan(/end/) and wsp and scan(/#{id}.*/)
         } or
         scan(/.*/))
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
        scan_unitstopper
        wsp
        _match_from(b,{:statementlist=>_sl},:comp_unit)
    end

    def pblock
        b = pos
        l=s=nil
        tokQUES{ l= _lambda and s= signature }; bl= block or return false
        h={:block=>bl};_hkv(h,:lambda,l);_hkv(h,:signature,s)
        _match_from(b,h,:pblock)
    end

    def _lambda; scan(/->|<->/); end

    def block
        b=pos
        let_pos{ scan(/{/) and sl= statementlist and _block_rest and
                 _match_from(b,{:statementlist=>sl},:block) }
    end

    #R QUESTION regexp_block lacks block's \h*.  Intentional?
    def regex_block
        let_pos{ scan(/{/) and r= regex('}') and _block_rest and
                 _match_from(b,{:regex=>r},:regex_block) }
    end

    def _block_rest
        ( scan(/}/) or panic("Missing right brace") ) and
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
        starRULE{ statement }
    end

    def semilist
        starRULE{ statement }
    end

    def label
        let_pos{ id = ident and scan(/:\s/) and wsp }
        #R ...missing... bookkeeping - needed?
    end


    def _hkv(h,k,v)
        h[k] = v if v and (v.instance_of?(Array) ? (not v.empty?) : true)
    end

    def statement
        $env_vars.scope_enter(:endstmt)
        $env_vars[:endstmt] = -1;
        (label_ = control_ = expr_ = mod_loop_ = mod_cond_ =
         loopx_ = condx_ = mod_condloop_ = modexpr_ = nil)
        b = pos
        label_= starTOK{ label }
        ((control_= statement_control) or
         let_pos{ x= expect_term and expr_= _EXPR(x) and
           b1 = pos and
           ((before{ stdstopper }) or
            (let_pos{ mod_loop_= statement_mod_loop and loopx= _EXPR }) or 
            (let_pos{ mod_cond_= statement_mod_cond and condx= _EXPR and
               (before{ stdstopper } or
                mod_condloop_= statement_mod_loop and loopx= _EXPR )}) )
             modexpr_= _match_from(b1,{},:statement__modexpr)
         } or
         before(/;/)) and
        eat_terminator or return false
        $env_vars.scope_leave
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

    def eat_terminator
        ( scan(/;/) or
          ($env_vars[:endstmt] == ws_from) or 
          before{ terminator } or
          @scanner.eos? or #R added QUESTION: what's the right thing?
          panic("Statement not terminated properly"))
    end

    #R XXX Do these need trailing wsp?  Current use is INCONSISTENT.
    #R XXX Should the get leading wsp by default?  Are they?
    #R XXX   STD.pm suggests they not have it?
    def_rules_rest :statement_control,%w{ use no },%q{
      e=nil
      wsp;
      mn = module_name and wsp and (e=_EXPR; wsp) and eat_terminator and
      (h={:module_name=>mn};_hkv(h,:EXPR,e);_match_from(start,h,:'statement_control:<sym>'))
    }
    def_rules_rest :statement_control,%w{ if }, %q{
      e=_EXPR and wsp and pb=pblock and wsp and
      ei=starRULE{ b1=pos; scan(/elsif/) and wsp and e1=_EXPR and wsp and pb1=pblock and
                   _match_from(b1,{:elsif_expr=>e1,:elsif_block=>pb1},:elsif) } and
      el=quesRULE{ b1=pos; scan(/else/) and wsp and pb1=pblock and
                   _match_from(b1,{:pblock=>pb1},:if__else) } and
      (h={:if_expr=>e,:if_block=>pb,:elsif=>ei};_hkv(h,:else,el);
       _match_from(start,h,:'statement_control:if'))
    }

    def_rules_rest :statement_control,%w{ unless while until  for given when },%q{
      e=_EXPR and wsp and pb=pblock and
      _match_from(start,{:expr=>e,:block=>pb},:'statement_control:<sym>')
    }
    def_rules_rest :statement_control,%w{ repeat },%q{
      ((wu= scan(/while|until/) and wsp and e=_EXPR and wsp and bk=block and wsp and
        _match_from(start,{'0'=>wu,:wu_expr=>e,:wu_block=>bk},:'statement_control:repeat')) or
       (bk=block and wsp and wu=scan(/while|until/) and wsp and e=_EXPR and wsp and
        _match_from(start,{'0'=>wu,:expr_wu=>e,:block_wu=>bk},:'statement_control:repeat')))
    }
    def_rules_rest :statement_control,%w{ loop },%q{
      ((scan(/\(/) and wsp and
        e1= _EXPR and wsp and scan(/;/) and wsp and
        e2= _EXPR and wsp and scan(/;/) and wsp and
        e3= _EXPR and wsp and scan(/\)/) and wsp and
        (h={};_hkv(h,:loop_e1,e1);_hkv(h,:loop_e2,e2);_hkv(h,:loop_e3,e3);
         eee= _match_from(start,h,:loop__eee)
       );true) and
       bk= block and 
       (h={:loop_block=>bk};_hkv(h,:loop_eee,eee);
        _match_from(start,h,:'statement_control:loop')))
    }

    def_rules_rest :statement_control,%w{
      default BEGIN CHECK INIT END START ENTER LEAVE KEEP UNDO FIRST NEXT LAST
      PRE POST CATCH CONTROL },
      %q{
        bk=block and wsp and
        _match_from(start,{:block=>bk},:'statement_control:<sym>')
      }

    def modifier_expr; wsp and e=_EXPR and wsp and e; end
    def_rules_rest :statement_mod_cond,%w{ if unless when },%q{
      me=modifier_expr and
      _match_from(start,{:mod_<sym>=>me},:'statement_mod_cond:<sym>')
    }
    def_rules_rest :statement_mod_loop,%w{ while until for given },%q{
      me=modifier_expr and
      _match_from(start,{:mod_<sym>=>me},:'statement_mod_loop:<sym>')
    }
    
    def_token_full :module_name,false,'normal',/(?=\w)/,%q{
      (n=name and na= starTOK{ colonpair }) and
      (h={:name=>n};_hkv(h,:colonpair,na)
       _match_from(start,h,:'module_name:normal'))
    }
    def_token_full :module_name,false,'depreciated',/(?=v6-alpha)/,%q{
      scan(/v6-alpha/) and
       _match_from(start,{},:'module_name:depreciated')
    }

    def whatever; scan(/\*/); end

    def_tokens_rest :version,false,%w{ v },%q{ scan(/ \d+ ( \. (\d+ | \*) )* \+?/x) }

    ###################################################

    #R YYY downward sync reached here

    def expect_term
        b = pos
        # queue up the prefixes to interleave with postfixes
        pre = starTOK lambda{
            m = _match_from(pos)
            if prefix_ = prefix
                m[:prec] = prefix_[:prec]
            elsif precircum = prefix_circumfix_meta_operator
                m[:prec] = precircum[:prec]
            else
                return false
            end
            # XXX assuming no precedence change
            starTOK{prefix_postfix_meta_operator}
            wsp
            m
        }

        noun_ = noun or return fail_at(b)

        # also queue up any postfixes, since adverbs could change things
        post= starTOK{expect_postfix}
        wsp
        quesTOK{adverbs}

        # now push ops over the noun according to precedence.
        #R    { make $¢.nounphrase(:noun($<noun>), :pre(@<pre>), :post(@<post>)) }
        nounphrase(noun_,pre,post)
        #R# return noun_ if postfix.empty? #R shorten tree
        #R# _match_from(b,{:noun=>noun_,:postfix=>postfix},:expect_term)
    end
    
    def nounphrase(nounS,preA,postA,*rest)
        nounphrase = nounS
        preS = preA.pop
        postS = postA.shift
        while pre or post
            oldterm = nounphrase
            if preS
                if postS and postS[:prec] > preS[:prec]
                    nonphrase = postS
                    postS = postA.shift
                else
                    nounphrase = preS
                    preS = preA.pop
                end
            else
                nounphrase = postS
                postS = postA.shift
            end
            nounphrase[:term] = oldterm
        end
        nounphrase
    end

    def adverbs
        plusTOK{ _cp = colonpair and (colonpair_ ||= []; colonpair_.push(_cp);true) and wsp } and
            ( prop = $env_vars[:prevop] or
              panic('No previous operator visible to adverbial pair ('+colonpair_+')');
              prop.adverb(colonpair_); true )
    end

    def noun
        (fatarrow || package_declarator || scope_declarator || plurality_declarator ||
         routine_declarator || regex_declarator || type_declarator || circumfix || subcall ||
         variable || value || capterm || sigterm || term || statement_prefix || colonpair)
    end
    
    def fatarrow
        let_pos{ (key = ident and
                  scan(/[ \t]*/) and
                  scan(/\=>/) and
                  val = _EXPR(nil,Hitem_assignment)) }
    end

    def colonpair
        scan(/:/) and (let_pos{ scan(/!/) and ident } or
                       (ident and (unsp; postcircumfix;true)) or
                       postcircumfix or
                       let_pos{ sigil and (twigil;true) and desigilname })
    end

    def quotepair
        scan(/:/) and (let_pos{ scan(/!/) and ident } or
                       (ident and (unsp; before(/\(/) and postcircumfix;true)) or
                       scan(/\d+[a-z]+/))
    end

    def expect_tight_infix(loosest)
        let_pos {
            (not (before(/\{/) or _lambda)) and expect_infix and ($env_vars[:thisop][:prec] > loosest or parsefail)
        }
    end

    def expect_infix
        ((i = infix) && starTOK{infix_postfix_meta_operator} && i) || #R XXX
            infix_prefix_meta_operator || infix_circumfix_meta_operator
    end

    def_tokens_rest :dotty,false,%w{ .+ .* .? .= .^ .: },%q{ methodop }
    def_tokens_rest :dotty,false,%w{ . },%q{ dottyop }
    def dottyop; methodop or postop; end

    def expect_postfix
        # last whitespace didn't end here (or was zero width)
        (pos != ws_to or ws_to == ws_from) and
            (scan(/\\(?=\.)/)) or
            unsp? and 
            starTOK{quesTOK{scan(/\./) and unsp?} and postfix_prefix_meta_operator and unsp?} and
            (dotty or postop_ = postop) and (xXXX[:prec] = postop_[:prec]) #R XXX ?
    end

    #R XXX TODO I currently don't understand the [LIST] part.  And dont support it.

    # Note: backtracks, or we'd never get to parse [LIST] on seeing [+ and such.
    # (Also backtracks if on \op when no \op infix exists.)
    def_token_full :prefix_circumfix_meta_operator,false,'reduce',/\[/,%q{
       b = pos
       scan(/\[/) or return false
       $env_vars.scope_enter(:thisop)
       #starNgRX(proc{ scan(/\\/) }){ expect_infix and scan(/\]/) }
       (let_pos{ expect_infix and scan(/\]/) } or 
        (scan(/\\\\/) and expect_infix and scan(/\]/)) or
        ($env_vars.scope_leave;return fail_at(b)))

       ((not $env_vars[:thisop][:assoc] == 'non') or
        panic("Can't reduce a non-associative operator"))
       ((not $env_vars[:thisop][:prec] == Hconditional[:prec]) or
        panic("Can't reduce a conditional operator"))

       xXXX[:prec] = $env_vars[:thisop][:prec]

       $env_vars.scope_leave
    }

    def_tokens_simple :prefix_postfix_meta_operator,false,%w{ « }
    def_tokens_simple :prefix_postfix_meta_operator,false,%w{ << }
    def_tokens_simple :postfix_prefix_meta_operator,false,%w{ » }
    def_tokens_simple :postfix_prefix_meta_operator,false,%w{ >> }

    def_tokens_rest :infix_prefix_meta_operator,:chaining,%w{ ! },%q{lex1(:negation) and let_pos{ (not before(/!/)) and infix } and (($env_vars[:thisop][:assoc] == :chain) or ($env_vars[:thisop][:assoc] and $env_vars[:thisop][:bool]) or panic("Only boolean infix operators may be negated")) and ( $env_vars[:thisop][:hyper] and panic("Negation of hyper operator not allowed");true)}

    def lex1(s)
        $env_vars[:thisop][s] and panic("Nested #{s} metaoperators not allowed")
        true
    end

    #R sym<X X>, fyi
    def_tokens_rest :infix_circumfix_meta_operator,:list_infix,%w{ X },%q{ lex1(:cross) and infix and scan(/X/) }

    #R NONSPEC - multiple tokens, rather than a single one
    #R  Can fix once we discard "hash model" of token parsing.
    def self._icmo_brackets(left,right)
        eval("def_tokens_rest :infix_circumfix_meta_operator,:hyper,%w{ #{left} },%q{ lex1(:hyper) and infix and scan(/#{right}/) }")
    end
    _icmo_brackets('«','»')
    _icmo_brackets('«','«')
    _icmo_brackets('»','»')
    _icmo_brackets('»','«')
    _icmo_brackets('<<','>>')
    _icmo_brackets('<<','<<')
    _icmo_brackets('>>','>>')
    _icmo_brackets('>>','<<')

    def_tokens_rest :infix_postfix_meta_operator,:item_assignment,%w{ = },%q{ lex1(:assignment) and (($env_vars[:thisop][:prec] > item_assignmentH[:prec]) or panic("Can't make assignment op of operator looser than assignment")) and ((not $env_vars[:thisop][:assoc] == :chain) or panic("Can't make assignment op of boolean operator")) and ((not $env_vars[:thisop][:assoc] == :non) or panic("Can't make assignment op of non-associative operator")) }
    
    def_tokens_rest :postcircumfix,:methodcall,%w{ ( },%q{ semilist and scan(/\)/) }
    def_tokens_rest :postcircumfix,:methodcall,%w{ [ },%q{ semilist and scan(/\]/) }
    def_tokens_rest :postcircumfix,:methodcall,%w{ \{ },%q{ semilist and scan(/\}/) }
    def_tokens_rest :postcircumfix,:methodcall,%w{ < },%q{ anglewords('>') and scan(/>/) }
    def_tokens_rest :postcircumfix,:methodcall,%w{ << },%q{ shellwords('>>') and scan(/>>/) }
    def_tokens_rest :postcircumfix,:methodcall,%w{ « },%q{ shellwords('»') and scan(/»/) }
    
    def postop
        ((op = postfix and (xXXX[:prec] = op[:prec])) or
         (op = postcircumfix and (xXXX[:prec] = op[:prec])))
    end

    def methodop
        ((ident or
          (before(/\$|\@/) and variable) or
          (before(/[\'\"]/) and q = quote and (q =~ /\W/ or panic("Useless use of quotes")))) and
         unsp? and
         (let_pos{ scan(/\./); unsp; scan(/\(/) and semilist and scan(/\)/) } or
          let_pos{ scan(/\:/) and before(/\s/) and (not $env_vars[:inqoute]) and arglist } or
          null))
    end

    def arglist
        $env_vars.scope_enter(:endargs)
        $env_vars[:endargs] = false #R ??? XXX "0" or "false"?
        v = _EXPR(nil,Hlist_prefix)
        $env_vars.scope_leave
        v
    end

    def anglewords(stop)
        wsp and starTOK{ (not before(/#{stop}/)) and scan(/./) } # XXX need to split
    end

    def shellwords(stop)
        wsp and starTOK{ (not before(/#{stop}/)) and scan(/./) } # XXX need to split
    end

    #R inlined lambda in re
    def_token_full :circumfix,:term,'{ }',/(?=\{|->|<->)/,%q{ pblock }

    def variable_decl
        (var = variable and ( xXXX[:sigil] = var[:sigil] ) and
         quesTOK{
             %w{ @ % }.member?(var[:sigil]) and
             wsp and
             before(/[\<\(\[\{]/) and
             postcircumfix
         } and
         starTOK{trait} and
         wsp and
         quesTOK{
             ((scan(/\=/) and wsp and _EXPR(nil,var[:sigil] == '$' ? Hitem_assignment : Hlist_prefix)) or
              (scan(/\.\=/) and wsp and _EXPR(nil,Hitem_assignment)))
         })
    end

    def scoped #rule
        regex_declarator or package_declarator or
            #R XXX not backtracking properly - don't know if it needs to yet
            (let_pos{ starRULE{typename} and 
                 ( variable_decl or
                   (scan(/\(/) and signature and scan(/\)/) and starRULE{trait})
                   plurality_declarator or
                   routine_declarator or
                   type_declarator) })
    end
    def_tokens_rest :scope_declarator,false,%w{ my our state constant has },%q{ scoped }
    def_tokens_rest :package_declarator,false,%w{ class grammar module role package },%q{b=pos; pd=package_def and _match_from(b,{:package_def=>pd},:<sym>) } #end;end
    def_tokens_rest :package_declarator,false,%w{ require },%q{ module_name and (_EXPR;true) }
    def_tokens_rest :package_declarator,false,%w{ trusts },%q{ module_name }

    #R added a .ws between module_name and block, and before module_name. XXX
    def package_def
        let_pos{
            (wsp and
             (mn = quesRULE{module_name} and wsp and starRULE{trait} and wsp and
              (let_pos{ $env_vars[:begin_compunit] and scan(/;/) and wsp and
                (mn.bool or panic("Compilation unit cannot be anonymous")) and
                ($env_vars[:begin_compunit] = false
                 true)} or
               (block and wsp))))
        }
    end

    def pluralized #R rule XXX rule-ness is currently ignored
        (variable_decl or
         let_pos{ scan(/\(/) and signature and scan(/\)/) and ruleSTAR{trait} } or
         package_declarator or
         routine_declarator or
         regex_declarator or
         type_declarator)
    end

    def_tokens_rest :plurality_declarator,false,%w{ multi proto only },%q{ pluralized }
    def_tokens_rest :routine_declarator,false,%w{ sub },%q{ routine_def }
    def_tokens_rest :routine_declarator,false,%w{ method submethod },%q{ method_def }
    def_tokens_rest :routine_declarator,false,%w{ macro },%q{ macro_def }
    def_tokens_rest :regex_declarator,false,%w{ regex token rule },%q{ regex_def }


    # Most of these special variable rules are there simply to catch old p5 brainos
    #R so most are ignored here.
    #R only $! and $¢ are non-obs?  Ignore them all for now.
    #R special_variables: ignored.


    # desigilname should only follow a sigil/twigil

    def desigilname
        ((before(/\$/) and variable) or
         (name))
    end
    
    def variable
        (special_variable or
         let_pos{ si = sigil and (tw = twigil;true) and
             ((si == '&' and (sln = sublongname or return false;sln)) or desigilname) and
             ((tw == '.' and unsp? and before(/\(/) and postcircumfix) or
              null)} or
         let_pos{ sigil and scan(/\d+/) } or
         # Note: $() can also parse as contextualizer in an expression; should have same effect
         let_pos{ sigil and before(/[<\(]/) and postcircumfix } or
         # Note: any ::() are handled within <name>, and subscript must be final part.
         # A bare ::($foo) is not considered a variable, but ::($foo)::<$bar> is.
         # (The point being that we want a sigil either first or last but not both.)
         #= FOO::<$x>
         let_pos{ name and scan(/::/) and before(/[\<\«\{]/) and postcircumfix }
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
        (let_pos{category and plusTOK{colonpair}} or
         desigilname)
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
        let_pos{ n = name and is_type(n.to_s) and 
            # parametric type?
            unsp? and quesTOK{ before(/\[/) and postcircumfix }
        }
    end

    def fulltypename #R regex XXX
        typename and
            quesRX{ wsp and scan(/of/) and wsp and fulltypename }
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
    def dec_number; scan(/\d+(?:_\d+)* (?: \. \d+(?:_\d+)* (?: [Ee] [+\-]? \d+ )? )/x); end #R QUESTION why outer bracket?
    def rad_number
        let_pos{ scan(/:/) and radix_ = scan(/\d+/) and unsp? } and
            ( ( scan(/</) and intpart = scan(/[0-9a-zA-Z]+/) and (fracpart = scan(/\.[0-9a-zA-Z]+/);true) and ( scan(/\*/) and base = radint and scan(/\*\*/) and exp = radint;true)) or
              ( before(/\[/) and postcircumfix ) or
              ( before(/\(/) and postcircumfix ))
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
        scan(/^[ \t]*?/) and eat($env_vars[:delim]) and scan(/[ \t]*$\n?/)
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
        def_token_full :quote,false,name,Regexp.new(Regexp.quote(left_sym))," qutoesnabber(#{args})"
    end
    def_quote "' '"  ,%q{':q'}
    def_quote '" "'  ,%q{':qq'}
    def_quote '« »'  ,%q{':qq',':ww'}
    def_quote '<< >>',%q{':qq',':ww'}
    def_quote '< >'  ,%q{':q',':w'}
    def_quote '/ /'  ,%q{':regex'}

    # handle composite forms like qww
    def_tokens_rest :quote,false,%w{ qq q },%q{ qm = quote_mod and quotesnabber(':<sym>',qm) }

    def_tokens_simple :quote_mod,false,%w{ w ww x to s a h f c b }

    def_tokens_rest :quote,false,%w{ rx m },%q{ quotesnabber(':regex') }
    def_tokens_rest :quote,false,%w{ mm },%q{ quotesnabber(':regex', ':s') }
    def_tokens_rest :quote,false,%w{ s },%q{ pat=quotesnabber(':regex') and finish_subst(pat) }
    def_tokens_rest :quote,false,%w{ ss },%q{ pat=quotesnabber(':regex', ':s') and finish_subst(pat) }
    def_tokens_rest :quote,false,%w{ tr },%q{ pat=quotesnabber(':trans') and finish_subst(pat) }

    def finish_subst(pat)
        $env_vars.scope_enter(:thisop)
        u =(
            # bracketed form
            (pat[:delim] == 2 and ((wsp and infix and
                                    ($env_vars[:thisop][:prec] == Hitem_assignment[:prec] or
                                     panic("Bracketed subst must use some form of assignment")) and
                                    repl=_EXPR(nil,Hitem_assignment)) or :failed)) or
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
            super(*a)
            @escapes = []
            @escapes.push('\\') if @option[:b]
            @escapes.push('$') if @option[:s]
            @escapes.push('@') if @option[:a]
            @escapes.push('%') if @option[:h]
            @escapes.push('&') if @option[:f]
            @escapes.push('{') if @option[:c]
        end
        def self.create(pedigree)
            q = QLang.new(nil,nil,nil,nil)
            base = pedigree.shift
            eval("init_root_of_#{base}")
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



    def quotesnabber (*qA)
        $env_vars.scope_enter(:delim)
        $env_vars[:delim] = '' 
        v = ((not before(/\w/)) and nofat and #R XX? ::
         wsp and
         starTOK{ q = quotepair and qA.push(q) and wsp } and
         # Dispatch to current lang's subparser.
         ( lang = qlang('Q',qA) and false ) #R XXX I don't understand this yet.
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
            panic("Whitespace not allowed as delimiter");
        # XXX not defined yet
        #    <?before <+isPe> > {
        #        self.panic("Use a closing delimiter for an opener is reserved");
        #    }
        elsif start = scan(/(.)\1*/) #R XXX huh?
            char = s.match(/(.)\1*/)[1]
            #R perhaps R should panic() rather than die().
            rightbrack = Open2close[char] or raise "No matching close delimiter";
            stop = rightbrack * start.length;
            return start, stop;
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
        scan(start) or return false
        while not before(stop)
            triple = /#{stop}#{stop}#{stop}/
            if before(triple)
                # XXX triple rule should just be in escapes to be customizable
                dequote_ = _EXPR(nil,HLOOSEST,triple) or return fail_at(b)
            elsif before(start)
                subtext_ = q_balanced(lang, start, stop, esc) or fail_at(b)
            elsif before(esc)
                e=q_escape(lang) or return fail_at(b)
            else
                scan(/./m) or break
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
                scan(/./m) or break
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
                scan(/./m) or break
            end
        end
        scan(stop) or return fail_at(b)
        _match_from(b,{},'q_unbalanced')
    end


    # We get here only for escapes in escape set, even though more are defined.
    def q_escape (lang)
        lang[:escrule].(self)
    end


    def quote_escapes
        (lex_pos{scan(/\\/) and qq_backslash} or
         (before(/\{/) and block) or
         (before(/$/) and variable and (extrapost;true)) or
         lex_pos{ variable and extrapost } or
         scan(/.|\n/))
    end

    # Note, backtracks!  So expect_postfix mustn't commit to anything permanent.
    def extrapost
        $env_vars.scope_enter(:inquote)
        $env_vars[:inquote] = 1
        starRX(proc{ expect_postfix }){ after(/[\]\}\>\)]/) }
        $env_vars.scope_leave
    end

    def multisig
        rul{ scan(/:?\(/) and wsp and signature and wsp and starRULE{ scan(/\|/) and wsp and scan(/:?\(/) and wsp and signature and wsp and scan(/\)/) } }
    end

    def routine_def
        rul{ quesRULE{ident} and wsp and 
            quesRULE{multisig} and wsp and
            starRULE{trait} and wsp and
            block }
    end

    def method_def
        rul{ (let_pos{ident and wsp and quesRULE{multisig}} or
              let_pos{before{sigil and scan(/\.[\[\{\(]/)} and sigil and postcircumfix }) and wsp and
            starRULE{trait} and wsp and
            block }
    end

    def regex_def
        rul{ quesRULE{ident} and wsp and
            starRULE{trait} and wsp and
            quesRULE{ scan(/:?\(/) and wsp and signature and wsp and scan(/\)/) } and wsp and
            regex_block }
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

    def_rules_rest :trait_verb,%w{ is },%q{ ident and ws_dot and (postcircumfix;true) }
    def_rules_rest :trait_verb,%w{ will },%q{ ident and ws_dot and block }
    def_rules_rest :trait_verb,%w{ of returns },%q{ fulltypename }
    def_rules_rest :trait_verb,%w{ handles },%q{ _EXPR }

    def capterm
        let_pos{ scan(/\\\(/) and capture and scan(/\)/) }
    end

    def capture #R rule
        _EXPR
    end

    def sigterm
        let_pos{ scan(/:\(/) and signature and scan(/\)/) }
    end

    def signature
        $env_vars.scope_enter(:zone)
        $env_vars[:zone] = 'posreq'
        v = rul{
            parsep = starRULE{ parameter and wsp and (scan(/,|:|;;|;/) or before(/-->|\)|\{/)) }
            wsp
            parsep and quesRULE{ scan(/-->/) and wsp and fulltypename }
        }
        $env_vars.scope_leave
        v
    end

    def_rules_rest :type_declarator,%w{ subset },%q{ name and wsp and quesRULE{ scan(/of/) and wsp and fulltypename } and scan(/where/) and wsp and _EXPR }

    def type_constraint
        rul{ value or
            (scan(/where/) and _EXPR(nil,Hchaining)) }
    end

    def post_constraint
        rul{ multisig or
            (scan(/where/) and _EXPR(nil,Hchaining)) }
    end

    def param_var
        let_pos{
            (sigil and (twigil;true) and
             (# Is it a longname declaration?
              (let_pos{ xXXX[:sigil] == '&' and ident } and
               (ident_=sublongname or return false;ident_)) or
              # Is it a shaped array or hash declaration?
              (let_pos{ (xXXX[:sigil] == '@' or xXXX[:sigil] == '%')  and
                   (ident;true) and wsp and before(/[\<\(\[\{]/) and
                   postcircumfix }) or
              # ordinary parameter name
              (ident) or
              # bare sigil?          
              (null))) }
    end

    def parameter
        quantS = nil
        v = (starTOK{ type_constraint } and 
             (let_pos{ quantchar_ = scan(/\*/) and pv = param_var and slurp_=[quantchar_,pv] and quantS = '*' } or
              ((let_pos{ quantchar_ = scan(/:/) and
                    (let_pos{ name_ = ident and scan(/\(/) and param_var and scan(/\)/) } or
                     let_pos{ pv = param_var and name_ = pv[:ident] }) and
                    quantS = '*' and
                    named_ = true #R ...
                } or
                let_pos{ param_var and quantS = '!' }) and
               quanchar_ = scan(/[\?!]/) and quantS = quantchar_ )) and
             quantSTAR{ trait } and
             quantSTAR{ post_constraint } and
             quesTOK{
                 (default_value and
                  (case quantchar_
                   when '!'
                       panic("Can't put a default on a required parameter")
                   when '*'
                       panic("Can't put a default on a slurpy parameter")
                   else
                       raise 'bug?'
                   end
                   quantS = '?'))
             })
        return v if not v
        # enforce zone constraints
        case quantS
        when '!'
            case $env_vars[:zone]
            when :posopt
                panic("Can't use required parameter in optional zone")
            when :var
                panic("Can't use required parameter in variadic zone")
            else
                raise 'bug?'
            end
        when '?'
            case $env_vars[:zone]
            when :posreq
                $env_vars[:zone] = :posopt
            when :var
                panic("Can't use optional positional parameter in variadic zone")
            else
                raise 'bug?'
            end
        when '*'
            $env_vars[:zone] = :var
        else
            raise 'bug?'
        end
        v
    end

    def default_value
        rul{ scan(/\=/) and _EXPR(nil,Hitem_assignment) }
    end

    def_tokens_rest :statement_prefix,false,%w{ do try gather contend async lazy },%q{ statement }


    ## term
    def_tokens_rest :term,:term,%w{ undef },%q{ scan(/undef/) and scan(/[ \t]*/) and nofat }
    def_tokens_simple :term,:term,%w{ self * }

    def self.sigil_speed_hack_re; /\$|@@|@|%|&|::/; end
    def_token_full :circumfix,:term,'sigil',sigil_speed_hack_re,%q{ scan(/\(/) and semilist and scan(/\)/) }

    def_token_full :circumfix,:term,'typename',//,%q{ typename and scan(/\(/) and semilist and scan(/\)/) }

    def_tokens_rest :circumfix,:term,%w{ ( },%q{let_pos{ t = statementlist and scan(/\)/) and t }}
    def_tokens_rest :circumfix,:term,%w{ [ },%q{let_pos{ t = statementlist and scan(/\]/) and t }}
    def_tokens_rest :circumfix,:term,%w{ < },%q{let_pos{ t = anglewords('>') and scan(/>/) and t }}
    def_tokens_rest :circumfix,:term,%w{ << },%q{let_pos{ t = shellwords('>>') and scan(/>>/) and t }}
    def_tokens_rest :circumfix,:term,%w{ « },%q{let_pos{ t = shellwords('»') and scan(/»/) and t }}
    def_tokens_simple :infix,:methodcall,%w{ . }
    def_tokens_simple :postfix,:methodcall,%w{ -> }
    def_tokens_simple :postfix,:autoincrement,%w{ ++ -- i }
    def_tokens_simple :prefix,:autoincrement,%w{ ++ -- }
    def_tokens_simple :infix,:exponentiation,%w{ ** }
    def_tokens_simple :prefix,:symbolic_unary,%w{ ! + - ~ ? = * ** ~^ +^ ?^ ^ | }
    def_tokens_simple :infix,:multiplicative,%w{ * / % +& +< << >> +> ~&> ~< ~> }
    def_tokens_simple :infix,:additive,%w{ + - +| +^ ~| ~^ ?| ?^ }
    def_tokens_simple :infix,:replication,%w{ x xx }
    def_tokens_simple :infix,:concatenation,%w{ ~ }
    def_tokens_simple :infix,:junctive_and,%w{ & }
    def_tokens_simple :infix,:junctive_or,%w{ | ^ }
    def_tokens_simple :prefix,:named_unary,%w{ rand sleep abs }
    def_tokens_simple :infix,:nonchaining,%w{ <=> cmp is but does .. ^.. ..^ ^..^ ff ^ff ff^ ^ff^ fff ^fff fff^ ^fff^ }
    def_tokens_simple :infix,:chaining,%w{ == != < <= > >= ~~ !~ =~ eq ne lt le gt ge =:= === }
    def_tokens_simple :infix,:tight_and,%w{ && }
    def_tokens_simple :infix,:tight_or,%w{ || // }
    def_tokens_rest :infix,:tight_or,%w{ ^^ },%q{ xXXX[:assoc] = :list }


    ## conditional
    def_token_full :infix,:conditional,'?? !!',/\?\?/,%q{ _EXPR(nil,Hconditional) }
    #R assorted cautionary panics left out

    ## assignment
    def_tokens_rest :infix,false,%w{ = },%q{ xXXX[:sigil] == '$' ? make(xXXX,Hitem_assignment) : make(xXXX,Hlist_assignment) }

    def_tokens_simple :infix,:item_assignment,%w{ := ::= }
    # XXX need to do something to turn subcall into method call here...
    def_tokens_simple :infix,:item_assignment,%w{ .= }
    # Note, other assignment ops generated by infix_postfix_meta_operator rule

    def_tokens_simple :prefix,:loose_unary,%w{ true not }
    def_tokens_simple :infix,:comma,%w{ , p5=> }
    def_tokens_simple :infix,:list_infix,%w{ X Z minmax }

    #R QUESTION: STD says ... \s ... not \s+.  That can't be right, can it?
    def_token_full :term,:list_prefix,'sigil',sigil_speed_hack_re,%q{ scan(/\s+/) and arglist }
    def_token_full :term,:list_prefix,'typecast',//,%q{ typename and scan(/\s+/) and arglist }

    # unrecognized identifiers are assumed to be post-declared listops.
    # (XXX for cheating purposes this rule must be the last term: rule)
    def_tokens_rest :term,:list_prefix,[""],%q{ s=ident and (let_pos{ scan(/\s/) and nofat and a=arglist } or nofat) }
       
    def_tokens_simple :infix,:loose_and,%w{ and andthen }
    def_tokens_simple :infix,:loose_or,%w{ or xor orelse }
    def_tokens_before :terminator,:terminator,%w{ ; <== ==> --> ) ] \} !! }


    #R regex - ##Q why is this a regex?
    def stdstopper
        (@scanner.eos? ||
         terminator || statement_mod_cond || statement_mod_loop ||
         ((before(/{/) or before{ _lambda }) and after(/\s/)) ||
#R         cent.pos == env[:endstmt] ||
#R         cent.pos == env[:endargs]
#R         #    | <$+unitstopper> #R?
false #R
         )
    end

    def pop(a);a.pop;end
    def push(a,e);a.push(e);end
    def reverse(a);a.reverse;end
    def item(h);raise "what does item do?";end
    def _EXPR(*args); let_pos{ _EXPR_raw(*args) }; end
    def _EXPR_raw(seenS=false, preclimH=nil, stopS=nil, *fateA) #R Args reordered!
        hereS_workaround = self

        preclimH ||= HLOOSEST
        stopS ||= method(:stdstopper)

        $env_vars.scope_enter(:inquoteS,:prevopS,:thisopH);
        
        my preclimS = preclimH[:prec];
        $env_vars[:inquoteS] = 0
        #    my terminatorA = before(lambda{|s| stop(s) } );
        #    return () if not terminatorA.empty? and terminatorA[0].bool;
        termstackA = []
        opstackA = []

        #R push opstackA, termH;         # (just a sentinel value)
        push opstackA, {:prec=>"a="} #R kludge

        hereS = nil
        if seenS 
            hereS = seenS;
        else 
            my tA = [expect_term()];
            hereS = tA[0];
        end
        push termstackA, hereS;
        say "In EXPR, at ", hereS_workaround.pos;

        reduce = lambda {
            say "entering reduce, termstack == ", termstackA.length, " opstack == ", opstackA.length;
            my opS = pop(opstackA);
            case opS[:assoc] 
            when 'chain' 
                say "reducing chain";
                chainA = []
                push chainA, pop(termstackA);
                push chainA, opS;
                while not opstackA.empty? 
                    break if opS[:prec] != opstackA[-1][:prec];
                    push chainA, pop(termstackA);
                    push chainA, pop(opstackA)[:top];
                end
                push chainA, pop(termstackA);
                opS[:top][:chain] = reverse chainA;
                push termstackA, opS[:top];
            when 'list' 
                say "reducing list";
                listA = []
                push listA, pop(termstackA);
                while not opstackA.empty? 
                    break if opS[:top][:sym] != opstackA[-1][:top][:sym];
                    push listA, pop(termstackA);
                    pop(opstackA);
                end
                push listA, pop(termstackA);
                opS[:top][:list] = reverse listA;
                push termstackA, opS[:top];
            else
                say "reducing";
                listA = []
                say termstackA.length;

                #opS[:top][:right] = pop termstackA;
                #opS[:top][:left] = pop termstackA;

                my _opH = opS;   # XXX anti-pugs hack
                _opH[:top][:right] = pop termstackA;
                _opH[:top][:left] = pop termstackA;
                opS = _opH;

                push termstackA, opS[:top];
            end
        }

        while true 
            say "In while true, at ", hereS_workaround.pos;

            wsp #R added

            my terminatorA = [hereS_workaround.before{stopS.()}]
            my tS = terminatorA[0];
            break if tS and terminatorA[0].bool;
            $env_vars[:thisopH] = {}
            #        my infixA = [hereS_workaround.expect_tight_infix(preclimS)];
            my infixA = [hereS_workaround.expect_infix()];
            my infixS = infixA[0];
            hereS = infixS;
            wsp

            # XXX might want to allow this in a declaration though
            if not infixS;  hereS_workaround.panic("Can't have two terms in a row"); end

            if not $env_vars[:thisopH].key?(:prec) 
                say "No prec case in thisop!";
                $env_vars[:thisopH] = Hterminator;
            end
            thisprecS = $env_vars[:thisopH][:prec];
            # substitute precedence for listops
            $env_vars[:thisopH][:prec] = $env_vars[:thisopH][:sub] if $env_vars[:thisopH][:sub];
            
            # Does new infix (or terminator) force any reductions?
            while opstackA[-1][:prec] > thisprecS 
                reduce.();
            end
            
            # Not much point in reducing the sentinels...
            break if thisprecS < SLOOSEST;
            
            # Equal precedence, so use associativity to decide.
            if opstackA[-1][:prec] == thisprecS 
                case $env_vars[:thisopH][:assoc].to_s
                when 'non' ;   hereS_workaround.panic("\"#{infixS}\" is not associative")
                when 'left' ;  reduce.()   # reduce immediately
                when 'right';  # just shift
                when 'chain';  # just shift
                when 'list'                # if op differs reduce else shift
                    reduce.() if $env_vars[:thisopH][:top][:sym] != opstackA[-1][:top][:sym];
                else
                    hereS_workaround.panic("Unknown associativity \"#{$env_vars[:thisopH][:assoc]}\" for \"#{infixS}\"")
                end
            end
            push opstackA, $env_vars[:thisopH]  #R item($env_vars[:thisopH]); # ignore puzzling item()
            my terminatorA = [hereS_workaround.before{stopS.()}];
            if not terminatorA.empty? and terminatorA[0].bool 
                hereS_workaround.panic("#{infixS.perl()} is missing right term");
            end
            $env_vars[:thisopH] = {}

            my tA = [hereS_workaround.expect_term()];
            hereS = tA[0];
            push termstackA, hereS;
            say "after push: ", termstackA.length;
        end
        reduce.() while termstackA.length > 1;
        termstackA.length == 1 or hereS_workaround.panic("Internal operator parser error, termstack == #{termstackA.length}");
        $env_vars.scope_leave
        return termstackA[0];
    end
end


    #############################################3333
    ## Regex
    #############################################3333

class Regex < Perl

    def regex(stop)
        $env_vars.scope_enter(:stop)
        $env_vars[:stop] = stop
        v = rul{ regex_ordered_disjunction }
        $env_vars.scope_leave
        v
    end

    def regex_ordered_disjunction
        rul{scan(/(\|\|)?/) and wsp and
            interleaveRULE(/\|\|/){ regex_ordered_conjunction } }
    end

    def regex_ordered_conjunction
        rul{ interleaveRULE(/&&/){ regex_submatch } }
    end

    def regex_submatch
        rul{ interleaveRULE(/!?~~/){ regex_unordered_disjunction } }
    end

    def regex_unordered_disjunction
        rul{ quesRULE{ let_pos{ scan(/\|/) and not before(/\|/) } } and
            interleaveRULE(/\!(?!\!)/){ regex_unordered_conjunction } }
    end

    def regex_unordered_conjunction
        rul{ interleaveRULE(/\&(?!\&)/){ regex_sequence } }
    end

    def regex_sequence
        plusRULE{regex_quantified_atom}
    end

    def regex_quantified_atom
        rul{ ra = regex_atom and wsp and
            (regex_quantifier and
             (ra.max_width or
              panic("Can't quantify zero-width atom"));true) }
    end

    def regex_atom
        (wsp and
         ((let_pos{ scan(/#{$env_vars[:stop]}/) } and return(false);false) or
          (regex_metachar and wsp) or
          (scan(/\w/) and wsp) or
          panic("unrecognized metacharacter")))
    end

    def_tokens_rest :regex_metachar,false,%w{ > && & || | ] ) \\ },%q{ return false }

    regex_quantifier_re = /\*\*|\*|\+|\?/
    def_token_full :regex_metachar,false,'quant',regex_quantifier_re,%q{ panic("quantifier quantifies nothing") }

    # "normal" metachars
    def_token_full :regex_metachar,false,'{ }',/(?=\{)/,%q{ block }

    def_token_full :regex_metachar,false,'mod',//,%q{ regex_mod_internal }

    def_token_full :regex_metachar,false,'[ ]',/(?=\[)/,%q{ regex(']') and scan(/\]/) }
    def_token_full :regex_metachar,false,'( )',/(?=\()/,%q{ regex(')') and scan(/\)/) }

    def_tokens_simple :regex_metachar,false,%w{ <( )> << >> « » }

    def_token_full :regex_metachar,false,'qw',/(?=<\s)/,%q{ quote }

    def_token_full :regex_metachar,false,'< >',/\</,%q{ unsp; regex_assertion and scan(/\>/) }
    def_tokens_rest :regex_metachar,false,%w{ \\ },%q{ regex_backslash }
    def_tokens_simple :regex_metachar,false,%w{ . ^^ ^ }
    def_tokens_simple :regex_metachar,false,%w{ $$ }
    def_tokens_rest :regex_metachar,false,%w{ $ },%q{ before(/\s|\||\)|\]|\>/) }

    def_token_full :regex_metachar,false,"' '",/\'/,%q{ quotesnabber(":q") }
    def_token_full :regex_metachar,false,'" "',/\"/,%q{ quotesnabber(":qq") }

    def_token_full :regex_metachar,false,'var',/(?!\$\$)/,%q{ binding_=nil; sym=variable and wsp and (scan(/:=/) and wsp and binding_= regex_quantified_atom; true) }

    def codepoint
        scan(/\[(.*?)\]/)
    end

    def_tokens_before :q_backslash,false,%w{ qq },%q{ quote }
    def_tokens_rest :q_backslash,false,[""],%q{ scan(/./) }

    def_tokens_rest :qq_backslash,false,%w{ c },%q{ scan(/\[[^\]\n\r]*\]/) or codepoint }

    def_tokens_simple :qq_backslash,false,%w{ \\ a b e f n r t 0 }
    def_tokens_rest :qq_backslash,false,%w{ o },%q{ octint or (scan(/\[/) and octint and starTOK{ scan(/,/) and octint } and scan(/\]/)) }
    def_tokens_rest :qq_backslash,false,%w{ x },%q{ hexint or (scan(/\[/) and hexint and starTOK{ scan(/,/) and hexint } and scan(/\]/)) }

    def self.rx_bs(letter,rest=nil)
        letter = letter.to_s
        def_token_full :regex_backslash,false,letter,/(?i:#{letter})/,rest
    end
    %w{ a b d e f h n r t v w }.each{|letter| rx_bs letter }
    rx_bs :c,%q{ (scan(/\[ [^\]\n\r]* \]/x) or
                  codepoint) }
    rx_bs :o,%q{ octint or (scan(/\[/) and octint and starTOK{ scan(/,/) and octint} and scan(/\]/)) }
    rx_bs :x,%q{ hexint or (scan(/\[/) and hexint and starTOK{ scan(/,/) and hexint} and scan(/\]/)) }
    def_token_full :regex_backslash,false,'oops',//,%q{ panic("unrecognized regex backslash sequence") }

    def_tokens_rest :regex_assertion,false,%w{ ? ! },%q{ regex_assertion }

    def_token_full :regex_assertion,false,'{ }',/\{/,%q{ block }

    def_token_full :regex_assertion,false,'variable',/(?=#{sigil_speed_hack_re})/,%q{ before{ sigil } and _EXPR(nil,HLOOSEST,method(:assertstopper)) }
    def_token_full :regex_assertion,false,'method',/(?=\.(?!>))/,%q{ before(/\.(?!>)/) and _EXPR(nil,HLOOSEST,method(:assertstopper)) }
    def_token_full :regex_assertion,false,'ident',//,%q{ ident and quesTOK{
      (let_pos{ scan(/\=/) and regex_assertion} or
       let_pos{ scan(/\:/) and wsp and q_unbalanced(qlang('Q',':qq'), '>')} or
       let_pos{ scan(/\(/) and semilist and scan(/\)/)} or
       (wsp and _EXPR(nil,HLOOSEST,method(:assertstopper))) ) } }

    def_tokens_rest :regex_assertion,false,%w{ [ + - },%q{ before(/[\<sym>]/) and plusTOK{cclass_elem} }
    def_tokens_simple :regex_assertion,false,%w{ . , }
    def_tokens_rest :regex_assertion,false,%w{ ~~ },%q{ (desigilname;true) }

    def cclass_elem
        (scan(/[-+]?/) and 
         ((name) or
          (before(/\[/) and bracketed(QLang(:cclass)))))
    end


    def regex_mod_arg
        let_pos{ scan(/\(/) and semilist and scan(/\)/) }
    end

    def_token_full :regex_mod_internal,false,'adv',//,%q{ quotepair and true }
    #R XXX     <quotepair> { $/<sym> := «: $<quotepair><key>» }

    def_tokens_rest :regex_mod_internal,false,%w{ :i },%q{ quesTOK{regex_mod_arg} }
    def_tokens_simple :regex_mod_internal,false,%w{ :!i }

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
    def is_type(name); HTypenames[name]; end


    #def heredoc; false; end
    def method_missing(method, *args)
        print "FAKING #{method}\n"
        false
    end
end


#p Perl.new(('3')).expect_infix
#p Perl.new((')')).terminator
#p Perl.new(('2')).expect_term
#p Perl.new(('(2)')).circumfix
#p Perl.new(('2+3*4'))._EXPR
#Repl.new.expr
Repl.new.parser_rule

## vim: expandtab sw=4
## Local Variables:
## ruby-indent-level: 4
## End:
