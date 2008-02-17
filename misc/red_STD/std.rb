# A ruby transliteration of src/perl6/STD.pm
# See README.
#
# STD issues
#   multiple copies of postfix:++, etal
#   "use v6-alpha;" doesn't work, because module_name/name/ident can't have '-'.
#   "token circumfix:sym<{ }> ( --> Circumfix)", but there is no Circumfix.
#   "token package_def {" doesn't accept ws between module_name and block,
#      nor before module_name?
#

require 'prelude'

class Perl < Grammar
    attr_accessor :ws_from, :ws_to

    def _TOP; _UNIT( $env_vars[:unitstopper] || "_EOS" ); end

    prec_op(:hyper             ,{ :transparent =>1                           })
    prec_op(:term              ,{ :prec =>"z="                               })
    prec_op(:methodcall        ,{ :prec =>"y="                               })
    prec_op(:autoincrement     ,{ :prec =>"x="                               })
    prec_op(:exponentiation    ,{ :prec =>"w=", :assoc =>:right, :assign =>1 })
    prec_op(:symbolic_unary    ,{ :prec =>"v="                               })
    prec_op(:multiplicative    ,{ :prec =>"u=", :assoc =>:left,  :assign =>1 })
    prec_op(:additive          ,{ :prec =>"t=", :assoc =>:left,  :assign =>1 })
    prec_op(:replication       ,{ :prec =>"s=", :assoc =>:left,  :assign =>1 })
    prec_op(:concatenation     ,{ :prec =>"r=", :assoc =>:left,  :assign =>1 })
    prec_op(:junctive_and      ,{ :prec =>"q=", :assoc =>:list,  :assign =>1 })
    prec_op(:junctive_or       ,{ :prec =>"p=", :assoc =>:list,  :assign =>1 })
    prec_op(:named_unary       ,{ :prec =>"o=",                              })
    prec_op(:nonchaining       ,{ :prec =>"n=", :assoc =>:non                })
    prec_op(:chaining          ,{ :prec =>"m=", :assoc =>:chain, :bool =>1   })
    prec_op(:tight_and         ,{ :prec =>"l=", :assoc =>:left,  :assign =>1 })
    prec_op(:tight_or          ,{ :prec =>"k=", :assoc =>:left,  :assign =>1 })
    prec_op(:conditional       ,{ :prec =>"j=", :assoc =>:right,             })
    prec_op(:item_assignment   ,{ :prec =>"i=", :assoc =>:right              })
    prec_op(:loose_unary       ,{ :prec =>"h=",                              })
    prec_op(:comma             ,{ :prec =>"g=", :assoc =>:list,              })
    prec_op(:list_infix        ,{ :prec =>"f=", :assoc =>:list,  :assign =>1 })
    prec_op(:list_assignment   ,{ :prec =>"i=", :sub =>"e=", :assoc =>:right })
    prec_op(:list_prefix       ,{ :prec =>"e=",                              })
    prec_op(:loose_and         ,{ :prec =>"d=", :assoc =>:left,  :assign =>1 })
    prec_op(:loose_or          ,{ :prec =>"c=", :assoc =>:left,  :assign =>1 })
    prec_op(:LOOSEST           ,{ :prec =>"a=!",                             })
    prec_op(:terminator        ,{ :prec =>"a=", :assoc =>:list               })
    SLOOSEST = HLOOSEST[:prec]

    #R module PrecOp
    def precop_method(m,defaults)
        defaults.each{|k,v| m[k] = v if not m.key? k }
        if $env_vars[:thisopH]
            $env_vars[:thisopH][:top] = m;
            if not m.key?(:transparent)
                $env_vars[:thisopH][:prec] = m[:prec];
                $env_vars[:thisopH][:assoc] = m[:assoc];
            end
        end
        return m;
    end
    #R I'm unsure what make() should be doing.
    def make(m,overwrite)
        defaults.each{|k,v| m[k] = v }
    end

    #R things like "class Term does PrecOp[|%term] {}" are folded into
    #R prec_op above.

    $env_vars.scope_enter(:endsym,:unitstopper,:endstmt,:endargs)
    $env_vars[:endsym] = "null"
    $env_vars[:unitstopper] = "_EOS"
    $env_vars[:endstmt] = -1
    $env_vars[:endargs] = -1

    #R XXX TODO - the non-simple constraints are not being applied.
    proto_token :category
    proto_token :sigil
    proto_token :twigil
    proto_token :special_variable
    proto_token :version
    proto_token :term
    proto_token :quote
    proto_token :prefix, 'defequiv'=>:symbolic_unary
    proto_token :infix,  'defequiv'=>:additive
    proto_token :postfix,'defequiv'=>:autoincrement
    proto_token :dotty,  'endsym'=>' <.unsp>? '
    proto_token :circumfix
    proto_token :postcircumfix
    proto_token :regex_metachar
    proto_token :regex_backslash
    proto_token :regex_assertion
    proto_token :regex_mod_internal
    proto_token :quote_mod
    proto_token :q_backslash
    proto_token :qq_backslash
    proto_token :trait_verb,      'endsym'=>' \s+ <nofat> '
    proto_token :trait_auxiliary, 'endsym'=>' \s+ <nofat> '
    proto_token :type_declarator,    'gtgt_nofat'
    proto_token :scope_declarator,   'gtgt_nofat'
    proto_token :package_declarator, 'gtgt_nofat'
    proto_token :routine_declarator, 'gtgt_nofat'
    proto_rule  :statement_prefix,   'gtgt_nofat'
    proto_rule  :statement_control, 'endsym'=>' <nofat> <?before \s | \'#\'> '
    proto_rule  :statement_mod_cond, 'gtgt_nofat'
    proto_rule  :statement_mod_loop, 'gtgt_nofat'
    proto_token :infix_prefix_meta_operator
    proto_token :infix_postfix_meta_operator
    proto_token :infix_circumfixfix_meta_operator
    proto_token :postfix_prefix_meta_operator
    proto_token :prefix_postfix_meta_operator
    proto_token :prefix_circumfix_meta_operator
    #R added:
    proto_token :terminator
    proto_token :regex_quantifier
    proto_token :infix_circumfix_meta_operator
    proto_token :plurality_declarator
    proto_token :regex_declarator


    # Lexical routines

    #R QUESTION: why is this regex not token?
    def nofat
        # <!before \h* <.unsp>? '=>' >
        not let_pos{ (h = scan(/[ \t]*/); unsp; h) and scan(/=>/) }
    end
            
    def dot_ws
        pos == ws_to and return true
        after(/\w/) and before(/\w/) and return false
        ws_from = pos
        starTOK{ unsp || (seqTOK{scan(/\v/); heredoc}) || unv }
        ws_to = pos
    end
    def unsp; let_pos{ scan(/\\/) and before(/\s|\#/) and starTOK{ scan(/\v/) || unv } }; end
    def unsp?; (unsp;true); end
    def unv
        let_pos{
            scan(/[ \t]+/) or
            (after(/\n|\A/) and (pod_comment or
                                 (let_pos{ scan(/\#/) and ((bracketed and panic("Can't use embedded comments in column 1")) or
                                                        scan(/.*/)) } ))) or
            (scan(/\#/) and (bracketed or scan(/.*/)))
        }
    end

    def ident; scan(/[[:alpha:]]\w*/); end
    
    def pod_comment
        after(/^|\n/) and scan(/=/) and unsp? and
        ( (scan(/begin/) and dot_ws and id = ident and
            scan(/.*?\n=/) and unsp? and scan(/end/) and dot_ws and scan(/#{id}.*/)
           ) or
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
        dot_ws
        _sl = statementlist
        #R XXX this next line should apply unitstopper, not just look at it. :/
        $env_vars[:unitstopper] or panic("Can't understand next input--giving up")
        _match_from(b,{:statementlist =>_sl},:comp_unit)
    end

    def pblock
        b=l=s=nil
        let_pos{b=pos; l=_lambda and s=signature }; bl=block and h={:block=>bl};_hkv(h,:lambda,l);_hkv(h,:signature,s);_match_from(b,h,:pblock)
    end

    def _lambda; scan(/->|<->/); end

    def block
        let_pos{b=pos; scan(/{/) and sl = statementlist and _block_rest and _match_from(b,{:statementlist=>sl},:block) }
    end

    #R QUESTION regexp_block lacks block's \h*.  Intentional?
    def regex_block
        let_pos{ scan(/{/) and regex('}') and _block_rest }
    end

    def _block_rest
        ( scan(/}/) or panic("Missing right brace") ) and
            #R QUESTION <?before < ,: >> typo?
            ( (scan(/[ \t]*/) and unsp? and before(/[,:]/)) or
              (unv; before(/\n/) and dot_ws and ($env_vars[:endstmt] = ws_from)) or
              ($env_vars[:endargs] = pos) )
    end


    def statementlist
        starRULE{ statement }
    end

    def semilist
        starRULE{ statement }
    end

    def label
        let_pos{ id = ident and scan(/:\s/) and dot_ws }
        #R ...missing... bookkeeping - needed?
    end


    def _hkv(h,k,v)
        h[k] = v if v and (v.instance_of?(Array) ? (not v.empty?) : true)
    end

    def statement
        $env_vars.scope_enter(:endstmt)
        $env_vars[:endstmt] = -1;
        label_ = control_ = expr_ = mod_loop_ = mod_cond_ = loopx_ = condx_ = mod_condloop_ = nil
        dot_ws
        b = pos
        label_ = starRULE{ label }; dot_ws
        ((control_ = statement_control) or
         let_pos{ x = expect_term and dot_ws and expr_ = _EXPR(x) and dot_ws and
           ( before{ stdstopper } or
             let_pos{ mod_loop_ = statement_mod_loop and dot_ws and loopx = _EXPR } or 
             ( let_pos{ mod_cond_ = statement_mod_cond and dot_ws and condx = _EXPR } and
               (  before{ stdstopper } or
                  let_pos{ mod_condloop_ = statement_mod_loop and dot_ws and loopx = _EXPR } )))} or
          before(/;/)) and
        dot_ws and
        eat_terminator or return false
        dot_ws
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
        #R missing: modexpr, null
        m = _match_from(b,h,:statement)
    end

    def eat_terminator
        ( scan(/;/) or
          ($env_vars[:endstmt] == ws_from) or 
          before{ terminator } or
          @scanner.eos? or #R added QUESTION: what's the right thing?
          panic("Statement not terminated properly"))
    end

    def_category_rules :statement_control,%w{ use no },%q{let_pos{ e=nil; mn = module_name and dot_ws and (e=_EXPR; dot_ws) and eat_terminator and (h={:module_name =>mn};_hkv(h,:EXPR,e);_match_from(start,h,:<sym>)) }}
    def_category_rules(:statement_control,%w{ if }, %q{let_pos{ b=pos; e=_EXPR and dot_ws and pb=pblock and dot_ws and ei=starRULE{let_pos{ b1=pos; scan(/elsif/) and dot_ws and e1=_EXPR and dot_ws and pb1=pblock and _match_from(b1,{:elsif_expr =>e1,:elsif_block =>pb1},:elsif)}} and el=quesRULE{ scan(/else/) and dot_ws and pblock} and (h={:if_expr =>e,:if_block =>pb,:elsif =>ei};_hkv(h,:else,el);_match_from(b,h,:if)) }})

    def_category_rules :statement_control,%w{ unless while until  for given when },%q{let_pos{ _EXPR and dot_ws and pblock and dot_ws }}
    def_category_rules :statement_control,%w{ repeat },%q{ ((scan(/while|until/) and dot_ws and _EXPR and dot_ws and block and dot_ws) or (block and dot_ws and scan(/while|until/) and dot_ws and _EXPR and dot_ws))}
    def_category_rules :statement_control,%w{ loop },%q{ ((scan(/\(/) and dot_ws and e1= _EXPR and dot_ws and scan(/;/) and dot_ws and e2= _EXPR and dot_ws and scan(/;/) and dot_ws and e3= _EXPR and dot_ws and scan(/\)/) and dot_ws);true) and block and dot_ws}

    def_category_rules :statement_control,%w{ default BEGIN CHECK INIT END START ENTER LEAVE KEEP UNDO FIRST NEXT LAST PRE POST CATCH CONTROL },%q{let_pos{ block and dot_ws }}

    def modifier_expr; dot_ws and _EXPR and dot_ws; end
    def_category_rules :statement_mod_cond,%w{ if unless when },%q{let_pos{ modifier_expr and dot_ws }}
    def_category_rules :statement_mod_loop,%w{ while until for given },%q{let_pos{ modifier_expr and dot_ws }}
    
    def module_name
        b = pos
        n = name
        (n and na = starTOK{ colonpair }) or return false
        h = {:name =>n}
        _hkv(h,:colonpair,na)
        _match_from(b,h,:module_name)
    end

    def whatever; scan(/\*/); end

    def_tokens_rest :version,false,%w{ v },%q{ scan(/ \d+ ( \. (\d+ | \*) )* \+?/x) }

    ###################################################

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
            dot_ws
            m
        }

        noun_ = noun or return fail_at(b)

        # also queue up any postfixes, since adverbs could change things
        postfix = starTOK{expect_postfix}
        dot_ws
        quesTOK{adverbs}

        # now push ops over the noun according to precedence.
        #    { make $¢.nounphrase(:noun($<noun>), :pre(@<pre>), :post(@<post>)) }
        return noun_ if postfix.empty? #R shorten tree
        _match_from(b,{:noun=>noun_,:postfix=>postfix},:expect_term)
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
        plusTOK{ _cp = colonpair and (colonpair_ ||= []; colonpair_.push(_cp);true) and dot_ws } and
            ( prop = $env_vars[:prevop] or
              panic('No previous operator visible to adverbial pair ('+colonpair_+')');
              prop.adverb(colonpair_); true )
    end

    def noun
        (pair || package_declarator || scope_declarator || plurality_declarator ||
         routine_declarator || regex_declarator || type_declarator || circumfix ||
         variable || value || subcall || capterm || sigterm || term || statement_prefix)
    end
    
    def pair
        let_pos{ ((key = ident and
                   scan(/[ \t]*/) and
                   scan(/\=>/) and
                   val = _EXPR(nil,Hitem_assignment)) or
                  (plusTOK{ colonpair and dot_ws })) }
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

# # Note: backtracks, or we'd never get to parse [LIST] on seeing [+ and such.
# # (Also backtracks if on \op when no \op infix exists.)
# regex prefix_circumfix_meta_operator:reduce {
#     :my %thisop is context<rw>;
#     @<sym> = [ '[' \\?? ]   # prefer no meta \ if op has \
#     <expect_infix>
#     @<sym> = [ ']' ]
# 
#     [ <!{ %+thisop<assoc> eq 'non' }>
#         || <panic: Can't reduce a non-associative operator> ]
# 
#     [ <!{ %+thisop<prec> eq %conditional<prec> }>
#         || <panic: Can't reduce a conditional operator> ]
# 
#     { .<prec> := %+thisop<prec> }
# 
#     {*}                                                         #= [ ]
# }

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
    
    #R postfix ++ and -- duplicates ignored.  postfix:i moved.

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
        dot_ws and starTOK{ (not before(/#{stop}/)) and scan(/./) } # XXX need to split
    end

    def shellwords(stop)
        dot_ws and starTOK{ (not before(/#{stop}/)) and scan(/./) } # XXX need to split
    end

    
    #R XXX TODO another token which requires more powerful indexing.
    #R   <?before '{'> <block>
    def_tokens_rest :circumfix,false,%w{ \{ },%q{ @scanner.pos = pos() -1; block }

    def variable_decl
        (var = variable and ( xXXX[:sigil] = var[:sigil] ) and
         quesTOK{
             %w{ @ % }.member?(var[:sigil]) and
             dot_ws and
             before(/[\<\(\[\{]/) and
             postcircumfix
         } and
         starTOK{trait} and
         dot_ws and
         quesTOK{
             ((scan(/\=/) and dot_ws and _EXPR(nil,var[:sigil] == '$' ? Hitem_assignment : Hlist_prefix)) or
              (scan(/\.\=/) and dot_ws and _EXPR(nil,Hitem_assignment)))
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
            dot_ws
            (mn = quesTOK{module_name} and starTOK{trait} and dot_ws and
             (($env_vars[:begin_compunit] and scan(/;/) and
               (mn.bool or panic("Compilation unit cannot be anonymous")) and
               ($env_vars[:begin_compunit] = false
                true)) or
              (block)))
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
            quesRX{ dot_ws and scan(/of/) and dot_ws and fulltypename }
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




# our @herestub_queue;

# token q_herestub ($lang) {
#     $<delimstr> = <quotesnabber()>  # force raw semantics on /END/ marker
#     {
#         push @herestub_queue,
#             Herestub.new(
#                 delim => $<delimstr><delimited><q><text>, # XXX or some such
#                 orignode => $_,
#                 lang => $lang,
#             );
#     }
#     {*}
# }

# class Herestub {
#     has Str $.delim;
#     has $.orignode;
#     has $.lang;
# }

# token theredoc {
#     ^^ $<ws>=(\h*?) $+delim \h* $$ \n?
# }

# # XXX be sure to temporize @herestub_queue on reentry to new line of heredocs

# method heredoc () {
#     my $here = self;
#     while my $herestub = shift @herestub_queue {
#         my $delim is context = $herestub.delim;
#         my $lang = $herestub.lang;
#         my $doc;
#         my $ws = "";
#         $here = $here.q_unbalanced_rule($lang, :stop(&theredoc)).MATCHIFY;
#         if $here {
#             if $ws {
#                 my $wsequiv = $ws;
#                 $wsequiv ~~ s/^ (\t+) /{ ' ' x ($0 * 8) }/; # per spec
#                 $here<text>[0] ~~ s/^/\n/; # so we don't match ^^ after escapes
#                 for @($here<text>) {
#                     s:g[\n ($ws || \h*)] = do {
#                         my $white = $0;
#                         if $white eq $ws {
#                             '';
#                         }
#                         else {
#                             $white ~~ s[^ (\t+) ] = do {
#                                 ' ' x ($0.chars * (COMPILING::<$?TABSTOP> // 8))
#                             };
#                             $white ~~ s/^ $wsequiv //
#                                 ?? $white
#                                 !! '';
#                         }
#                     }
#                 }
#                 $here<text>[0] ~~ s/^ \n //;
#             }
#             $herestub.orignode<doc> = $here;
#         }
#         else {
#             self.panic("Ending delimiter $delim not found");
#         }
#     }
#     return $here;
# }



# token quote:sym<' '>   { <?before "'"  > <quotesnabber(":q")>        }
# token quote:sym<" ">   { <?before '"'  > <quotesnabber(":qq")>       }
# token quote:sym<« »>   { <?before '«'  > <quotesnabber(":qq",":ww")> }
# token quote:sym«<< >>» { <?before '<<' > <quotesnabber(":qq",":ww")> }
# token quote:sym«< >»   { <?before '<'  > <quotesnabber(":q", ":w")>  }

# token quote:sym</ />   {
#     <?before '/'  > <quotesnabber(":regex")>
#     [ (< i g s m x c e ] >+) 
#         # note: only the submatch fails here on the obs call
#         [ $0 ~~ 'i' <obs("/i",":i")> ]?
#         [ $0 ~~ 'g' <obs("/g",":g")> ]?
#         [ $0 ~~ 's' <obs("/s","^^ and $$ anchors")> ]?
#         [ $0 ~~ 'm' <obs("/m",". or \N")> ]?
#         [ $0 ~~ 'x' <obs("/x","normal default whitespace")> ]?
#         [ $0 ~~ 'c' <obs("/c",":c or :p")> ]?
#         [ $0 ~~ 'e' <obs("/e","interpolated {...} or s{} = ... form")> ]?
#         <obs("suffix regex modifiers","prefix adverbs")>
#     ]?
# }

    # handle composite forms like qww
    def_tokens_rest :quote,false,%w{ qq q },%q{ qm = quote_mod and quotesnabber(':<sym>',qm) }

    def_tokens_simple :quote_mod,false,%w{ w ww x to s a h f c b }

# token quote:rx { <sym> <quotesnabber(':regex')> }

# token quote:m { <sym>  <quotesnabber(':regex')> }
# token quote:mm { <sym> <quotesnabber(':regex', ':s')> }

# token quote:s {
#     <sym>  <pat=quotesnabber(':regex')>
#     <finish_subst($<pat>)>
# }
# token quote:ss {
#     <sym> <pat=quotesnabber(':regex', ':s')>
#     <finish_subst($<pat>)>
# }
# token quote:tr {
#     <sym> <pat=quotesnabber(':trans')>
#     <finish_trans($<pat>)>
# }

# token finish_subst ($pat) {
#     :my %thisop is context<rw>;
#     [
#     # bracketed form
#     | <?{ $pat<delim> == 2 }> ::
#           <.ws>
#           <infix>            # looking for pseudoassign here
#           { %+thisop<prec> == %item_assignment<prec> or
#               .panic("Bracketed subst must use some form of assignment") }
#           <repl=EXPR(%item_assignment)>
#     # unbracketed form
#     | <repl=q_unbalanced(qlang('Q',':qq'), $pat<delim>[0])>
#     ]
# }

# token finish_trans ($pat) {
#     [
#     # bracketed form
#     | <?{ $pat<delim> == 2 }> ::
#           <.ws>
#           <repl=q_pickdelim(qlang('Q',':tr'))>
#     # unbracketed form
#     | <repl=q_unbalanced(qlang('Q',':tr'), $pat<delim>[0])>
#     ]
# }

# # The key observation here is that the inside of quoted constructs may
# # be any of a lot of different sublanguages, and we have to parameterize
# # which parse rule to use as well as what options to feed that parse rule.

# role QLang {
#     has %.option;
#     has $.tweaker handles 'tweak';
#     has $.parser;
#     has $.escrule;

#     # a method, so that everything is overridable in derived grammars
#     method root_of_Q () {
#         return
#             tweaker => ::Q_tweaker,      # class name should be virtual here!
#             parser => &Perl::q_pickdelim,
#             option => < >,
#             escrule => &Perl::quote_escapes;
#     }

#     method new (@pedigree) {
#         if @pedigree == 1 {
# #           my %start = try { self."root_of_@pedigree[0]" } //
#             my %start = try { self.root_of_Q } //
#                 panic("Quote construct @pedigree[0] not recognized");
#             return self.bless(|%start);
#         }
#         else {
#             my $tail = pop @pedigree;
#             my $self = qlang(@pedigree).clone
#                 orelse fail "Can't clone {@pedigree}: $!";
#             return $self.tweak($tail);
#         }
#     }
# }

# sub qlang (@pedigree) {
#     my $pedigree = @pedigree.join;
#     (state %qlang){$pedigree} //= new QLang(@pedigree);
# }

# class Q_tweaker does QLang {
#     has @.escapes;

#     method escset {
#         @.escapes ||=               # presumably resolves after adverbs
#            '\\' xx ?%.option<b>,
#             '$' xx ?%.option<s>,
#             '@' xx ?%.option<a>,
#             '%' xx ?%.option<h>,
#             '&' xx ?%.option<f>,
#             '{' xx ?%.option<c>;
#     } #'

#     multi method tweak (:q($single)) {
#         $single or panic("Can't turn :q back off");
#         %.option.keys and panic("Too late for :q");
#         %.option = (:b, :!s, :!a, :!h, :!f, :!c);
#     }

#     multi method tweak (:qq($double)) {
#         $double or panic("Can't turn :qq back off");
#         %.option.keys and panic("Too late for :qq");
#         %.option = (:b, :s, :a, :h, :f, :c);
#     }

#     multi method tweak (:b($backslash))   { %.option<b>  = $backslash }
#     multi method tweak (:s($scalar))      { %.option<s>  = $scalar }
#     multi method tweak (:a($array))       { %.option<a>  = $array }
#     multi method tweak (:h($hash))        { %.option<h>  = $hash }
#     multi method tweak (:f($function))    { %.option<f>  = $function }
#     multi method tweak (:c($closure))     { %.option<c>  = $closure }

#     multi method tweak (:x($exec))        { %.option<c>  = $exec }
#     multi method tweak (:w($words))       { %.option<w>  = $words }
#     multi method tweak (:ww($quotewords)) { %.option<ww> = $quotewords }

#     multi method tweak (:to($heredoc)) {
#         $.parser = &Perl::q_heredoc;
#         %.option<to> = $heredoc;
#     }

#     multi method tweak (:$regex) {
#         $.tweaker = ::RX_tweaker,
#         $.parser = &Perl::rx_pickdelim;
#         %.option = < >;
#         $.escrule = &Perl::regex_metachar;
#     }

#     multi method tweak (:$trans) {
#         $.tweaker = ::TR_tweaker,
#         $.parser = &Perl::tr_pickdelim;
#         %.option = < >;
#         $.escrule = &Perl::trans_metachar;
#     }

#     multi method tweak (:$code) {
#         $.tweaker = ::RX_tweaker,
#         $.parser = &Perl::rx_pickdelim;
#         %.option = < >;
#         $.escrule = &Perl::regex_metachar;
#     }

#     multi method tweak (*%x) {
#         panic("Unrecognized quote modifier: %x.keys()");
#     }

# }

# class RX_tweaker does QLang {
#     multi method tweak (:g($global))      { %.option<g>  = $global }
#     multi method tweak (:i($ignorecase))  { %.option<i>  = $ignorecase }
#     multi method tweak (:c($continue))    { %.option<c>  = $continue }
#     multi method tweak (:p($pos))         { %.option<p>  = $pos }
#     multi method tweak (:ov($overlap))    { %.option<ov> = $overlap }
#     multi method tweak (:ex($exhaustive)) { %.option<ex> = $exhaustive }
#     multi method tweak (:s($sigspace))    { %.option<s>  = $sigspace }

#     multi method tweak (:$bytes)  { %.option<UNILEVEL> = 'bytes' }
#     multi method tweak (:$codes)  { %.option<UNILEVEL> = 'codes' }
#     multi method tweak (:$graphs) { %.option<UNILEVEL> = 'graphs' }
#     multi method tweak (:$langs)  { %.option<UNILEVEL> = 'langs' }

#     multi method tweak (:$rw)      { %.option<rw>      = $rw }
#     multi method tweak (:$ratchet) { %.option<ratchet> = $ratchet }
#     multi method tweak (:$keepall) { %.option<keepall> = $keepall }
#     multi method tweak (:$panic)   { %.option<panic>   = $panic }

#     # XXX probably wrong
#     multi method tweak (:P5($Perl5)) {
#         %.option<P5> = $Perl5;
#         $.tweaker = ::P5RX_tweaker,
#         $.parser = &Perl::p5rx_pickdelim;
#         $.escrule = &Perl::p5regex_metachar;
#     }

#     multi method tweak (:$nth)            { %.option<nth> = $nth }
#     multi method tweak (:x($times))       { %.option<x> = $times }

#     # Ain't we special!
#     multi method tweak (*%x) {
#         my $na = %x.keys();
#         my ($n,$a) = $na ~~ /^(\d+)(<[a-z]>+)$/
#             or panic("Unrecognized regex modifier: $na");
#         if $a eq 'x' {
#             %.option{x} = $n;
#         }
#         elsif $a eq 'st' | 'nd' | 'rd' | 'th' {
#             %.option{nth} = $n;
#         }
#     }
# }

# class P5RX_tweaker does QLang {
#     multi method tweak (:$g) { %.option<g>  = $g }
#     multi method tweak (:$i) { %.option<i>  = $i }
#     multi method tweak (:$s) { %.option<s>  = $s }
#     multi method tweak (:$m) { %.option<m>  = $m }

#     multi method tweak (*%x) {
#         panic("Unrecognized Perl 5 regex modifier: %x.keys()");
#     }
# }

# class TR_tweaker does QLang {
#     multi method tweak (:$c) { %.option<c>  = $c }
#     multi method tweak (:$d) { %.option<d>  = $d }
#     multi method tweak (:$s) { %.option<s>  = $s }

#     multi method tweak (*%x) {
#         panic("Unrecognized transliteration modifier: %x.keys()");
#     }
# }



    def quotesnabber (*qA)
        $env_vars.scope_enter(:delim)
        $env_vars[:delim] = '' 
        v = ((not before(/\w/)) and nofat and #R XX? ::
         dot_ws and
         starTOK{ q = quotepair and qA.push(q) and dot_ws } and
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


# # assumes whitespace is eaten already

# method peek_delimiters () {
#     return peek_brackets ||
#         substr(self.orig,0,1) xx 2;
# }

# method peek_brackets () {
#     if m:p(self) / \s / {
#         self.panic("Whitespace not allowed as delimiter");
#     }
#     elsif m:p(self) / <+isPe>> / {
#         self.panic("Use a closing delimiter for an opener is reserved");
#     }
#     elsif m:p(self) / (.) ** <?same> > / {
#         my $start = ~$/;
#         my $rightbrack = %open2close{$0} orelse
#             return ();
#         my $stop = $rightbrack x $start.chars;
#         return $start, $stop;
#     }
#     else {
#         return ();
#     }
# }

# regex bracketed ($lang = qlang("Q")) {
#     :my ($start,$stop);
#     <?{ ($start,$stop) = .peek_brackets() }>
#     <q=q_balanced($lang, $start, $stop)>
#     {*}
# }

# regex q_pickdelim ($lang) {
#     :my ($start,$stop);
#     {{
#        ($start,$stop) = .peek_delimiters();
#        if $start eq $stop {
#            $<q> := .q_unbalanced($lang, $stop);
#        }
#        else {
#            $<q> := .q_balanced($lang, $start, $stop);
#        }
#     }}
#     {*}
# }

# regex rx_pickdelim ($lang) {
#     [
#     || { ($<start>,$<stop>) = .peek_delimiters() }
#       $<start>
#       <rx=regex($<stop>)>        # counts its own brackets, we hope
#     || $<stop> = [ [\S] || <panic: Regex delimiter must not be whitespace> ]
#       <rx=regex($<stop>)>
#     ]
#     {*}
# }

# regex tr_pickdelim ($lang) {
#     [
#     || { ($<start>,$<stop>) = .peek_delimiters() }
#       $<start>
#       <tr=transliterator($<stop>)>
#     || $<stop> = [ [\S] || <panic: tr delimiter must not be whitespace> ]
#       <tr=transliterator($<stop>)>
#     ]
#     {*}
# }

# regex transliterator($stop) {
#     # XXX your ad here
# }

# regex q_balanced ($lang, $start, $stop, :@esc = $lang.escset) {
#     <start=$start>
#     $<text> = [.*?]
#     @<more> = (
#         <!before <$stop>>
#         [ # XXX triple rule should just be in escapes to be customizable
#         | <?before <$start> ** 3>
#             $<dequote> = <EXPR(%LOOSEST,/<$stop> ** 3/)>
#         | <?before <$start>>
#             $<subtext> = <q_balanced($lang, $start, $stop, :@esc)>
#         | <?before @esc>
#             $<escape> = [ <q_escape($lang)> ]
#         ]
#         $<text> = [.*?]
#     )*
#     <stop=$stop>
#     {*}
# }

# regex q_unbalanced ($lang, $stop, :@esc = $lang.escset) {
#     $<text> = [.*?]
#     @<more> = (
#       <!before <$stop>>
#       <?before @esc> $<escape> = [ <q_escape($lang)> ]
#       $<text> = [.*?]
#     )*
#     <stop=$stop>
#     {*}
# }

# # We get here only for escapes in escape set, even though more are defined.
# method q_escape ($lang) {
#     $lang<escrule>(self);
#     {*}
# }

    def quote_escapes
        (lex_pos{scan(/\\/) and qq_backslash} or
         (before(/\{/) and block) or
         (before(/$/) and variable and (extrapost;true)) or
         lex_pos{ variable and extrapost } or
         scan(/.|\n/))
    end

# # Note, backtracks!  So expect_postfix mustn't commit to anything permanent.
# regex extrapost {
#     :my $inquote is context = 1;
#     <expect_postfix>*
#     # XXX Shouldn't need a backslash on anything but the right square here
#     <?after <[ \] \} \> \) ]> > 
#     {*}
# }

    def multisig
        rul{ scan(/:?\(/) and dot_ws and signature and dot_ws and starRULE{ scan(/\|/) and dot_ws and scan(/:?\(/) and dot_ws and signature and dot_ws and scan(/\)/) } }
    end

    def routine_def
        rul{ quesRULE{ident} and dot_ws and 
            quesRULE{multisig} and dot_ws and
            starRULE{trait} and dot_ws and
            block }
    end

    def method_def
        rul{ (let_pos{ident and dot_ws and quesRULE{multisig}} or
              let_pos{before{sigil and scan(/\.[\[\{\(]/)} and sigil and postcircumfix }) and dot_ws and
            starRULE{trait} and dot_ws and
            block }
    end

    def regex_def
        rul{ quesRULE{ident} and dot_ws and
            starRULE{trait} and dot_ws and
            quesRULE{ scan(/:?\(/) and dot_ws and signature and dot_ws and scan(/\)/) } and dot_ws and
            regex_block }
    end

    # XXX redundant with routine_def?
    def macro_def
        rul{ quesRULE{ident} and dot_ws and 
            quesRULE{multisig} and dot_ws and
            starRULE{trait} and dot_ws and
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
            parsep = starRULE{ parameter and dot_ws and (scan(/,|:|;;|;/) or before(/-->|\)|\{/)) }
            dot_ws
            parsep and quesRULE{ scan(/-->/) and dot_ws and fulltypename }
        }
        $env_vars.scope_leave
        v
    end

    def_rules_rest :type_declarator,%w{ subset },%q{ name and dot_ws and quesRULE{ scan(/of/) and dot_ws and fulltypename } and scan(/where/) and dot_ws and _EXPR }

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
                   (ident;true) and dot_ws and before(/[\<\(\[\{]/) and
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

    def_rules_rest :statement_prefix,%w{ do try gather contend async lazy },%q{ statement }


    ## term
    def_tokens_rest :term,:term,%w{ undef },%q{ scan(/undef/) and scan(/[ \t]*/) and nofat }
    def_tokens_simple :term,:term,%w{ self * }

# token circumfix:sigil ( --> Term)
#     { <sigil> '(' <semilist> ')' {*} }                          #= $( ) 

# token circumfix:typecast ( --> Term)
#     { <typename> '(' <semilist> ')' {*} }                       #= Type( ) 


    def_tokens_circum :term,%w{ ( },%q{let_pos{ t = statementlist and scan(/\)/) and t }}
    def_tokens_circum :term,%w{ [ },%q{let_pos{ t = statementlist and scan(/\]/) and t }}
    def_tokens_circum :term,%w{ < },%q{let_pos{ t = anglewords('>') and scan(/>/) and t }}
    def_tokens_circum :term,%w{ << },%q{let_pos{ t = shellwords('>>') and scan(/>>/) and t }}
    def_tokens_circum :term,%w{ « },%q{let_pos{ t = shellwords('»') and scan(/»/) and t }}
    def_tokens_simple :infix,:methodcall,%w{ . }
    def_tokens_simple :postfix,:methodcall,%w{ -> }
    def_tokens_simple :postfix,:autoincrement,%w{ i ++ -- }
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


# ## conditional
# token infix:sym<?? !!> ( --> Conditional) {
#     '??'
#     <EXPR(%conditional)>
#     [ '!!' ||
#         [
#         || <?before '='> <panic: Assignment not allowed within ??!!>
#         || <?before '::'> <panic: Please use !! rather than ::>
#         || <?before <infix>>    # Note: a tight infix would have parsed right
#             <panic: Precedence too loose within ??!!; use ??()!! instead >
#         || <panic: Found ?? but no !!; possible precedence problem>
#         ]
#     ]
#     {*}                                                         #= ?? !!
# }


    ## assignment
    def_tokens_rest :infix,false,%w{ = },%q{ xXXX[:sigil] == '$' ? make(xXXX,Hitem_assignment) : make(xXXX,Hlist_assignment) }

    def_tokens_simple :infix,:item_assignment,%w{ := ::= }
    # XXX need to do something to turn subcall into method call here...
    def_tokens_simple :infix,:item_assignment,%w{ .= }
    # Note, other assignment ops generated by infix_postfix_meta_operator rule

    def_tokens_simple :prefix,:loose_unary,%w{ true not }
    def_tokens_simple :infix,:comma,%w{ , p5=> }
    def_tokens_simple :infix,:list_infix,%w{ X Z minmax }

# token term:sigil ( --> List_prefix)
#     { <sym=sigil> \s <arglist> {*} }                                #= $

# token term:typecast ( --> List_prefix)
#     { <sym=typename> \s <arglist> {*} }                             #= Type

    # unrecognized identifiers are assumed to be post-declared listops.
    # (XXX for cheating purposes this rule must be the last term: rule)
    def_tokens_rest :term,:list_prefix,[""],%q{ s=ident and (let_pos{ scan(/\s/) and nofat and a=arglist } or nofat) }
       
    def_tokens_simple :infix,:loose_and,%w{ and andthen }
    def_tokens_simple :infix,:loose_or,%w{ or xor orelse }
    def_tokens_before :terminator,:terminator,%w{ ; <== ==> --> ) ] \} !! }
    def_tokens_before :terminator,:terminator,%w{ \{ } #R added, XXX speculative


    #R regex - ##Q why is this a regex?
    def stdstopper
        (@scanner.eos? ||
         terminator || statement_mod_cond || statement_mod_loop ||
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
        push opstackA, {:prec =>"a="} #R kludge

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

            dot_ws #R added

            my terminatorA = [hereS_workaround.before{stopS.()}]
            my tS = terminatorA[0];
            break if tS and terminatorA[0].bool;
            $env_vars[:thisopH] = {}
            #        my infixA = [hereS_workaround.expect_tight_infix(preclimS)];
            my infixA = [hereS_workaround.expect_infix()];
            my infixS = infixA[0];
            hereS = infixS;
            dot_ws

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




    #############################################3333
    ## Regex
    #############################################3333


    def regex(stop)
        $env_vars.scope_enter(:stop)
        $env_vars[:stop] = stop
        v = rul{ regex_ordered_disjunction }
        $env_vars.scope_leave
        v
    end

    def regex_ordered_disjunction
        rul{scan(/(\|\|)?/) and dot_ws and
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
        rul{ ra = regex_atom and dot_ws and
            (regex_quantifier and
             (ra.max_width or
              panic("Can't quantify zero-width atom"));true) }
    end

    def regex_atom
        (dot_ws and
         ((let_pos{ scan(/#{$env_vars[:stop]}/) } and return(false);false) or
          (regex_metachar and dot_ws) or
          (scan(/\w/) and dot_ws) or
          panic("unrecognized metacharacter")))
    end

    def_tokens_rest :regex_metachar,false,%w{ > && & || | ] ) \\ },%q{ return false }

# token regex_metachar:quant { <regex_quantifier> <panic: quantifier quantifies nothing> }

# # "normal" metachars
# token regex_metachar:sym<{ }> {
#     <block>
#     {{ $/<sym> := <{ }> }}
#     {*}                                                         #= { }
# }

# token regex_metachar:mod {
#     <regex_mod_internal>
#     { $/<sym> := $<regex_mod_internal><sym> }
#     {*}                                                         #= :mod
# }

# token regex_metachar:sym<[ ]> {
#     '[' <regex ']'> ']'
#     { $/<sym>:=<[ ]> }
#     {*}                                                         #= [ ]
# }

# token regex_metachar:sym<( )> {
#     '(' <regex ')'> ')'
#     { $/<sym>:=<( )> }
#     {*}                                                         #= ( )
# }

# token regex_metachar:sym« <( » { '<(' {*} }                     #= <(
# token regex_metachar:sym« )> » { ')>' {*} }                     #= )>

# token regex_metachar:sym« << » { '<<' {*} }                     #= <<
# token regex_metachar:sym« >> » { '>>' {*} }                     #= >>
# token regex_metachar:sym< « > { '«' {*} }                       #= «
# token regex_metachar:sym< » > { '»' {*} }                       #= »

# token regex_metachar:qw {
#     <?before '<' \s >  # (note required whitespace)
#     <quote>
#     {*}                                                         #= quote
# }

# token regex_metachar:sym«< >» {
#     '<' <unsp>? <regex_assertion> '>'
#     {*}                                                         #= < >
# }
    def_tokens_rest :regex_metachar,false,%w{ \\ },%q{ regex_backslash }
    def_tokens_simple :regex_metachar,false,%w{ . ^^ ^ }
# token regex_metachar:sym<$$> {
#     <sym>
#     [ <?before (\w+)> <obs("\$\$$0 to deref var inside a regex","\$(\$$0)")> ]?
#     {*}
# }
# token regex_metachar:sym<$>  {
#     '$'
#     <before
#     | \s
#     | '|'
#     | ')'
#     | ']'
#     | '>'
#     >
#     {*}                                                         #= $
# }

# token regex_metachar:sym<' '> { <?before "'"  > <quotesnabber(":q")>  }
# token regex_metachar:sym<" "> { <?before '"'  > <quotesnabber(":qq")> }

# token regex_metachar:var {
#     <!before '$$'>
#     <sym=variable> <.ws>
#     $<binding> = ( ':=' <.ws> <regex_quantified_atom> )?
#     {*}                                                         #= var
# }

    def codepoint
        scan(/\[(.*?)\]/)
    end

    def_tokens_full :q_backslash,%w{ qq },%q{ before(/qq/) and <quote> }
    def_tokens_rest :q_backslash,false,[""],%q{ scan(/./) }

    def_tokens_rest :qq_backslash,false,%w{ c },%q{ scan(/\[[^\]\n\r]*\]/) or codepoint }

    def_tokens_simple :qq_backslash,false,%w{ \\ a b e f n r t 0 }
    def_tokens_rest :qq_backslash,false,%w{ o },%q{ octint or (scan(/\[/) and octint and starTOK{ scan(/,/) and octint } and scan(/\]/)) }
    def_tokens_rest :qq_backslash,false,%w{ x },%q{ hexint or (scan(/\[/) and hexint and starTOK{ scan(/,/) and hexint } and scan(/\]/)) }

# token regex_backslash:a { :i <sym> }
# token regex_backslash:b { :i <sym> }
# token regex_backslash:c { :i <sym>
#     [
#     || '[' <-[ \] \v ]>* ']'
#     || <codepoint>
#     ]
# }

# token regex_backslash:d { :i <sym> }
# token regex_backslash:e { :i <sym> }
# token regex_backslash:f { :i <sym> }
# token regex_backslash:h { :i <sym> }
# token regex_backslash:n { :i <sym> }
# token regex_backslash:o { :i <sym> [ <octint> | '['<octint>[','<octint>]*']' ] }
# token regex_backslash:r { :i <sym> }
# token regex_backslash:t { :i <sym> }
# token regex_backslash:v { :i <sym> }
# token regex_backslash:w { :i <sym> }
# token regex_backslash:x { :i <sym> [ <hexint> | '['<hexint>[','<hexint>]*']' ] }
# token regex_backslash:oops { :: <panic: unrecognized regex backslash sequence> }

    def_tokens_rest :regex_assertion,false,%w{ ? ! },%q{ regex_assertion }

# token regex_assertion:sym<{ }> { <block> }
# token regex_assertion:variable {
#     <?before <sigil>>  # note: semantics must be determined per-sigil
#     <EXPR(%LOOSEST,&assertstopper)>
#     {*}                                                        #= variable
# }
# token regex_assertion:method {
#     <?before '.' <!before '>'> >
#     <EXPR(%LOOSEST,&assertstopper)>
#     {*}                                                        #= method
# }
# token regex_assertion:ident { <ident> [               # is qq right here?
#                                 | '=' <regex_assertion>
#                                 | ':' <.ws>
#                                     <q_unbalanced(qlang('Q',':qq'), :stop«>»)>
#                                 | '(' <semilist> ')'
#                                 | <.ws> <EXPR(%LOOSEST,&assertstopper)>
#                                 ]?
# }




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

    #R XXX TODO
# token regex_mod_internal:adv {
#     <quotepair> { $/<sym> := «: $<quotepair><key>» }
# }

    def_tokens_rest :regex_mod_internal,false,%w{ :i },%q{ quesTOK{regex_mod_arg} }
    def_tokens_simple :regex_mod_internal,false,%w{ :!i }

    def_tokens_rest :regex_quantifier,false,%w{ * + ? },%q{ quantmod }
    def_tokens_rest :regex_quantifier,false,%w{ ** },%q{ quantmod and dot_ws and scan(/\d+(?:\.\.(?:\d+|\*))?/) or block or regex_atom }
    def quantmod; scan(/[?!:+]/); true; end


    Typenames = %w{ Bit Int Str Num Complex Bool Rat
    Exception Code Block List Seq Range Set Bag Junction Pair
    Mapping Signature Capture Blob Whatever Undef Failure
    StrPos StrLen Version P6opaque
    bit int uint buf num complex bool rat
    Scalar Array Hash KeyHash KeySet KeyBag Buf IO Routine Sub Method
    Submethod Macro Regex Match Package Module Class Role Grammar Any Object }
    def is_type(name); Typenames.member?(name); end


    def heredoc; false; end
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
