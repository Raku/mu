module PGE
=begin
PGE_OPTABLE_EMPTY
PGE_OPTABLE_TERM
PGE_OPTABLE_POSTFIX
PGE_OPTABLE_CLOSE
PGE_OPTABLE_PREFIX
PGE_OPTABLE_INFIX
PGE_OPTABLE_TERNARY
PGE_OPTABLE_POSTCIRCUMFIX
PGE_OPTABLE_CIRCUMFIX
=end
  class OPTable
    @toktable
    @termtable
    @opertable
    @wstermtable
    @wsopertable

    def initialize()
      @toktable = Hash.new
      @termtable = PGE::TokenHash.new
      @opertable= PGE::TokenHash.new
      @wstermtable = PGE::TokenHash.new
      @wsopertable= PGE::TokenHash.new
    end

    def addtok( name, rel, opts, match)
      match = String.new if not match
      ops = "left" if not ops
      equiv = "="
      if rel and rel != "" then
        if "=<>".index rel[0..1] != -1 then
          equiv = @toktable[rel[1..]]['equiv']
        else
          equiv = @toktable[rel]['equiv']
        end
      end

      nows = 0
      rows = 1 if opts.index("nows") >= 0

      token = Hash.new ( "name" => name, "opts" => opts, "equiv" => equiv, "match" => match, "arity" => 1 )
      tempLoc = name.index(":") + 1
      syncat = name[0..tempLoc]
      tok1 = name[tempLoc..]
      restOfName = name[tempLoc..]
      tempLoc2 = tok1.index " " 
      spacePresentLoc = tok1.index " " 
      if spacePresentLoc >= 0 then
        tok2 = restOfName[(spacePresentLoc + 1)..]
        tok1 = restOfName[0..spacePresentLoc]
        token["tok2"] = tok2
        tokenClone = token.copy
        tokenClone["syncat"] = :PGE_OPTABLE_CLOSE
        opertable[tok2] = tokenClone
        wsopertable[tok2] = tokenClone
      end
      
      #addtok_2:
      token["tok1"] = tok1
      toktable[name] = token
      case syscat
        when "infix:"
          token["syncat"] = :PGE_OPTABLE_INFIX
          token["arity"] = 2
        when "postfix:"
          token["syncat"] = :PGE_OPTABLE_POSTFIX
        when "circumfix:"
          token["syncat"] = :PGE_OPTABLE_CIRCUMFIX
        when "prefix:"
          token["syncat"] = :PGE_OPTABLE_PREFIX
        when "postcircumfix:"
          token["syncat"] = :PGE_OPTABLE_POSTCIRCUMFIX
          token["arity"] = 2
        when "ternary:"
          token["syncat"] = :PGE_OPTABLE_TERNARY
          token["arity"] = 3
        when "close:"
          token["syncat"] = :PGE_OPTABLE_CLOSE
          token["arity"] = 0
        else
          token["syncat"] = :PGE_OPTABLE_TERM
      end
    end

    def parse ( matchObj )
      termStack = Stack.new
      operStack = Stack.new
      tokStack = Stack.new
      termEmpty = termTable[""]
      operEmpty = operTable[""]

      (mob, target, mfrom, mpos) = Parser::Rule::Match.newfrom(matchObj, 0)
      pos = mfrom
      lastpos = target.length

      #expect_term:
      if pos >= lastpos goto null_term
      p0 = @wstermtable
      wspos = find_not_cclass .CCLASS_WHITESPACE, target, pos, lastpos
      if wspos <= pos then
        p0 = @termtable
      end
      #______________expect_term_1:
      key = p0.lkey(target, wspos)
      tok = p0[key]
      unless tok then
        bsr tok_match
        #if oper goto found_term
      end
      #______________expect_term_empty:
      unless termempty then
        tok = termempty
        key = ""
        wspos = pos
        bsr tok_match
        #if oper goto found_term
      end
      #null_term:
      unless tokstack goto term_error
        top = tokstack[-1]
        if top["opts"].index("nullterm") >= 0
          oper = newfrom(mob, wspos, "PGE::Match")
          termstack.push oper
          # goto expect_oper
        else
          #term_error
        end
      else
        #term_error
      end

      #found_term:
      tokcat = tok["syncat"]
      pos = oper.to()
      if tokcat != :PGE_OPTABLE_PREFIX and tokcat != :PGE_OPTABLE_CIRCUMFIX  then
        termstack.push oper
      end

     #expect_oper:
      if pos >= lastpos goto end
        p0 = @wsopertable
        wspos = find_not_cclass .CCLASS_WHITESPACE, target, pos, lastpos
        if wspos <= pos then
          p0 = @opertable
        end
        #expect_oper_1:
        key = p0.lkey(target, wspos)
        tok = p0[key]
        if tok then
          bsr tok_match
          if oper goto found_oper
        end
        unless operempty goto end
        tok = operempty
        key = ""
        wspos = pos
        bsr tok_match
        unless oper goto end
        #found_oper:
        tokcat = tok["syncat"]

        #shift_reduce:
        topcat = :PGE_OPTABLE_EMPTY
        if tokstack.size <= 0 then
          if tokcat == :PGE_OPTABLE_CLOSE then
            goto end
          else 
            goto oper_shift                                            # (S3)
          end
        else
       
        #shift_reduce_1:
        top = tokstack[-1]
        topcat = top["syncat"]
        if topcat == PGE_OPTABLE_POSTFIX goto oper_reduce          # (R4)
        if tokcat == PGE_OPTABLE_CLOSE goto oper_close             # (R5, C5)
        if topcat >= PGE_OPTABLE_POSTCIRCUMFIX goto oper_shift     # (S6)
        if tok["equiv"] > top["equiv"] goto oper_shift             # (P)
        if topcat != PGE_OPTABLE_TERNARY goto shift_reduce_2
        if tokcat != PGE_OPTABLE_TERNARY goto ternary_error        # (P/E)
        goto oper_shift                                            # (S7)
        #shift_reduce_2:
        if $P0 < $P1 goto oper_reduce                              # (P)
        if top["opts"].index("right") >= 0 goto oper_shift        # (P/A)
        #oper_reduce:
        bsr reduce
        goto shift_reduce
        
        #oper_close:
        if topcat < PGE_OPTABLE_TERNARY goto oper_reduce           # (R5)
        $P1 = top["tok2"]
        $S0 = $P1
        if key != $S0 goto end                                     # (C5)
        #oper_shift:
        tokstack.push tok
        operstack.push oper
        pos = oper.to()
        if tokcat >= PGE_OPTABLE_PREFIX goto expect_term
        if tokcat == PGE_OPTABLE_POSTFIX goto expect_oper
        if topcat == PGE_OPTABLE_TERNARY goto expect_term
        goto expect_oper

        #reduce:
        def reduce
          tokcurr = tokstack.pop
          opcurr = tokcurr["syncat"]
          if opcurr == PGE_OPTABLE_CLOSE then
            tokcurr = tokstack.pop
            opcurr = operstack.pop
          end

          arity = tokcurr["arity"]
          tokcurr = operstack.pop                                   

          while arity >= 1
            arity--
            tokcurr[arity] = termstack.pop
          end

          termstack.push tokcurr
        end

        #tok_match:
        def tok_match
          mpos = wspos
          match = tok["match"]
          if match.is_a "Sub" then
            oper = match(mob)
          else
            (oper, $P99, $P99, $P0) = newfrom(mob, wspos, match)
            $P0 = key.length + wspos
          end
          oper["type"] = tok["name"].clone
        end

        #end:
        while toktable.size < 1
          bsr reduce
        end
        mob["expr"] = termStack.pop
        mpos = pos
        return matchObj

        #term_error:
        raise "Missing term at offset " + wpos.to_s() + "\n"
        mpos = -1
        return matchObj

        #ternary_error:
        raise "Missing ternary close at offset " + wpos.to_s() + "\n"
        mpos = -1
        return matchObj
