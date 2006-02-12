module Parse::Rule
  class Match < Hash
    @target       # target
    @fromPos      # start of match
    @pos          # current match position
    @coroutine    # match's corou
    @capture      # subpattern captures

    def newFrom( src )
      if src.is_a( "Mob" ) then
        if isGrammar() then
          target = mob.target
          from = mob.from.clone
        end
      else
        if isGrammar() then
        end
      end
      me = Grammar.new()
      me.target = @target
      me.from = @from
      me.pos = -1

      if has_fromd and from < 0 then
        from = fromd
      end

      return me, target, from, pos

    end

    def next()
      if not @coroutine then
        @pos = -1
      else
        @coroutine.call()
      end
    end

    def to()
      @pos
    end

    def matched() #__get_bool
      @pos > 0
    end

    def __get_integer()
      self
    end 

    def __get_number()
      self
    end

    def stringVal()
      return "" if not matched()
      return "" if @pos <= @from
      @target[@from..@pos]
    end

    def get_hash()
      return self
    end

    def get_array()
      return @capture
    end

  end
end 
