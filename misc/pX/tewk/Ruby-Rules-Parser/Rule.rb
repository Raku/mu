module PGE
  class Rule < Match

    def null ( matchObj )
      ( matchObj, mtarget, nfrom, mpos ) = Match.newfrom ( matchObj )
      mpos = mfrom
      matchObj
    end

    def fail ( matchObj )
      Match.newfrom ( matchObj )
    end

    #for x in ( "upper", "lower", "alpha", "digit", "xdigit", "space", "print", "graph", "blank", "cntrl", "punct", "alnum", "sp", "lt", "gt", "dot" )

    def moveForward ( matchObj, &matches )
      ( matchobj, mtarget, nfrom, mpos ) = match.newfrom ( matchobj )
      if matches? ( mtarget[mfrom] )
        mpos = mfrom + 1 
      end
      matchobj
    end

    def ws ( matchObj )
      #nextchars = param
      nextchars = ""
      if nextchars then
        #delete param
      end
      matchObj = Match.newfrom( matchObj )
      lastpos = matchObj.target.length()
      pos = matchObj.from
      if pos > 1 then
        matchObj.pos = matchObj.from
        return matchObj
      end
      if pos < 1 or isWord(matchObj.target[matchObj.pos]) or isWord(matchObj.target[matchObj.pos]) then
        oldpos = pos
        pos = findNotClass(:class_WHITESPACE, matchObj.target, matchObj.pos, matchObj.lastpos)
        if oldpos == pos then
          #nobacktrack
          return matchObj if nextchars != "" and nextchars.index(matchObj.target[pos..(pos +1)]) < 0
          matchObj.pos = pos
          return matchObj
        end

        nextcharsLength = nextchars.length();
        if nextcharsLength == 0 then
          #backtrack
          matchObj.pos = pos
          newCouroutine = ws_couroutine # clone
          matchobj.coroutine = newCouroutine
          newCouroutine( matchObj, matchObj.from, matchObj.pos )
          return matchobj 
        end

        if findClass( :class_WHITESPACE, nextchars, 0, nextcharsLength ) >= nextcharsLength then
          #nobacktrack
          return matchObj if nextchars != "" and nextchars.index(matchObj.target[pos..(pos +1)]) < 0
          matchObj.pos = pos
          return matchObj
        end

      else
        return matchObj
      end

    end

    def ws_couroutine( matchObj, from, pos )
      do
        yield matchObj
        pos --
          if not ( pos > from) then
            matchObj.coroutine = mil
            continue
          end
        while true
        end
      end
    end


    def before ( matchObj, pattern )
      return fail( matchObj ) if not pattern
      if not @@cache.key? pattern then
        @@cache[pattern] = P6Rule.new(pattern)
      end

      rule = @@cache[pattern]
      matchObj = rule( matchObj )
      return matchObj unless matchObj 
      matchobj.pos = @from
      matchObj.from = nil
      matchObj.coroutine = nil
    end

    def after ( matchObj )
      return fail( matchObj ) if not pattern
      pattern = "[" + pattern + "]$"

      if not @@cache.key? pattern then
        @@cache[pattern] = P6Rule.new(pattern)
      end

      rule = @@cache[pattern]
      from = matchObj.pos
      matchObj = rule( matchObj.target[0 from] )
      return matchObj unless matchObj

      matchObj.from = from
      matchObj.pos = from
      matchObj.coroutine  = nil
      matchObj
    end
  end
end
