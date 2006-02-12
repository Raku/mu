module Parse::Rule
  class Exp
    @@_serno = 0

    def serrno(prefix, start)
      prefix = "R" if prefix.empty?
      val = @@serno++
        start = val if not start
      ( prefix+=val.to_s,  val)
    end

    def emit(src, format, args)
      if format.length >= 2 and format[-2..2] == "##" then
        codeClone = code + format
        lineList = codeClone.split "\n"
        format += " " + lineList.length.to_s
      else
        format += "\n"
      end
      do
        formatPos = format.index("%s")
        if formatPos < 0 then
          return code += format
        end
        format.replace args.shift
      end
    end

    def emit(src, format, args)

    end

    def escape(src, format, args)

    end
  end

  class Literal
  end
  class Scalar
  end
  class CCShortcut
  end
  class EnumCharList
  end
  class Anchor
  end
  class Concat
  end
  class Alt
  end
  class Conj
  end
  class Group
  end
  class Subrule
  end
  class Cut
  end
  class Quant
  end
  class Modifier
  end
  class Closure
  end
  class Commit
  end
end
