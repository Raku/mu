module Rpn
  def Rpn.evaluate(expr)
    stack = []
    expr.split(' ').each do |tok|
      stack << 
        case tok
        when /^-?\d+$/
          tok.to_i
        when %r{^[-+*/]$}
          x, y = stack.pop, stack.pop
          x && y or raise "Stack underflow"
          y.send tok, x
        else
          raise "Invalid token:\"#{tok}\""
        end
    end
    stack.length == 1 or raise "Invalid stack:[#{stack*' '}]"
    stack[0]
  end
end
