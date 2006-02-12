module Goto
  LabelContexts = []
  
  class Label
    attr_accessor :label
    attr_accessor :block

    def initialize(label, block)
      @label = label
      @block = block
    end

    def ==(sym)
      @name == sym
    end
  end

  class Label
