
use v6-alpha;
use v6::AST::Base;

role v6::AST::Native     does v6::AST::Base   {}

role v6::AST::NBit       does v6::AST::Native {}
role v6::AST::NFloat     does v6::AST::Native {}
role v6::AST::NInt       does v6::AST::Native {}

role v6::AST::IFinite      does v6::AST::NInt {}
role v6::AST::INotANumber  does v6::AST::NInt {}
role v6::AST::IInfinite    does v6::AST::NInt {}

role v6::AST::NStr       does v6::AST::Native {}
role v6::AST::NBool      does v6::AST::Native {}
