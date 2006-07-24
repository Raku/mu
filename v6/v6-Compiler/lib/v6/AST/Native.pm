
use v6-alpha;
use v6::AST::Base;

class v6::AST::Native  is v6::AST::Base {}

class v6::AST::NBit    is v6::AST::Native {}
class v6::AST::NFloat  is v6::AST::Native {}
class v6::AST::NInt    is v6::AST::Native {}

class v6::AST::IFinite     is v6::AST::NInt {}
class v6::AST::INotANumber is v6::AST::NInt {}
class v6::AST::IInfinite   is v6::AST::NInt {}

class v6::AST::NStr    is v6::AST::Native {}
class v6::AST::NBool   is v6::AST::Native {}
