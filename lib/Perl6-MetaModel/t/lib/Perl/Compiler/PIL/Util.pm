
package Perl::Compiler::PIL::Util;

use strict;
use warnings;

use Perl6::MetaModel;

=pod

class Perl::Compiler::PIL::Util::Pad {
    has Perl::Compiler::PIL::Util::Pad $.parent;
    has Str @.names;
}

=cut

class 'Perl::Compiler::PIL::Util::Pad' => {
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::Util::Pad' => '$.parent'], 
            '@.names' 
        ]
    }  
};


=pod

class Perl::Compiler::PIL::Util::Pos {
    has Int $.line;
    has Int $.column;
}

=cut

class 'Perl::Compiler::PIL::Util::Pos' => {
    class => {
        attrs => [ '$.line', '$.column' ]
    }  
};


=pod

class Perl::Compiler::PIL::Util::PosRange {
    has Str $.file;
    has Perl::Compiler::PIL::Util::Pos $.begin;
    has Perl::Compiler::PIL::Util::Pos $.end;

    has $.match;
}

=cut

class 'Perl::Compiler::PIL::Util::PosRange' => {
    class => {
        attrs => [ 
            '$.file', 
            [ 'Perl::Compiler::PIL::Util::Pos' => '$.begin'], 
            [ 'Perl::Compiler::PIL::Util::Pos' => '$.end'  ], 
            '$.match' 
        ],
    }
};

=pod

role Perl::Compiler::PIL::Util::Type { }

=cut

role 'Perl::Compiler::PIL::Util::Type' => {};

=pod

class Perl::Compiler::PIL::Util::ConcreteType
    does Perl::Compiler::PIL::Util::Type {
    has Str $.name;
}

=cut

class 'Perl::Compiler::PIL::Util::ConcreteType' => {
    does => [ 'Perl::Compiler::PIL::Util::Type' ],
    class => {
        attrs => [ '$.name' ]
    }
};

=pod

class Perl::Compiler::PIL::Util::TypeConjunction
    does Perl::Compiler::PIL::Util::Type {
    has Perl::Compiler::PIL::Util::Type $.left;
    has Perl::Compiler::PIL::Util::Type $.right;
}

=cut

class 'Perl::Compiler::PIL::Util::TypeConjunction' => {
    does => [ 'Perl::Compiler::PIL::Util::Type' ],
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::Util::Type' => '$.left'  ], 
            [ 'Perl::Compiler::PIL::Util::Type' => '$.right' ] 
        ]        
    }
};

=pod

class Perl::Compiler::PIL::Util::TypeDisjunction
    does Perl::Compiler::PIL::Util::Type {
    has Perl::Compiler::PIL::Util::Type $.left;
    has Perl::Compiler::PIL::Util::Type $.right;
}

=cut

class 'Perl::Compiler::PIL::Util::TypeDisjunction' => {
    does => [ 'Perl::Compiler::PIL::Util::Type' ],
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::Util::Type' => '$.left'  ], 
            [ 'Perl::Compiler::PIL::Util::Type' => '$.right' ] 
        ]                
    }
};

=pod

role Perl::Compiler::PIL::Util::Context { }

=cut

role 'Perl::Compiler::PIL::Util::Context' => {};

=pod

class Perl::Compiler::PIL::Util::VoidContext
    does Perl::Compiler::PIL::Util::Context
{ }

=cut

class 'Perl::Compiler::PIL::Util::VoidContext' => {
    does => [ 'Perl::Compiler::PIL::Util::Context' ]
};

=pod

class Perl::Compiler::PIL::Util::ItemContext
    does Perl::Compiler::PIL::Util::Context {
    has Perl::Compiler::PIL::Util::Type $.type;
}

=cut

class 'Perl::Compiler::PIL::Util::ItemContext' => {
    does => [ 'Perl::Compiler::PIL::Util::Context' ],
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::Util::Type' => '$.type' ] 
        ]
    }  
};

=pod

class Perl::Compiler::PIL::Util::SlurpyContext
    does Perl::Compiler::PIL::Util::Context {
    has Perl::Compiler::PIL::Util::Type @.types;
    has Bit $.unbounded;   # the last type represents a *@array of
                           # that type if true
}

=cut

class 'Perl::Compiler::PIL::Util::SlurpyContext' => {
    does => [ 'Perl::Compiler::PIL::Util::Context' ],
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::Util::Type' => '@.types' ], 
            '$.unbounded'
        ]
    }
};

=pod

class Perl::Compiler::PIL::Util::Signature {
    has Perl::Compiler::PIL::Util::Parameter @.params;
}

=cut

class 'Perl::Compiler::PIL::Util::Signature' => {
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::Util::Parameter' => '@.params' ] 
        ],
    }  
};

=pod

class Perl::Compiler::PIL::Util::Parameter {
    has Bit $.is_invocant;
    has Bit $.is_optional;
    has Bit $.is_named;
    has Bit $.is_ref;
    has Bit $.is_rw;
    has Bit $.is_delayed;

    has Str $.name;
    has Perl::Compiler::PIL::Util::Context $.context;
    has Perl::Compiler::PIL::PIL $.default;
}

=cut

class 'Perl::Compiler::PIL::Util::Parameter' => {
    class => {
        attrs => [
            '$.is_invocant',
            '$.is_optional',
            '$.is_named',
            '$.is_ref',
            '$.is_rw',
            '$.is_delayed',
            '$.name',
            [ 'Perl::Compiler::PIL::Util::Context' => '$.context' ],
            [ 'Perl::Compiler::PIL::PIL' => '$.default' ]
        ]
    }  
};

1;
 