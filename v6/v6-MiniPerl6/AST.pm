# Mini Perl 6 Syntax Nodes

# A program is a sequence of compilation units.
subset Program of (Seq of CompUnit);

# A compilation unit is a named class.
class CompUnit {
    has $.class         is Type;                    # class Name;
    has %.attributes    is Mapping of Type;         # has $.attr is Type;
    has %.methods       is Mapping of Lit::Code;    # method foo { ... }
    has $.body          is Lit::Code;               # body of code
}

subset Exp of (Var | Val | Lit | Bind | Call | Apply | Return | If | When | For );
subset ID of Str;
subset Type of Str;

enum Sigil ('$', '%', '@', '&');
enum Twigil ('', '.', '!', '^');

class If {
    has $.cond          is Exp;
    has @.body          is Seq of Exp;
    has @.otherwise     is Seq of Exp;
}

class When {
    has @.parameters    is Seq of Exp;
    has @.body          is Seq of Exp;
}

class For {
    has $.cond          is Exp;
    has @.topics        is Seq of Var;
    has @.body          is Seq of Exp;
}

class Return {
    has $.result    is Exp;
}

class Var {
    has $.sigil     is Sigil;
    has $.twigil    is Twigil;
    has $.name      is ID;
}

subset Val of
    ( Val::Undef    # undef
    | Val::Object   # (not exposed to the outside)
    | Val::int      # 123
    | Val::bit      # True, False
    | Val::num      # 123.456
    | Val::buf      # "moose"
    );

class Val::Object {
    has $.class         is Type;
    has %.fields        is Mapping of Val;
}

subset Lit of
    ( Lit::Seq      # (a, b, c)
    | Lit::Array    # [a, b, c]
    | Lit::Hash     # {a => x, b => y}
    | Lit::Code     # sub $x {...}
    | Lit::Object   # ::Tree(a => x, b => y);
    );

class Lit::Code {
    has %.pad           is Mapping of Type; # All my/state/parameter variables
    has %.state         is Mapping of Exp;  # State initializers, run upon first entry 
    has @.parameters    is Seq of Exp;      # Signature
    has @.body          is Seq of Exp;      # Code body 
}

class Lit::Object {
    has $.class         is Type;            # Class name
    has %.fields        is Mapping of Exp;  # Field initializers
}

class Bind {
    has @.parameters    is Exp;             # Signature
    has @.arguments     is Exp;             # Capture
}

class Call {
    has $.invocant  is Exp;                 # $obj
    has $.method    is ID;                  # .method
    has @.arguments is Seq of Exp;          # ($args)
}

class Apply {
    has $.code      is Exp;                 # &sub
    has @.arguments is Seq of Exp;          # ($args)
}

