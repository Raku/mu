use Onion::Ast;
use Onion::Visitor::Emit::AstPerl;
#use Onion::Visitor::Emit::Perl5;
use Onion::Traverse;
# say 'hello World'
my $ast = ::CompUnit(
    body => ::Lit::Code(
        body => [
            ::Apply(
                arguments => [ ::Val::Buf( buf => 'hello, World', ), ],
                code      => ::Var(
                    namespace => [],
                    name      => 'say',
                    twigil    => '',
                    sigil     => '&',
                ),
            ),
        ],
        sig => ::Sig( invocant => '', positional => [], ),
#        pad => ::Pad(
#            lexicals  => [],
#            namespace => "Main",
#            parent    => ::Pad(...),
#            evaluator => ::Sub(...),
#        ),
        state => {},
    ),
    methods    => {},
    name       => 'Main',
    attributes => {},
    traits     => [],
    unit_type  => 'module',
  )
say $ast.emit(Onion::Visitor::Emit::AstPerl.new());
