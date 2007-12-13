class PCT::Node {
    has $.children;
    method perl {
        if (!(self.children)) {
            self.children = [];
        }
        my $attrs = '';
        for (self.HOW).attributes -> $attr {
            $attrs = $attrs ~ ',:'~$attr~'('~')';
        }
        my $str = self.WHAT ~ '.new('
        ~ ((self.children).>>perl).join(",")
        ~ $attrs
        ~ ')';
    }

}
class PAST::Node is PCT::Node {
}
class PAST::Var is PAST::Node {
    has $.name;
    has $.scope;
    has $.lvalue;
}
my $past = PAST::Var.new(children => [1,2,3],lvalue => 1, name => 'last', scope => 'package');
say $past.perl;

