use MooseX::Declare;
class VAST::infix__S_Comma {
    use AST::Helpers;
    method emit_m0ld {
#        use Data::Dumper;
#        print Dumper($self);
        #ref $_ eq 'HASH' ? EXPR($_) : 
        map {$_->emit_m0ld} @{$self->{args}};
    }
}
