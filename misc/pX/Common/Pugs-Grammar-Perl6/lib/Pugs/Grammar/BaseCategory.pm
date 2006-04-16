package Pugs::Grammar::BaseCategory;
use strict;
use warnings;
use Pugs::Compiler::Rule;
use base qw(Pugs::Grammar::Base);

# from Rule.pmc
sub ws {
    my $grammar = shift;
    $_[0] = "" unless defined $_[0];
    my $bool = $_[0] =~ /^((?:\s|\#(?-s:.)*)+)(.*)$/sx;
    #print "ws: $grammar [$_[0]] -- [$2] \n";
    return {
        bool  => $bool,
        match => $1,
        tail  => $2,
    }
};
    
sub add_rule {
    my ( $class, $key, $rule ) = @_;
    no strict qw( refs );
    ${"${class}::hash"}{$key} = Pugs::Compiler::Rule->compile( 
        $rule, 
        grammar => $class,
    );
}

sub capture {
    # print Dumper ${$_[0]}->{match}[0]{match}[1]{capture}; 
    return ${$_[0]}->{match}[0]{match}[1]{capture};
}

sub recompile {
    my ( $class ) = @_;
    no strict qw( refs );
    #print "creating ${class}::parse()\n";
    *{"${class}::parse"} = Pugs::Compiler::Rule->compile( '
        %' . $class . '::hash
        { return Pugs::Grammar::BaseCategory::capture( $/ ) }
    ' )->code;
}

1;
