#!/usr/bin/perl

use Runtime::RuleCompiler;
use Perl6::Slurp;
my $filename = shift;
my $tail = slurp($filename);

RULEMATCH:
while ($tail =~ m/rule\s+(\S+)\s+\{(.+)$/gs) {
    my $rulename = $1;
    $tail = $2;
    my $source = "";
    my $deep = 1;
  PARENMATCH:
    while ($tail =~ m/([^\{\}]*)([\{\}])(.+)$/gs) {
        $source .= $1;
        my $paren = $2;
        $tail = $3;
        if ($paren eq '}') {
            $deep--;
            last PARENMATCH unless $deep;
        } else {
            $deep++;
        }
        $source .= $paren;
    }

    my $rule = Runtime::RuleCompiler->compile($source);
    print '${\''.$rulename.'\'} = Runtime::RuleCompiler->compiled(
'.$rule->perl5().'
);
*{\''.$rulename.'\'} = ${\''.$rulename.'\'}->code();
';
}
__END__


package Test;
${'a'} = '10';
*{'a'} = sub { '20 ' };
print Test->a.$/;
print &Test::a.$/;
print $Test::a.$/;


${'rule'} = Pugs::Compiler::Rule->compiled(
sub {
    
}
});
*{'rule'} = sub {
    ${'rule'}->match(@_);
}
