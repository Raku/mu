use v5.10;
use MooseX::Declare;
class Mildew::Backend::Desugar {
    method output($what,$where) {
        if ($where) {
            open(my $out,">",$where);
            binmode $out, ':utf8';
            print $out $what;
        } else {
            binmode STDOUT, ':utf8';
            print $what;
        }
    }
    method compile($ast,$output) {
        $self->output($ast->pretty."\n",$output);
    }
    method run($ast) {
        die;
    }
}
