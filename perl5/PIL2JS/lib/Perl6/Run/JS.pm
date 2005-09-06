
package Perl6::Run::JS;
use strict;
use vars qw($VERSION @ISA @EXPORT);
require Exporter;
$VERSION = '0.01';
@ISA = qw(Exporter);
@EXPORT = qw();

use PIL2JS;
use JavaScript::SpiderMonkey;

sub new {
    my($class)=@_;
    my $jssm = JavaScript::SpiderMonkey->new();
    $jssm->init();
    $jssm->function_set("print", sub { print "@_\n"; });
    my $self = {
        JS => $jssm
    };
    bless $self,$class;
    $self->init();
}
sub init {
    my($self)=@_;

    my $js = jsbin_hack(compile_perl6_to_standalone_js("-e", ""));
    $self->{JS}->eval($js);    
    die "$self: init: $@" if $@;

    $self->{JS}->function_set("_bare_js_function__eval_perl5",
                              sub { my($code)=@_;
                                    my @res = eval($code);  warn $@ if $@;
                                    return @res; });
    $self->eval(q/sub eval_perl5 ($str) { # XXX - should be *eval_perl5
      return JS::inline('(function (str) {
          return _bare_js_function__eval_perl5(str);
        })')(~$str);
    }/);

    $self;
}

sub eval {
    my($self,$p6)=@_;
    my $js = compile_perl6_to_mini_js("-e", $p6);
    $self->{JS}->eval($js); # $@ passed on
}
sub eval_js {
    my($self,$js)=@_;
    $self->{JS}->eval($js);
}

sub DESTROY {
    my($self)=@_;
    $self->{JS}->destroy();
}

1;
__END__
