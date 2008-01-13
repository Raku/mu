package KindaPerl6::Runtime::Perl5V6::Runtime;
use Exporter 'import';
our @EXPORT=qw($Code_say $Code_print $Code_substr $Code_prefix_58__60__43__43__62_ $Code_prefix_58__60__126__62_ $Code_VAR_defined);
our $Code_say = sub {
    print(@_,"\n");
};
our $Code_print = sub {
    print(@_);
};
our $Code_substr = sub {
    substr($_[0],$_[1],$_[2]);
};
#prefix:<++>
our $Code_prefix_58__60__43__43__62_ = sub {
    ++$_[0];
};
#prefix:<~>
our $Code_prefix_58__60__126__62_ = sub {
    ''.$_[0];
};
our $Code_VAR_defined = sub {
    defined($_[0]);
};
1;
