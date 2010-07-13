use utf8;
use v5.10;
use MooseX::Declare;
class VAST::statement_control__S_no {
    use AST::Helpers;
    method emit_m0ld {
        use YAML::XS;
        my $module = $self->{module_name}{longname};
        if ($module->{name}{identifier}{TEXT} eq 'adhoc-signatures') {
            $Mildew::adhoc_sig = 0; 
            ();
        } else {
            XXX;
        }
    }
}
