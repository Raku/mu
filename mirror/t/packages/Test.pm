
package Our::Test;

sub ns  { "Our::Test" }

sub pkg { $?PACKAGE }

sub test_export is export { "party island" }

sub get_our_pkg {
    Our::Package::pkg();
}

# these are currently parsefail - hence the eval
eval '
our package Our::Package {

    sub pkg { $?PACKAGE }

}
';

sub cant_see_pkg {
    return My::Package::pkg();
}

eval '
{
    sub my_pkg {
	return My::Package::pkg();
    }

    my package My::Package {
	sub pkg { $?PACKAGE }
    }

}
';

sub dummy_sub_with_params($arg1, $arg2) is export { "[$arg1] [$arg2]" }
