use v6;

use Test;

# plain http link
# L<http://groups.google.com/first>

plan 75;

# Link so Synopsis
# L<S29/Scalar/"=item undef">

is(undef, undef, "undef is equal to undef");
ok(!defined(undef), "undef is not defined");

{
    my $a;
    ok(!defined($a), "uninitialized lexicals are undef");
}

# Another plain http link
# L<http://groups.google.com/second>

# Link to Synopsys in subdirectory
# L<S32::Abstraction/Numbers/"=item available">
{
    my @ary = "arg1";
    my $a = @ary.pop;
    ok(defined($a), "pop from array");
    $a = @ary.pop;
    ok(!defined($a), "pop from empty array");
}

# Multiline smartlink 
# L<S05/Match objects/"they will all be undefined" closure
#                                 "let keyword">

{
    my ($num, $alpha);
    my ($rx1, $rx2);
    eval '
        $rx1 = rx
        ';
        ok(defined($num), '{$_}: successful hypothetical');
}

# subroutines
{
    sub bar ($bar, $baz?, :$quux) {
        is($bar, "BAR", "defined param"); # sanity

        # internal legacy smartlink
        # L<<S06/Optional parameters/Missing optional arguments>>
        ok(!defined($baz), "unspecified optional param");

        # internal Synopsys smartlink
        # L<S06/Named parameters/Named parameters are optional>
        ok(!defined($quux), "unspecified optional param");
    }

    bar("BAR");

}

# Smartlink to some other file (not synopsys) but either Module.pm or Module.pod 
# L<Module/Autoloading>

flunk('FIXME: parsefail');

# Smartlink to some other file (not synopsys) but either Module.pm or Module.pod 
# L<Module::Name/Autoloading>
is((undef) + 1, 1, 'undef + 1');
is(1 + (undef), 1, '1 + undef');

# another http link
# L<"http://colabti.de/irclogger/irclogger_log/perl6?date=2006-09-12,Tue&sel=145#l186">
is ?(@(undef,)), Bool::False, '?(@(undef,)) is false';


# L<Module/Something else>
is ?(list(undef,)), Bool::False, '?(@(undef,)) is false';

