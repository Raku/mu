
@jsfiles = map{chomp;/\.js$/ ? ($_) : ()} sort `find mozilla_js_tests`;
print "plan ".@jsfiles."\n";
foreach $jsfile (@jsfiles) {
    print STDERR "# $jsfile\n";
    my $cmd = '../../pugs -I . -e \'use Grammar; if slurp("'.$jsfile.'") ~~ /^<JavaScript::ECMAScript3::Grammar::Spec::Program>$/ {say "ok"}else{say "not ok"}\' 2>&1';
    print $cmd,"\n";
    my $out = `$cmd`;
    print $out;
    exit;
}
