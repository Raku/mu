# low level IO module

$::Kp6Security = make_class(name=>"Kp6Security",parent=>[$meta_Value],methods=>{
    guard_unsecure_code => sub {
                                if (Main::KP6_DISABLE_UNSECURE_CODE) {
                                    my ($package, $filename, $line) = caller(5);
                                    my $msg = "forbidden code at $filename line $line (".KP6_DISABLE_UNSECURE_CODE.")\n";
                                    $msg   .= "              ";
                                    die $msg;
                                } else {
                                    ::DISPATCH($::Int, "new", 1);
                                }
                               },
 });


1;
