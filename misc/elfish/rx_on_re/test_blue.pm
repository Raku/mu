#use nodes;
#use analysis;
#use emit5;

sub inline_remains is p5 {'use remains_of_Regexp_ModuleA;
'}
inline_remains();

sub test is p5 {'

   my $test_target;
   my $test_target6;

   if( 1 ) { # $ARGV[-1] eq "--test-blue") {
     shift;
     my $env = undef; #{package main; sub{my$s=eval($_[0]);Carp::carp($@)if$@;$s}};
     $test_target =
       sub {
         my($mods,$re)=@_;
         my $mod1 = $mods ? "(?$mods)" : "";
         #my $code = "m:P5/$mod1$re/";
         my $code = "m/:P5 $mod1$re/";
         print STDERR $code,"\n";
         my $o = GLOBAL::eval($code,$env);
         sub{my($s)=@_;$o->match($s)}
       };
     $test_target6 =
       sub {
         my($mods,$re)=@_;
         my $code = "m/$mods"."::$re/";
         print STDERR $code,"\n";
         my $o = GLOBAL::eval($code,$env);
         sub{my($s)=@_;$o->match($s)}
       };
   }

   print "re_tests\n\n";
   require "./t/re_tests.pl";
   Pkg_re_tests::test($test_target);

   print "\n\n";
   print "rx_tests\n\n";
   require "./t/rx.pl";
   Pkg_re_tests::test6($test_target6);
'}
test();
