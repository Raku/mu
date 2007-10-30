# low level IO module

$::IO = KindaPerl6::Runtime::Perl5::MOP::make_class(name=>"IO",parent=>[$meta_Value],methods=>{
    mkdir => sub {
                  $::Kp6Security->guard_insecure_code;
                  my $self = shift;
                  my $dirname = GLOBAL::_str($_[0]);
                  ::DISPATCH($::Int, "new", mkdir($dirname) ? 1 : 0 );
                 },
    rmdir => sub {
                  $::Kp6Security->guard_insecure_code;
                  my $self = shift;
                  my $dirname = GLOBAL::_str($_[0]);
                  ::DISPATCH($::Int, "new", rmdir($dirname) ? 1 : 0 );
                 },
 });


1;
