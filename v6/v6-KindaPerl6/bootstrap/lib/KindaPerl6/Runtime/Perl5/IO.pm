# low level IO module

$::IO = make_class(name=>"IO",parent=>[$meta_Value],methods=>{
    mkdir => sub {
                  $::Kp6Security->guard_unsecure_code;
                  my $self = shift;
                  my $dirname = $_[0]{_value};
                  ::DISPATCH($::Int, "new", mkdir($dirname) ? 1 : 0 );
                 },
    rmdir => sub {
                  $::Kp6Security->guard_unsecure_code;
                  my $self = shift;
                  my $dirname = $_[0]{_value};
                  ::DISPATCH($::Int, "new", rmdir($dirname) ? 1 : 0 );
                 },
 });


1;
