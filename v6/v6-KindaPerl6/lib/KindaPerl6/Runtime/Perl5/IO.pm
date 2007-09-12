# low level IO module

$::IO = make_class(name=>"IO",parent=>[$meta_Value],methods=>{
    mkdir => sub {
                  $::Kp6Security->guard_insecure_code;
                  my $self = shift;
                  my $dirname = ::DISPATCH( $_[0], 'str' )->{_value};
                  ::DISPATCH($::Int, "new", mkdir($dirname) ? 1 : 0 );
                 },
    rmdir => sub {
                  $::Kp6Security->guard_insecure_code;
                  my $self = shift;
                  my $dirname = ::DISPATCH( $_[0], 'str' )->{_value};
                  ::DISPATCH($::Int, "new", rmdir($dirname) ? 1 : 0 );
                 },
 });


1;
