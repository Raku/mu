# low level IO module

$::IO = make_class(name=>"IO",parent=>[$meta_Value],methods=>{
    mkdir => sub {
                  die "forbidden code" if $ENV{KP6_DISABLE_UNSECURE_CODE};
                  my $self = shift;
                  my $dirname = $_[0]{_value};
                  ::DISPATCH($::Int, "new", mkdir($dirname) ? 1 : 0 );
                 },
    rmdir => sub {
                  die "forbidden code" if $ENV{KP6_DISABLE_UNSECURE_CODE};
                  my $self = shift;
                  my $dirname = $_[0]{_value};
                  ::DISPATCH($::Int, "new", rmdir($dirname) ? 1 : 0 );
                 },
 });


1;
