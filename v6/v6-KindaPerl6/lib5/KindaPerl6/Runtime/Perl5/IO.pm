# low level IO module

$::IO = make_class(name=>"IO",parent=>[$meta_Value],methods=>{
    mkdir => sub {
                  my $self = shift;
                  my $dirname = $_[0]{_value};
                  ::DISPATCH($::Int, "new", mkdir($dirname) ? 1 : 0 );
                 },
    rmdir => sub {
                  my $self = shift;
                  my $dirname = $_[0]{_value};
                  ::DISPATCH($::Int, "new", rmdir($dirname) ? 1 : 0 );
                 },
 });


1;
