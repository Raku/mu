# low level Math module

$::Math = make_class(name=>"Math",parent=>[$meta_Num],methods=>{
    srand => sub {
                  my $self = shift;
                  my $seed = $_[0]{_value};
                  ::DISPATCH($::Num, "new", srand($seed));
                 },
    rand => sub {
                 my $self = shift;
                 my $expr = $_[0]{_value} || 1;
                 ::DISPATCH($::Num, "new", rand($expr));
                },
    Inf => sub {
                 my $self = shift;
                 my $expr = 9 ** 9 ** 9;
                 ::DISPATCH($::Num, "new", $expr );
                },
 });

1;
