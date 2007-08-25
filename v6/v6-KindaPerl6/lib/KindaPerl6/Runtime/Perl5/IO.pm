# low level IO module

$::IO = make_class(name=>"IO",parent=>[$meta_Value],methods=>{
    zzz =>sub {
            my $self = shift; 
        },
});

1;
