use v5.10;
use MooseX::Declare;
class VAST::package_declarator {
    use Mildew::AST::Helpers;
    method emit_m0ld {
        my $name  = $self->{package_def}{longname}[0]{name}{identifier}{TEXT};
        my $id_type_sub = Mildew::AST::unique_id;
    
        my %how_type = (
            knowhow => 'PrototypeHOW',
            class => 'ClassHOW',
            role => 'RoleHOW'
        );
        XXX unless $how_type{$self->{SYM}};

    
        
        my $init = $self->{package_def}{blockoid}->emit_m0ld;
    
        my $mold = Mildew::AST::Block->new(regs => $init->regs,stmts => [
            let(call(new=>FETCH(lookup("Package"))),sub {
                my $package = shift;
                let call("^!CREATE" => FETCH(lookup("p6opaque"))),sub {
                    my $proto = shift;
                    Mildew::AST::Seq->new(stmts => [
                        call(STORE => call(name => $package),[string $name]),
                        call(STORE => call("postcircumfix:{ }" => FETCH(call(outer => reg '$scope')),
                                      [string $name]),[$proto]),
                        call(STORE => call("postcircumfix:{ }" => FETCH(call outer => reg '$scope'),
                                      [string $name.'::']),[$package]),
                        let(call(new=>FETCH(lookup("Package"))), sub {
    			my $export = shift;
    			Mildew::AST::Seq->new(stmts => [
    					  call(STORE => call("postcircumfix:{ }"=>$package,[string 'EXPORT::']),
    					       [ $export ]),
    					  call(STORE => call("postcircumfix:{ }"=>$export,[string 'ALL::']),
    					       [ call(new=>FETCH(lookup('Package'))) ]),
    					  call(STORE => call("postcircumfix:{ }"=>$export,[string 'DEFAULT::']),
    					       [ call(new=>FETCH(lookup('Package'))) ]),
    				      ]);
    		    }),
                        call(STORE => call("postcircumfix:{ }" => FETCH(call(outer => reg '$scope')),
                                      [string '&'.$name]),[
    			     call(new => FETCH(lookup('Code')),[],[string 'outer'=>reg '$scope',,string 'signature'=>empty_sig(),string 'mold' => Mildew::AST::Block->new(regs=>['interpreter','scope'],stmts=>trailing_return([lookup('$?CLASS')]))])
                                      ]),
                        call(STORE => call("postcircumfix:{ }" => reg '$scope',[string '$?PACKAGE']),[$package]),
                        call(STORE => call("postcircumfix:{ }" => FETCH(call lookup => FETCH(call outer => reg '$scope'),[string '$?PACKAGE']),[string $name.'::']),[$package]),
                        call(STORE => call("postcircumfix:{ }" => reg '$scope',[string '$?CLASS']),[$proto]),
                        call(STORE => call("^!how" => $proto),[FETCH lookup($how_type{$self->{SYM}})]),
                        call(STORE => call("^!who" => $proto),[$package])
                    ]);
                };
            }),
            @{$init->stmts}
        ]);
        call("postcircumfix:( )" =>
    	 call(new => FETCH(lookup('Code')),[],[string 'outer'=>reg '$scope',string 'mold' => $mold,string 'signature'=>empty_sig()]),
    	 [capturize()]
            );
    }
}
