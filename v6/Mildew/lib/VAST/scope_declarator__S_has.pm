use v5.10;
use MooseX::Declare;
use utf8;
class VAST::scope_declarator__S_has {
    use Mildew::AST::Helpers;
    sub accessor {
        my $var_decl = shift;
        #XXX hack
        my $priv = bless { twigil => [ { SYM => '!', TEXT => '!' } ],
    		       sigil => { TEXT => $var_decl->{variable}{sigil}{TEXT} },
    		       desigilname => $var_decl->{variable}{desigilname} }, 'VAST::variable';
    
        my $sig = Mildew::AST::Call->new
            ( identifier => string 'new',
              capture => Mildew::AST::Capture->new
              ( invocant => FETCH(lookup('AdhocSignature')),
                positional => [],
                named =>
                [ string 'BIND' => Mildew::AST::Block->new
                  ( regs => [qw(interpreter scope capture)],
                    stmts => trailing_return([call BIND => (call 'postcircumfix:{ }' => reg '$scope',[string '$¿self']),[call positional => reg '$capture',[integer 0]]]))]));
    
        call(new=>FETCH(lookup('Code')),[],
    	 [ string 'outer' => reg '$scope',
               string 'signature' => $sig,
    	   string 'mold' => 
    	   Mildew::AST::Block->new(regs => ['interpreter','scope'],
    			   stmts => trailing_return([ $priv->emit_m0ld ])) ]);
    }
    sub attribute {
        my $var_decl = shift;
        let call(new=>FETCH(lookup('Attribute'))), sub {
    	my $attribute = shift;
    	my $container_type;
    	my $sigil = $var_decl->{variable}{sigil}{TEXT};
    	if ($sigil eq '$') {
    	    $container_type = 'Scalar';
    	} elsif ($sigil eq '@') {
    	    $container_type = 'Array';
    	} elsif ($sigil eq '%') {
    	    $container_type = 'Hash';
    	} else {
    	    XXX;
    	}
    	my $twigil = $var_decl->{variable}{twigil}[0]{TEXT};
    	my $private_name = $sigil.'!'.$var_decl->{variable}{desigilname}{longname}{name}{identifier}{TEXT};
    	my $name = $sigil.$twigil.$var_decl->{variable}{desigilname}{longname}{name}{identifier}{TEXT};
    	Mildew::AST::Seq->new(stmts => [
    			  call(STORE=>call(name=>$attribute),[string $name]),
    			  call(STORE=>call(private_name=>$attribute),[string $private_name]),
    			  call(STORE=>call(container_type=>$attribute),[FETCH(lookup($container_type))]),
    			  call(FETCH=>$attribute)
    		      ]);
        };
    }
    method emit_m0ld {
	my $var_decl = $self->{scoped}{declarator}{variable_declarator};
	let FETCH(lookup('$?CLASS')), sub {
	    my $CLASS = shift;

	    my @additional;
	    if ($var_decl->{variable}{twigil}[0]{TEXT} eq '.') {
		# add a public accessor.
		push @additional,


		call(add_method => FETCH(call '^!how' => $CLASS),
		     [$CLASS,
		      string $var_decl->{variable}{desigilname}{longname}{name}{identifier}{TEXT},
		      accessor($var_decl)]);

	    }

	    Mildew::AST::Seq->new
		(stmts => [
		     call(add_attribute =>
			  FETCH(call '^!how' => $CLASS),
			  [$CLASS,
			   string varname($var_decl->{variable}),
			   attribute($var_decl)]),
		     @additional
		 ]);
	};
    }
}
