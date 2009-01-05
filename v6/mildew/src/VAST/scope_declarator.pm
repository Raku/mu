package VAST::scope_declarator;
use utf8;
use strict;
use warnings;
use AST::Helpers;

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
	AST::Seq->new(stmts => [
			  call(STORE=>call(name=>$attribute),[string $name]),
			  call(STORE=>call(private_name=>$attribute),[string $private_name]),
			  call(STORE=>call(container_type=>$attribute),[FETCH(lookup($container_type))]),
			  call(FETCH=>$attribute)
		      ]);
    };
}

sub accessor {
    my $var_decl = shift;
    my $priv = bless { twigil => [ { sym => '!', TEXT => '!' } ],
		       sigil => { TEXT => $var_decl->{variable}{sigil}{TEXT} },
		       desigilname => $var_decl->{variable}{desigilname} }, 'VAST::variable';
    call(new=>FETCH(lookup('Code')),[],
	 [ string 'outer' => reg '$scope',
	   string 'mold' => 
	   AST::Block->new(regs => ['interpreter','scope'],
			   stmts => trailing_return([ $priv->emit_m0ld ])) ]);
}

sub VAST::scope_declarator::emit_m0ld {
    my $m = shift;
    if ($m->{'sym'} eq 'my' || $m->{'sym'} eq 'our') {
        if (my $decl = $m->{scoped}{declarator}) {
            if (my $var_decl = $decl->{variable_declarator}) {
		let call(new => FETCH(lookup 'Scalar')), sub {
		    my $value = shift;
		    AST::Seq->new(stmts => [
				      ( $m->{'sym'} eq 'our' ? 
				      call(BIND => (call 'postcircumfix:{ }' => FETCH(lookup '$?PACKAGE'),
						    [ string varname($var_decl->{variable}) ]),[$value])
				      :()),
				      call(BIND => (call 'postcircumfix:{ }' => reg('$scope'),
						    [ string varname($var_decl->{variable}) ]),[$value])
				  ]);

		}
            } elsif (my $routine_decl = $decl->{routine_declarator}) {
		$routine_decl->{routine_def}->emit_m0ld($m->{sym});
            } else {
                XXX('unknown scope declarator');
            }
        } else {
            XXX('scoped declarator without a declarator');
        }
    } elsif ($m->{sym} eq 'has') {
	# attribute!
	my $var_decl = $m->{scoped}{declarator}{variable_declarator};
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

	    AST::Seq->new
		(stmts => [
		     call(add_attribute =>
			  FETCH(call '^!how' => $CLASS),
			  [$CLASS,
			   string varname($var_decl->{variable}),
			   attribute($var_decl)]),
		     @additional
		 ]);
	};
    } else {
        XXX('unknown sym in scope declarator');
    }
}

1;
