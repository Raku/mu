
# A fairly complete (but almost certainly buggy) operator precedence parser

# method EXPR (%preclim = %LOOSEST, :$stop = &stdstopper) {
sub EXPR {
    my($cls,$Hpreclim,%args)=@_;
    $Hpreclim ||= $HLOOSEST;
    $args{stop} ||= \&stdstopper;
#    my $preclim = %preclim<prec>;
    my $preclim = $Hpreclim->{prec};
#    my $inquote is context = 0;
    local $inquote = 0;
#    if m:p/ <?before <$stop>> / {
#        return;
#    }
    #XXX
#    my $prevop is context<rw>;
#    my %thisop is context<rw>;
    local $prevop;
    local $Hthisop;
#    my @termstack;
#    my @opstack;
    my @termstack;
    my @opstack;

#    push @opstack, %terminator;         # (just a sentinel value)
#    push @termstack, $.expect_term();
    push @opstack, $Hterminator;         # (just a sentinel value)
    push @termstack, $cls->expect_term();

#    my sub reduce () {
    my $reduce = sub {
        #my $op = pop @opstack;
        my $op = pop @opstack;
        #given $op<assoc> {
	local $_ = $op->{assoc};
        #    when 'chain' {
        #        my @chain;
        #        push @chain, pop(@termstack);
        #        push @chain, $op;
        #        while @opstack {
        #            last if $op<prec> ne @opstack[-1]<prec>;
        #            push @chain, pop(@termstack);
        #            push @chain, pop(@opstack)<top>;
        #        }
        #        push @chain, pop(@termstack);
        #        $op<top><chain> = reverse @chain;
        #        push @termstack, $op<top>;
        #    }
	{
	    if($_ eq 'chain') {
                my @chain;
                push @chain, pop(@termstack);
                push @chain, $op;
                while (@opstack) {
                    last if $op->{prec} ne $opstack[-1]{prec};
                    push @chain, pop(@termstack);
                    push @chain, pop(@opstack)->{top};
                }
                push @chain, pop(@termstack);
                $op->{top}{chain} = [reverse @chain];
                push @termstack, $op->{top};
            }
        #    when 'list' {
        #        my @list;
        #        push @list, pop(@termstack);
        #        while @opstack {
        #            last if $op<top><sym> ne @opstack[-1]<top><sym>;
        #            push @list, pop(@termstack);
        #            pop(@opstack);
        #        }
        #        push @list, pop(@termstack);
        #        $op<top><list> = reverse @list;
        #        push @termstack, $op<top>;
        #    }
            elsif($_ eq 'list') {
                my @list;
                push @list, pop(@termstack);
                while @opstack {
                    last if $op->{top}{sym} ne $opstack[-1]{top}{sym};
                    push @list, pop(@termstack);
                    pop(@opstack);
                }
                push @list, pop(@termstack);
                $op->{top}{list} = [reverse @list];
                push @termstack, $op->{top};
            }
        #    default {
        #        $op<top><right> = pop @termstack;
        #        $op<top><left> = pop @termstack;
        #        push @termstack, $op<top>;
        #    }
            else {
                $op->{top}{right} = pop @termstack;
                $op->{top}{left} = pop @termstack;
                push @termstack, $op->{top};
            }
        }
    };

#    while not m:p/ <?before <$stop> > / {
     #XXX
    {
#        %thisop = ();
        $Hthisop = {};
#        my $infix := $.expect_tight_infix($preclim);
        my $infix := $cls->expect_tight_infix($preclim);
#        if not defined %thisop<prec> {
#            %thisop = %terminator;
#        }
        if(!defined $Hthisop->{prec}) {
            $Hthisop = $Hterminator;
        }
#        my Str $newprec = %thisop<prec>;
        my $newprec = $Hthisop->{prec};

#        # Does new infix (or terminator) force any reductions?
#        while @opstack[-1]<prec> lt $newprec {
#            reduce();
#        }
        # Does new infix (or terminator) force any reductions?
        while($opstack[-1]{prec} lt $newprec) {
            $reduce->();
        }

#        # Not much point in reducing the sentinels...
#        last if $newprec lt $LOOSEST;
        # Not much point in reducing the sentinels...
        last if $newprec lt $LOOSEST;

#        # Equal precedence, so use associativity to decide.
#        if @opstack[-1]<prec> eq $newprec {
#            given %thisop<assoc> {
#                when 'non'   { panic(qq["$infix" is not associative]) }
#                when 'left'  { reduce() }   # reduce immediately
#                when 'right' | 'chain' { }  # just shift
#                when 'list'  {              # if op differs reduce else shift
#                    reduce() if %thisop<top><sym> !eqv @opstack[-1]<top><sym>;
#                }
#                default { panic(qq[Unknown associativity "$_" for "$infix"]) }
#            }
#        }
        # Equal precedence, so use associativity to decide.
        if($opstack[-1]{prec} eq $newprec) {
            local $_ = $Hthisop->{assoc};
	    {
                if($_ eq 'non')     { panic(qq["$infix" is not associative]) }
                elsif($_ eq 'left') { $reduce->() }   # reduce immediately
                elsif($_ eq 'right' || $_ eq 'chain') { }  # just shift
                elsif($_ eq 'list') {              # if op differs reduce else shift
                    $reduce->() if $Hthisop=>{top}{sym} ne $opstack[-1]{top}{sym};
                }
                else { panic(qq[Unknown associativity "$_" for "$infix"]) }
            }
        }
#        push @opstack, %thisop;
        push @opstack, $Hthisop;
#        if m:p/ <?before <$stop>> / {
#            fail("$infix.perl() is missing right term");
#        }
	#XXX
#        %thisop = ();
#        push @termstack, $.expect_term();
        $Hthisop = {};
        push @termstack, $cls->expect_term();
    }
#    reduce() if @termstack > 1;
#    @termstack == 1 or panic("Internal operator parser error");
#    return @termstack[0];
    $reduce->() if @termstack > 1;
    @termstack == 1 or panic("Internal operator parser error");
    return $termstack[0];
}

