# Porting gimme5 EXPR to elf.
# Plan:
# get EXPR compilable under elf; runable with context jig; '3;' with actual context.
# forget about mirroring STD.pm code, let alone running it unmodified.
# automate convertion as possible - typos are death.
# Todo:
# comment out debugging.  worry about it later.
# push @foo, bar; etal -> @foo.push(bar);  non-multi dispatch, and better elf support.
# inline $mumble._FOO stuff from Cursor(?).
# work around assumption of cursorness.
# simplify loop constructs
method EXPR ($preclvl)
{
#ELF
#    temp $CTX = self.callm if $*DEBUG +& DEBUG::trace_call;
#    if self.peek {
#        return self._AUTOLEXpeek('EXPR', $retree);
#    }
#   my $preclim = $preclvl ?? $preclvl.<prec> // $LOOSEST !! $LOOSEST; #ELFBUG
    my $preclim; if $preclvl { $preclim = $preclvl.<prec> }; #ELFFIX
    if not(defined($preclim)) { $preclim = $LOOSEST }; #ELFFIX
    my $inquote is context = 0;
    my $SIGIL is context<rw> = '';
    my @termstack;
    my @opstack;
    my $termish = 'termish';

#    push @opstack, { 'O' => item %terminator, 'sym' => '' };         # (just a sentinel value)#ELFBUG
    my $sentinel = {}; $sentinel<O> = %terminator; $sentinel<sym> = ''; #ELFFIX

#    my $here = self;
    my $S = parser_pos();
#    self.deb("In EXPR, at $S") if $*DEBUG +& DEBUG::EXPR;

    my $reduce = sub {#
#        self.deb("entering reduce, termstack == ", +@termstack, " opstack == ", +@opstack) if $*DEBUG +& DEBUG::EXPR;
#        my $op = pop @opstack;
        my $op = @opstack.pop;#
        my $sym = $op<sym>;
#        given $op<O><assoc> // 'unary' {
        my $assoc = $op<O><assoc> || 'unary';#
#            when 'chain' {
        if $assoc eq 'chain' {#
#                self.deb("reducing chain") if $*DEBUG +& DEBUG::EXPR;
                my @chain;
#                push @chain, pop(@termstack);
                @chain.push(@termstack.pop);#
#                push @chain, $op;
                @chain.push($op);
                while @opstack.elems {
#                    last if $op<O><prec> ne @opstack[*-1]<O><prec>;
                    last if $op<O><prec> ne @opstack[-1]<O><prec>;#
#                    push @chain, pop(@termstack);
#                    push @chain, pop(@opstack);
                    @chain.push(@termstack.pop);#
                    @chain.push(@opstack.pop);#
                }
#                push @chain, pop(@termstack);
#                @chain = reverse @chain if @chain > 1;
                my $startpos = @chain[0].pos;
                my $nop = $op.cursor_fresh();
                $nop<chain> = [@chain];
                $nop<_arity> = 'CHAIN';
                @termstack.push($nop._REDUCE($startpos, 'EXPR'));
            }
#            when 'list' {
        elsif $assoc eq 'list' {
#                self.deb("reducing list") if $*DEBUG +& DEBUG::EXPR;
                my @list;
                my @delims = $op;
                @list.push(@termstack.pop);
                while @opstack {
#                    self.deb($sym ~ " vs " ~ @opstack[*-1]<sym>) if $*DEBUG +& DEBUG::EXPR;
                    last if $sym ne @opstack[-1]<sym>;
                    if @termstack and defined @termstack[0] {
                        @list.push(@termstack.pop);
                    }
                    else {
                        self.worry("Missing term in " ~ $sym ~ " list");
                    }
                    @delims.push(@opstack.pop);
                }
                if @termstack and defined @termstack[0] {
                    @list.push(@termstack.pop);
                }
                elsif $sym ne ',' {
                    self.worry("Missing final term in '" ~ $sym ~ "' list");
                }
                @list = @list.reverse() if @list > 1;
                my $startpos = @list[0].pos;
                @delims = @delims.reverse() if @delims > 1;
                my $nop = $op.cursor_fresh();
                $nop<sym> = $sym;
                $nop<O> = $op<O>;
                $nop<list> = [@list];
                $nop<delims> = [@delims];
                $nop<_arity> = 'LIST';
                @termstack.push($nop._REDUCE($startpos, 'EXPR'));
            }
#            when 'unary' {
        elsif $assoc eq 'unary' {
#                self.deb("reducing") if $*DEBUG +& DEBUG::EXPR;
                my @list;
#                self.deb("Termstack size: ", +@termstack) if $*DEBUG +& DEBUG::EXPR;

#                self.deb($op.dump) if $*DEBUG +& DEBUG::EXPR;
                $op<arg> = (pop @termstack);
                if ($op<arg><_from> < $op<_from>) {
                    $op<_from> = $op<arg><_from>;
                }
                if ($op<arg><_pos> > $op<_pos>) {
                    $op<_pos> = $op<arg><_pos>;
                }
                $op<_arity> = 'UNARY';
                @termstack.push($op._REDUCE($op<_from>, 'EXPR'));
            }
#            default {
        else {
#                self.deb("reducing") if $*DEBUG +& DEBUG::EXPR;
                my @list;
#                self.deb("Termstack size: ", +@termstack) if $*DEBUG +& DEBUG::EXPR;

                $op<right> = (pop @termstack);
                $op<left> = (pop @termstack);
                $op<_from> = $op<left><_from>;
                $op<_pos> = $op<right><_pos>;
                $op<_arity> = 'BINARY';
#                self.deb($op.dump) if $*DEBUG +& DEBUG::EXPR;
                @termstack.push($op._REDUCE($op<_from>, 'EXPR'));
            }
        }
    };

  TERM:
    loop {
#        self.deb("In loop, at ", $here.pos) if $*DEBUG +& DEBUG::EXPR;
        my $oldpos = $here.pos;
        $here = $here.cursor_fresh();
        $SIGIL = @opstack[-1]<O><prec> gt $item_assignment_prec ?? '@' !! '';
        my @t = $here.$termish;

        if not @t or not $here = @t[0] or ($here.pos == $oldpos and $termish eq 'termish') {
            return ();
            # $here.panic("Failed to parse a required term");
        }
        $termish = 'termish';

        # interleave prefix and postfix, pretend they're infixish
        my $M = $here;

        # note that we push loose stuff onto opstack before tight stuff
        my @pre;
        my $tmp;
        @pre = @$tmp if $tmp = ( $M<PRE> :delete );
        my @post;
        @post = @$tmp.reverse() if $tmp = ( $M<POST> :delete );
        while @pre and @post {
            my $postO = @post[0]<O>;
            my $preO = @pre[0]<O>;
            if $postO<prec> lt $preO<prec> {
                @opstack.push(@post.shift);
            }
            elsif $postO<prec> gt $preO<prec> {
                @opstack.push(@pre.shift);
            }
            elsif $postO<uassoc> eq 'left' {
                @opstack.push(@post.shift);
            }
            elsif $postO<uassoc> eq 'right' {
                @opstack.push(@pre.shift);
            }
            else {
                $here.panic('"' ~ @pre[0]<sym> ~ '" and "' ~ @post[0]<sym> ~ '" are not associative');
            }
        }
        @opstack.push(@pre.flatten,@post.flatten);

        @termstack.push($here);
#        self.deb("after push: " ~ (0+@termstack)) if $*DEBUG +& DEBUG::EXPR;

        loop {     # while we see adverbs
            $oldpos = $here.pos;
            last TERM if (@+MEMOS[$oldpos]<endstmt> // 0) == 2;
            $here = $here.cursor_fresh.ws;
            my @infix = $here.cursor_fresh.infixish();
            last TERM unless @infix;
            my $infix = @infix[0];
            last TERM unless $infix.pos > $oldpos;
            
            if not $infix<sym> {
#                die $infix.dump if $*DEBUG +& DEBUG::EXPR;
            }

            my $inO = $infix<O>;
            my Str $inprec = $inO<prec>;
            if not defined $inprec {
#                self.deb("No prec given in infix!") if $*DEBUG +& DEBUG::EXPR;
#                die $infix.dump if $*DEBUG +& DEBUG::EXPR;
                $inprec = %terminator<prec>;
            }

            last TERM unless $inprec gt $preclim;

            $here = $infix.cursor_fresh.ws();

            # substitute precedence for listops
            $inO<prec> = $inO<sub> if $inO<sub>;

            # Does new infix (or terminator) force any reductions?
            while @opstack[-1]<O><prec> gt $inprec {
                reduce();
            }

            # Not much point in reducing the sentinels...
            last if $inprec lt $LOOSEST;

        if $infix<fake> {
            my $adverbs = @termstack[-1]<ADV> ||= [];
            push @$adverbs, $infix<colonpair>;
            next;  # not really an infix, so keep trying
        }

            # Equal precedence, so use associativity to decide.
            if @opstack[-1]<O><prec> eq $inprec {
              my $assoc = $inO<assoc>;
#                given $inO<assoc> {
#                    when 'non'   { $here.panic('"' ~ $infix.text ~ '" is not associative') }
              if $assoc eq 'non' { $here.panic('"' ~ $infix.text ~ '" is not associative') }
#                    when 'left'  { reduce() }   # reduce immediately
              elsif $assoc eq 'left' { $reduce.() }
#                    when 'right' { }            # just shift
              elsif $assoc eq 'right' { }
#                    when 'chain' { }            # just shift
              elsif $assoc eq 'chain' { }
#                    when 'unary' { }            # just shift
              elsif $assoc eq 'unary' { }
#                    when 'list'  {              # if op differs reduce else shift
              elsif $assoc eq 'list' {
                        $reduce.() if not($infix<sym> eq @opstack[-1]<sym>);
                    }
#                    default { $here.panic('Unknown associativity "' ~ $_ ~ '" for "' ~ $infix<sym> ~ '"') }
              else { $here.panic('Unknown associativity "' ~ $assoc ~ '" for "' ~ $infix<sym> ~ '"') }
                }
            }

            $termish = $inO<nextterm> if $inO<nextterm>;
            @opstack.push($infix);              # The Shift
            last;
        }
    }
    reduce() while @opstack.elems > 1;
    if @termstack.elems {
        #@termstack.elems == 1 or $here.panic("Internal operator parser error, termstack == " ~ (+@termstack));
        @termstack[0]<_from> = self.pos;
        @termstack[0]<_pos> = $here.pos;
    }
    self._MATCHIFYr($S, "EXPR", @termstack);
}
