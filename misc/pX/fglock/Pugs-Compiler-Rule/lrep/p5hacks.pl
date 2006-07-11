sub return_block_hack {
	my $tab = shift;
	my @s =   @_;
        if (@s && $s[-1] =~ /^#return-block#(.*)/s ) {
            #print "return block\n";
            my $code = $1;
            #print "Code: $code\n";
            pop @s;
            my $program;
            if ( @s == 1 ) {
                $program = $s[0];
            }
            else {
                $program = "$tab concat(\n" . 
                            ( join '', @s ) . 
                            "$tab ),\n";
            }
            #print "program $program\n";
            my $return;
            $return = "
    sub { 
        my \$rule = \n$program    ;
        my \$match = \$rule->( \@_ );
        return unless \$match;
        my \$capture_block = sub " . $code . "; 
        #use Data::Dumper;
        #print \"capture was: \", Dumper( \$match->{capture} );
        return { 
            \%\$match,
            capture => [ \$capture_block->( \$match ) ],
        }; 
    }\n";
            return $return;
        }
        return $s[0] if @s == 1;
        return "$tab concat(\n" . 
               ( join '', @s ) . 
               "$tab ),\n";
}

1;

