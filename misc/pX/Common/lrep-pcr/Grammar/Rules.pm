package Grammar::Perl6;
use Pugs::Grammar::Rule;
*{'rule_decl'} = 

    sub { 
	print __FILE__ . __LINE__ . "\n" if $::trace;
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "rule" )
       ,
         Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
       ,
         Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\{" )
       ,
         Runtime::Perl5::RuleOps::capture( 'Pugs::Grammar::Rule::rule', sub{ Pugs::Grammar::Rule->rule(@_)} )
       ,
         Runtime::Perl5::RuleOps::constant( "\}" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { rule_decl =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&rule_decl;
