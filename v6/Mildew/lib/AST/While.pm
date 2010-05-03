use v5.10;
use MooseX::Declare;
class AST::While extends AST::Base {
    use AST::Helpers;
    has 'cond' => (is => 'ro');
    has 'body' => (is => 'ro');

    method m0ld {
        my $id_cond = AST::unique_id;
        my $start = AST::unique_label;
        my $label_start = AST::unique_label;
        my $label_end = AST::unique_label;
        my $label_body = AST::unique_label;
    
        $label_start . ":" . $self->cond->m0ld($id_cond) . "\n" .
        'my '.$id_cond.'_val = '.$id_cond.'."FETCH"();'."\n".
        'my '.$id_cond.'_bool = '.$id_cond.'_val."true"();'."\n".
        'if '.$id_cond.'_bool { goto '.$label_body.' } else { goto '.$label_end.' };'."\n".
        $label_body.": " .$self->body->m0ld('$void') ."\n".
        "goto $label_start;\n".
        $label_end.": noop;\n";
    }

    method pretty {
        'while ' . $self->cond->pretty . " {\n"
        . AST::indent($self->body->pretty) . "\n"
        . "}\n";
    }

    method simplified {

        my ($cond,@cond_setup) = call(true => FETCH($self->cond))->simplified;
        my ($ret,@body_setup) = $self->body->simplified;

        my $branch = AST::Branch->new(cond=>$cond);

        my $cond_block = AST::Seq->new(id=>AST::unique_label,stmts=>[@cond_setup,$branch]);
        my $body = AST::Seq->new(id=>AST::unique_label,stmts=>[
            @body_setup,
            AST::Goto->new(block=>$cond_block)
        ]);

        my $end = AST::Seq->new(id=>AST::unique_label,stmts=>[]);
        
        $branch->then($body);
        $branch->else($end);

        ($ret,$cond_block,$body,$end);
    }
}
