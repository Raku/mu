package sm0p_actions;
use Moose;
use autobox::Core;
use Data::Dumper;
sub make($) {
    $_->{''} = $_[0];
}
sub adapter {
    if (ref $_[0] eq 'ARRAY') {
        [map {$_->item} @{$_[0]}];
    } elsif ($_[0]) {
        $_[0]->item;
    } else {
        $_[0]
    }
}
sub TOP__sm0p_0 {
my $self=shift;
make adapter($_->{name}) . " = " . adapter($_->{frame}) . ';'}
sub frame__sm0p_1 {
my $self=shift;
 make 'SMOP_DISPATCH(interpreter, '
      . 'SMOP__SLIME__Frame, SMOP__ID__new, SMOP__NATIVE__capture_create('
      . 'interpreter, SMOP__SLIME__Frame, (SMOP__Object*[]){ '
      . adapter($_->{nodes}) . ' }, NULL))' }
sub nodes__sm0p_2 {
my $self=shift;
push(@smop::nodes_stack,[$smop::node_counter,$smop::labels]);$smop::node_counter=0;$smop::labels={};}
sub nodes__sm0p_3 {
my $self=shift;
 if (adapter($_->{node})->elems) { make adapter($_->{node})->join(', ') . ', NULL'} else { make '' }}
sub nodes__sm0p_4 {
my $self=shift;
my $prev=pop(@smop::nodes_stack);$smop::node_counter=$prev->[0];$smop::labels=$prev->[1];}
sub label__sm0p_5 {
my $self=shift;
$smop::labels->{adapter($_->{name})} = $smop::node_counter;}
sub node__sm0p_6 {
my $self=shift;
 make adapter($_->{node_empty}) . '' }
sub node__sm0p_7 {
my $self=shift;
 make adapter($_->{node_result}) . '' }
sub node__sm0p_8 {
my $self=shift;
 make adapter($_->{node_capturized}) . '' }
sub node__sm0p_9 {
my $self=shift;
 make adapter($_->{node_full}) . '' }
sub node__sm0p_10 {
my $self=shift;
$smop::node_counter++;}
sub node_empty__sm0p_11 {
my $self=shift;
 make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
      . ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, NULL))' }
sub node_full__sm0p_12 {
my $self=shift;
 make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
      . ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
      . ' SMOP__ID__responder, SMOP_REFERENCE(interpreter,(SMOP__Object*)SMOP_RI(' . adapter($_->{responder}) . ')), '
      . ' SMOP__ID__identifier, SMOP_REFERENCE(interpreter,' . adapter($_->{identifier}) . '), '
      . ' SMOP__ID__capture, SMOP__NATIVE__capture_create(interpreter, '
      . 'SMOP_REFERENCE(interpreter,' . (adapter($_->{invocant}) ? adapter($_->{invocant}) : adapter($_->{responder})) . '), '. adapter($_->{positional}) .', '. adapter($_->{named}) .') '
      . ' , NULL  }))' }
sub node_capturized__sm0p_13 {
my $self=shift;
 make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
      . ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
      . ' SMOP__ID__responder, SMOP_REFERENCE(interpreter,(SMOP__Object*)SMOP_RI(' . adapter($_->{responder}) . ')), '
      . ' SMOP__ID__identifier, SMOP_REFERENCE(interpreter,' . adapter($_->{identifier}) . '), '
      . ' SMOP__ID__capture, ' . adapter($_->{identifier2}) 
      . ' , NULL  }))' }
sub node_result__sm0p_14 {
my $self=shift;
 make 'SMOP_DISPATCH(interpreter, SMOP__SLIME__Node, SMOP__ID__new, '
      . ' SMOP__NATIVE__capture_create(interpreter, SMOP__SLIME__Node, NULL, (SMOP__Object*[]){'
      . ' SMOP__ID__result, ' . adapter($_->{value}) 
      . ' , NULL}))' }
sub invocant__sm0p_15 {
my $self=shift;
 make adapter($_->{identifier}) . '' }
sub responder__sm0p_16 {
my $self=shift;
 make adapter($_->{identifier}) . '' }
sub positional__sm0p_17 {
my $self=shift;
 make '(SMOP__Object*[]){' . adapter($_->{positionals}) . '}'  }
sub positional__sm0p_18 {
my $self=shift;
 make ' NULL' }
sub nativestring__sm0p_19 {
my $self=shift;
 make 'SMOP__S1P__Str_create("' . $_->{0}->item . '")' }
sub value__sm0p_20 {
my $self=shift;
 make adapter($_->{frame}) }
sub value__sm0p_21 {
my $self=shift;
 make adapter($_->{nativestring}) }
sub value__sm0p_22 {
my $self=shift;
 make adapter($_->{nativeint}) }
sub value__sm0p_23 {
my $self=shift;
 make adapter($_->{capturize}) }
sub value__sm0p_24 {
my $self=shift;
 make 'SMOP_REFERENCE(interpreter,' . adapter($_->{identifier}) . ')' }
sub value__sm0p_25 {
my $self=shift;
 
        die "undefined label " . adapter($_->{name}) ."\n" unless defined $smop::labels->{adapter($_->{name})};
        make 'SMOP__NATIVE__int_create(' . ($smop::node_counter - $smop::labels->{adapter($_->{name})}) . ')'
      }
sub positionals__sm0p_26 {
my $self=shift;
 make adapter($_->{value}) . ',' . adapter($_->{positionals})  }
sub positionals__sm0p_27 {
my $self=shift;
 make adapter($_->{value}) . ', NULL' }
sub named__sm0p_28 {
my $self=shift;
 make '(SMOP__Object*[]){' . adapter($_->{pairs}) . '}'  }
sub named__sm0p_29 {
my $self=shift;
 make ' NULL' }
sub pairs__sm0p_30 {
my $self=shift;
 make adapter($_->{pair}) . ', ' . adapter($_->{pairs}) }
sub pairs__sm0p_31 {
my $self=shift;
 make adapter($_->{pair}) . ', NULL' }
sub pair__sm0p_32 {
my $self=shift;
 make adapter($_->{identifier}) . ', ' . adapter($_->{identifier2}) }
sub identifier2__sm0p_33 {
my $self=shift;
 make adapter($_->{identifier}) . '' }
sub identifier__sm0p_34 {
my $self=shift;
 make adapter($_->{name}) . '' }
sub identifier__sm0p_35 {
my $self=shift;
 my $id=$_->{0}->item;$id =~ s/\\]/]/g;make 'SMOP__NATIVE__idconst_create("' . $id . '")' }
sub identifier__sm0p_36 {
my $self=shift;
 make adapter($_->{idconst}) . ''}
sub identifier__sm0p_37 {
my $self=shift;
 make 'SMOP__NATIVE__idconst_create("' . adapter($_->{name}) . '")' }
sub idconst__sm0p_38 {
my $self=shift;
 make 'SMOP__ID__' . adapter($_->{idconst_list}) . '' }
sub capturize__sm0p_39 {
my $self=shift;
 make 'SMOP__SLIME__Capturize_create(' . adapter($_->{cint1}) . ','
      . adapter($_->{cintlist1}) . ',' . adapter($_->{cintlist2}) . ','
      . adapter($_->{cint2}) . ')' }
sub cint1__sm0p_40 {
my $self=shift;
 make adapter($_->{cint}) . '' }
sub cint2__sm0p_41 {
my $self=shift;
 make adapter($_->{cint}) . '' }
sub cint__sm0p_42 {
my $self=shift;
 make adapter($_->{digits}) . '' }
sub cint__sm0p_43 {
my $self=shift;
 make ($smop::node_counter - $smop::labels->{adapter($_->{name})}) }
sub cintlist1__sm0p_44 {
my $self=shift;
 make adapter($_->{cintlist}) . '' }
sub cintlist2__sm0p_45 {
my $self=shift;
 make adapter($_->{cintlist}) . '' }
sub cintlist__sm0p_46 {
my $self=shift;
make '(int[]){ '. adapter($_->{cintlistbody}) . ' }' }
sub cintlist__sm0p_47 {
my $self=shift;
make 'NULL'}
sub cintlist__sm0p_48 {
my $self=shift;
make 'NULL'}
sub cintlistbody__sm0p_49 {
my $self=shift;
 make adapter($_->{cint}) . ', ' . adapter($_->{cintlistbody}) }
sub cintlistbody__sm0p_50 {
my $self=shift;
 make adapter($_->{cint}) . ', 0 '}
sub nativeint__sm0p_51 {
my $self=shift;
 make 'SMOP__NATIVE__int_create(' . adapter($_->{digits}) . ')' }
sub nativeint_list__sm0p_52 {
my $self=shift;
make '(SMOP__Object*[]){ '. adapter($_->{nativeint_list_body}) . ' }' }
sub nativeint_list__sm0p_53 {
my $self=shift;
make 'NULL'}
sub nativeint_list_body__sm0p_54 {
my $self=shift;
 make adapter($_->{nativeint}) . ', ' . adapter($_->{nativeint_list_body}) }
sub nativeint_list_body__sm0p_55 {
my $self=shift;
 make adapter($_->{nativeint}) . ', NULL '}
1;
