module Algorithm::NaiveBayes::Model::Discrete-0.03;
class Algorithm::NaiveBayes::Model::Discrete is Algorithm::NaiveBayes;
use v6;

use Algorithm::NaiveBayes::Util <sum variance>;

# XXX! Should pi be in the core, like it is in Haskell?
sub Pi() { 4*atan2(1, 1) }

has %:model;

method :do_add_instance(%attrs, @labels) {
  for @labels -> $label {
    my %mylabel = %.training_data<labels>{$label};
    %mylabel<count>++;

    for %attrs.kv -> $attr, $value {
      push %mylabel<attrs>{$attr}, $value;
    }
  }
}

method :do_train() {
  my $m := %:model;
  
  my $instances = $.instances;
  my %labels    = %.training_data<labels>;
  
  for %labels.kv -> $label, $data {
    $m<prior_probs>{$label} = log(%labels{$label}<count> / $instances);
    
    # Calculate the mean & stddev for each label-attribute combination
    for $data<attrs>.kv -> $attr, $values {
      my $mean = sum($values) / @$values;
      my $var  = variance $values, $mean
	or next;  # Can't use variance of zero
      $m<summary>{$attr}{$label}{<mean var>} = ($mean, $var);
    }
  }

  return $m;
}

method :do_predict(%attrs) {
  my %scores = %:model<prior_probs>;

  for %attrs.kv -> $feature, $value {
    next unless exists $:model<summary>{$feature};  # Ignore totally unseen features

    for %:model<summary>{$feature}.kv -> $label, $data {
      my ($mean, $var) = $data{<mean var>};
      # This is simplified from
      #   +=  log( 1/sqrt($var*2*Pi) * exp(-($value-$mean)**2/(2*$var)) );
      %scores{$label} -= 0.5*(log($var) + log(2*Pi) + ($value-$mean)**2/$var);
    }
  }
  
  rescale %scores;

  return %scores;
}

1;
