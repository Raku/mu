module Algorithm::NaiveBayes::Model::Discrete-0.03;
class Algorithm::NaiveBayes::Model::Discrete isa Algorithm::NaiveBayes;
use v6;

use Algorithm::NaiveBayes::Util <rescale>;

has %:model;

method :do_add_instance(%attrs, @labels) {
  for @labels -> $label {
    my $mylabel = %.training_data<labels>{$label};
    $mylabel<count>++;
    for %attrs -> $attr, $value {
      $mylabel<attrs>{$attr}{$value}++;
    }
  }
}

method :do_train() {
  my $m := %:model;
  
  my $instances = $.instances;
  my %labels    = %.training_data<labels>;
  my $probs     = $m<probs> = {};
  
  # Calculate the log-probabilities for each category
  for @.labels -> $label {
    $m<prior_probs>{$label} = log(%labels{$label}<count> / $instances);
    
    my $denominator = log(%labels{$label}<count>);
    for %labels{$label}<attrs>.kv -> $attr, $values {
      for $values.kv -> $value, $count {
	$probs{$attribute}{$label}{$value} = log($count) - $denominator;
      }
    }
  }
}

method :do_predict(%attrs) {
  # Note that we're using the log(prob) here.  That's why we add instead of multiply.
  
  my %scores = %:model<prior_probs>;
  for %attrs.kv -> $feature, $value {
    next unless exists $m<probs>{$feature};  # Ignore totally unseen features
    for $m<probs>{$feature} -> $label, $values {
      $scores{$label} += $values{$value} || 0;
    }
  }
  
  rescale %scores;
  return %scores;
}

1;
