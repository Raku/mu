module Algorithm::NaiveBayes::Model::Frequency-0.03;
class Algorithm::NaiveBayes::Model::Frequency isa Algorithm::NaiveBayes;
use v6;

use Algorithm::NaiveBayes::Util <sum_hash add_hash max rescale>;

has %.attributes;
has %.labels;
has %:model;

method :do_add_instance(%attrs, @labels) {
  add_hash %.training_data<attributes>, %attrs;
  
  my $mylabels = %.training_data<labels>;
  for @labels -> $label {
    $mylabels{$label}<count>++;
    add_hash $mylabels{$label}<attributes>, %attrs;
  }
}

method :do_train() {
  my $m := %:model;

  my $instances  = $.instances;
  my $labels     = %.training_data<labels>;
  $m{attributes} = %.training_data{attributes};
  my $vocab_size = keys $m.<attributes>;
  
  # Calculate the log-probabilities for each category
  for @.labels -> $label {
    $m<prior_probs>{$label} = log($labels{$label}<count> / $instances);
    
    # Count the number of tokens in this cat
    my $label_tokens = sum_hash $labels{$label}<attributes>;
    
    # Compute a smoothing term so P(word|cat)==0 can be avoided
    $m<smoother>{$label} = -log($label_tokens + $vocab_size);
    
    # P(attr|label) = $count/$label_tokens                         (simple)
    # P(attr|label) = ($count + 1)/($label_tokens + $vocab_size)   (with smoothing)
    # log P(attr|label) = log($count + 1) - log($label_tokens + $vocab_size)
    
    my $denominator = log($label_tokens + $vocab_size);
    
    for $labels{$label}<attributes>.kv -> $attr, $count {
      $m<probs>{$label}{$attr} = log($count + 1) - $denominator;
    }
  }

  return $m;
}

method :do_predict(%attrs) {
  # Note that we're using the log(prob) here.  That's why we add instead of multiply.
  
  my %scores = %:model<prior_probs>;
  for %attrs -> $feature, $value {
    next unless exists $:model<attributes>{$feature};  # Ignore totally unseen features
    for %:model<probs> -> $label, $attributes {
      # P($feature|$label)**$value
      $scores{$label} += ($attributes{$feature} || %:model<smoother>{$label})*$value;
    }
  }
  
  rescale %scores;

  return %scores;
}

1;
