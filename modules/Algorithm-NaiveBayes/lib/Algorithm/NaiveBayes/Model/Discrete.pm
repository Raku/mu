package Algorithm::NaiveBayes::Model::Discrete;

use strict;
use base qw(Algorithm::NaiveBayes);
use Algorithm::NaiveBayes::Util qw(rescale);

sub do_add_instance {
  my ($self, $attributes, $labels, $data) = @_;
  
  foreach my $label ( @$labels ) {
    my $mylabel = $data->{labels}{$label} ||= {};
    $mylabel->{count}++;
    while (my ($attr, $value) = each %$attributes) {
      $mylabel->{attrs}{$attr}{$value}++;
    }
  }
}

sub do_train {
  my ($self, $training_data) = @_;
  my $m = {};
  
  my $instances = $self->instances;
  my $labels = $training_data->{labels};
  my $probs = $m->{probs} = {};
  
  # Calculate the log-probabilities for each category
  foreach my $label ($self->labels) {
    $m->{prior_probs}{$label} = log($labels->{$label}{count} / $instances);
    
    my $denominator = log($labels->{$label}{count});
    while (my ($attribute, $values) = each %{ $labels->{$label}{attrs} }) {
      while (my ($value, $count) = each %$values) {
	$probs->{$attribute}{$label}{$value} = log($count) - $denominator;
      }
    }
  }
  
  return $m;
}

sub do_predict {
  my ($self, $m, $newattrs) = @_;
  
  # Note that we're using the log(prob) here.  That's why we add instead of multiply.
  
  my %scores = %{$m->{prior_probs}};
  while (my ($feature, $value) = each %$newattrs) {
    next unless exists $m->{probs}{$feature};  # Ignore totally unseen features
    while (my ($label, $values) = each %{$m->{probs}{$feature}}) {
      $scores{$label} += ($values->{$value} || 0);
    }
  }
  
  rescale \%scores;
  return \%scores;
}

1;
