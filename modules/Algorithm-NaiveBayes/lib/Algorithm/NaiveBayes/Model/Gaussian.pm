package Algorithm::NaiveBayes::Model::Gaussian;

use strict;
use base qw(Algorithm::NaiveBayes);
use Algorithm::NaiveBayes::Util qw(sum variance);
use constant Pi => 4*atan2(1, 1);

sub do_add_instance {
  my ($self, $attributes, $labels, $training_data) = @_;
  
  foreach my $label ( @$labels ) {
    my $mylabel = $training_data->{labels}{$label} ||= {};
    $mylabel->{count}++;
    while (my ($attr, $value) = each %$attributes) {
      push @{$mylabel->{attrs}{$attr}}, $value;
    }
  }
}

sub do_train {
  my ($self, $training_data) = @_;
  my $m = {};
  
  my $instances = $self->instances;
  my $labels = $training_data->{labels};
  
  while (my ($label, $data) = each %$labels) {
    $m->{prior_probs}{$label} = log($labels->{$label}{count} / $instances);
    
    # Calculate the mean & stddev for each label-attribute combination
    while (my ($attr, $values) = each %{$data->{attrs}}) {
      my $mean = sum($values) / @$values;
      my $var  = variance($values, $mean)
	or next;  # Can't use variance of zero
      @{ $m->{summary}{$attr}{$label} }{'mean', 'var'} = ($mean, $var);
    }
  }
  return $m;
}

sub do_predict {
  my ($self, $m, $newattrs) = @_;
  
  my %scores = %{$m->{prior_probs}};
  while (my ($feature, $value) = each %$newattrs) {
    next unless exists $m->{summary}{$feature};  # Ignore totally unseen features
    while (my ($label, $data) = each %{$m->{summary}{$feature}}) {
      my ($mean, $var) = @{$data}{'mean', 'var'};
      # This is simplified from
      #   +=  log( 1/sqrt($var*2*Pi) * exp(-($value-$mean)**2/(2*$var)) );
      $scores{$label} -= 0.5*(log($var) + log(2*Pi) + ($value-$mean)**2/$var);
    }
  }
  
  rescale(\%scores);

  return \%scores;
}

1;
