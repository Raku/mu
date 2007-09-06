  use v6-alpha;
  class BankAccount;
  
  has Int $.balance is rw = 0;
  
  method deposit ($amount) {
      $.balance += $amount;
  }
  
  method withdraw ($amount) {
      my $current_balance = $.balance;
      ($current_balance >= $amount)
          orelse fail "Account overdrawn";
      $.balance = $current_balance - $amount;
  }
  
=begin
  use v5;
  package BankAccount;
  use Moose;
  
  has balance => (isa => 'Int', is => 'rw', default => 0);
  
  sub deposit {
      my ($self, $amount) = @_;
      $self->balance($self->balance + $amount);
  }
  
  sub withdraw {
      my ($self, $amount) = @_;
      my $current_balance = $self->balance;
      ($current_balance >= $amount)
          or die "Account overdrawn";
      $self->balance($current_balance - $amount);
  }
=end
  
