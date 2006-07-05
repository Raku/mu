  use v6-**;
  class CheckingAccount;
  
  is BankAccount;
  
  has BankAccount $.overdraft_account is rw;
  
  method withdraw ($amount) {
   
      my $overdraft_amount = $amount - $.balance;
      if ($.overdraft_account and $overdraft_amount > 0) {
          $.overdraft_account.withdraw($overdraft_amount);
          $.deposit($overdraft_amount);
      }
      call;
  };
  
=begin
  use v5;
  package CheckingAccount;
  use Moose;
  extends 'BankAccount';
  
  has overdraft_account => (isa => 'BankAccount', is => 'rw');	
  
  before withdraw => sub {
      my ($self, $amount) = @_;
      my $overdraft_amount = $amount - $self->balance;
      if ($self->overdraft_account and $overdraft_amount > 0) {
          $self->overdraft_account->withdraw($overdraft_amount);
          $self->deposit($overdraft_amount);
      }
      
  };
=end
