#!/usr/bin/perl
use strict;
use warnings;
use Transform;
use Rule;
use SimpleNode;
use Data::Dumper;

sub table {
    return SimpleNode->new('table',{},@_);
}
sub row {
    return SimpleNode->new('row',{},@_);
}
sub cell {
    return SimpleNode->new('cell',{},@_);
}

my $matrix=table([
    row([
        cell([1]),
        cell([2]),
        cell([3]),
    ]),
    row([
        cell([4]),
        cell([5]),
        cell([6]),
    ]),
    row([
        cell([7]),
        cell([8]),
        cell([9]),
    ]),
]);

my $printer=Transform->new([
    Rule->new('print',sub {$_->name() eq 'table'},
              sub {return join '',$main::_T->apply('print',$_->children)}),
    Rule->new('print',sub {$_->name() eq 'row'},
              sub {return '['.(join ', ',$main::_T->apply('print',$_->children))."]\n"}),
    Rule->new('print',sub {$_->name() eq 'cell'},
              sub {return join '',$_->children}),
]);

print $printer->apply('print',$matrix);


my $transposer=Transform->new([
    Rule->new('trans',sub {$_->name() eq 'table'},
              sub {
                  warn "table!\n";
                  return table([$main::_T->apply('trans',$_->children)]) }),
    Rule->new('trans',sub {$_->name() eq 'row'},
              sub {
                  warn "row! $main::_POS\n";
                  my $pos=$main::_POS;
                  return row([ $main::_T->apply
                                   ('trans',
                                    $main::_T->find($main::_C,
                                                    sub {
                                                        return grep {$_->position == $pos }
                                                            $_->children;
                                                    }),
                                ) ])
              }),
    Rule->new('trans',sub {$_->name() eq 'cell'},
              sub { return cell([$_->children]) }),
]);

my ($xirtam)=$transposer->apply('trans',$matrix);
print $printer->apply('print',$xirtam);
