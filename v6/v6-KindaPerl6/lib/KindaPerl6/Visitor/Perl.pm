package KindaPerl6::Visitor::Perl;
use strict;

sub new {
    bless {}, $_[0];
}
sub visit {
    my ( $visitor, $self, $node_name, $data ) = @_;
    my $result = '';
    $result .= "::$node_name(\n";
    #print Dumper( $data );
    for my $item ( keys %$data ) {
        $result .= "  $item => ";
        #$result .= Dumper( $data->{$item} );       
        #$result .= "isa:" . ref( $data->{$item} );
        if ( 'ARRAY' eq ref $data->{$item} ) {
            $result .= "[ " 
                . join( ", ", map { $_->emit( $visitor ) } @{$data->{$item}} ) 
                . " ],\n";
        } 
        elsif ( 'HASH' eq ref $data->{$item} ) {
            $result .= "{ " 
                . join( ", ", map { $_ . ' => ' . ($data->{$item}{$_})->emit( $visitor ) } keys %{$data->{$item}} ) 
                . " },\n";
        } 
        elsif ( '' ne ref $data->{$item} ) {
            #$result .= "isa:" . ref( $data->{$item} );
            $result .= ($data->{$item})->emit( $visitor ); 
        } 
        else {
            $result .= "'" . $data->{$item} . "',\n";
        } 
    }
    $result .= ")\n";
}

1;
