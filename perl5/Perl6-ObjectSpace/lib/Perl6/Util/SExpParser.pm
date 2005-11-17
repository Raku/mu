
package Perl6::Util::SExpParser;

use strict;
use warnings;

sub parse {
    my ($class, $source) = @_;
    my ($stack, $subexp) = ([], []);
    
    my @tokens = ($source =~ /\(|\)|\'|\"|[a-zA-Z0-9_\:\=\!\$\%\?\@\&\*\<\>\{\}\;]+/g);
    
#    warn join "\n" => @tokens;
    
    foreach my $token (@tokens) {
        if ($token eq '(') {
            my $new_arr = [];
            if (@{$stack}) {
                push @{$stack->[-1]} => $new_arr;
            }
            else {
                push @{$subexp} => $new_arr;
            }
            push @{$stack} => $new_arr;
        }
        elsif ($token eq ')') {
            pop @{$stack};
        }
        else {
            if (@{$stack}) {
                push @{$stack->[-1]} => $token;                            
            }
        }
    }
    return $subexp->[0]
}

1;

__END__

# this code was shamelessly swiped from:
# - http://mail.python.org/pipermail/python-list/2005-March/270531.html
# and converted to Ruby, ...
# and then converted to perl (see above)
# all so that we can parse a scheme-like syntax 
# with Attribute Grammers (stolen from Haskell (I think))

class SExpParser     
    def parse (source)
        stack = Array.new
        subexp = Array.new
        source.scan(/\(|\)|\'|[a-zA-Z0-9_:=!$%?@&]+/).each { |token|
            if token == '('
                new_arr = Array.new
                if !stack.empty?
                    stack[-1].push(new_arr)
                else
                    subexp.push(new_arr)
                end
                stack.push(new_arr)
            elsif token == ')'
                stack.pop
            else 
                stack[-1].push(token)
            end
        }
        return subexp[0]
    end
end

