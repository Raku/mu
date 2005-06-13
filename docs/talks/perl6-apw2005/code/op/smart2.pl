#=Array und Hash Smart-Match
$s ~~ @a        # $s Element in @a
123 ~~ @a       # ?@a[123]
123 ~~ *@a      # 123 Element in @a
@a ~~ /regex/   # irgendein Element
@a ~~ @b        # alle Elemente
$s ~~ %h        # ?%h{$s}
%h ~~ /regex/   # ein passender Hash-Key
%a ~~ %b        # gleiche Hashes
any($a,'b',/regex/) ~~ any(@a, 'c', 'd') # cool, nicht?
