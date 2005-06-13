#=prefix
++$i		# $i.prefix:<++>()
-123		# 123.prefix:<->()
="file"		# "file".prefix:<=>()

# der "reduce metaoperator" ist ein prefix Listen-Operator
[+] 1, 2, 3	# (1,2,3).prefix:<[+]>()
#=postfix
$i++		# $i.postfix:<++>()

# Fakultät; ist kein Standard-Operator!
3!  		# 3.postfix:<!>()
