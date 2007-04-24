class ClassNode:
	def __init__(self, name, statements):
		self.name = name
		self.statements = statements
	def __str__(self):
		rv = 'class %s\n' % self.name
		rv += str(self.statements)
		rv += 'end'
		return rv
class StatementNode:
	def __init__(self, ident):
		self.ident = ident
	def __str__(self):
		return self.ident + '\n'
class StatementListNode:
	def __init__(self, first, second):
		self.first = first
		self.second = second
	def __str__(self):
		return str(self.first) + str(self.second)
