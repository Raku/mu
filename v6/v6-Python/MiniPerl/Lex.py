import re

tokens = (('hello', 'abc'),
	('goodbye', 'def'))

class Lex(object):
	def input(self, string):
		self.input = string
	def tokens(self):
		rv = list()
		while True:
			tok = self.token()
			if not tok: break
			rv.append(tok)
		return rv
	def token(self):
		self.input = self.input.lstrip()
		for tok in tokens:
			(meaning, regex) = tok
			if self.m(regex):
				return (meaning, self.match)
	def m(self, regex):
		self.match = re.match(regex, self.input)
		if not self.match:
			return False
		else:
			self.input = self.input.replace(self.match.group(0), '', 1)
			return self.match
