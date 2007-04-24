import ply.lex as lex

tokens = (
	'SQSTR',
	'FNNAME',
	'LBRACE',
	'RBRACE',
)

def t_SQSTR(t):
	r"'(.*?[^\\])'"

# import re
# 
# class Lex(object):
# 	def set_input(self, string):
# 		self.set_input = string
# 		self.toklist = list()
# 	def tokadd(self, meaning, token):
# 		self.toklist.append((meaning, token))
# 	def tokens(self):
# 		rv = list()
# 		while True:
# 			tok = self.token()
# 			if not tok: break
# 			rv.append(tok)
# 		return rv
# 	def token(self):
# 		for tok in tokens:
# 			(meaning, regex) = tok
# 			if self.m(regex):
# 				return (meaning, self.match)
# 	def m(self, regex):
# 		self.match = re.match(regex, self.input)
# 		if not self.match:
# 			return False
# 		else:
# 			self.input = self.input.replace(self.match.group(0), '', 1)
# 			return self.match
