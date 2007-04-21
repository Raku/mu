from re import *

class Backend(object):
	def __init__(self, code, args):
		self.code = code
		self.args = args
	def run(self):
		tokens = Tokenizer(self.code).tokens()
		print tokens
		#ast    = Parser(tokens).parse()
		#Environment(ast).run()
class Tokenizer(object):
	def __init__(self, code):
		self.code = code
		c = compile
		self.tokarray = (('SEMICOLON', c(r'^(;)')),
				 ('SINGLE_QUOTED_STRING', c(r"^'(.*?[^\\])'")),
				 ('BAREWORD', c(r'^([a-zA-Z_]\w+)')),
				 ('COMMENT', c(r'^#(.*?)$')))
	def token(self):
		self.code = self.code.lstrip()
		for tok in self.tokarray:
			title, pat = tok[0], tok[1]
			match = pat.match(self.code)
			if match:
				print "Got", match.group(1)
				self.code = pat.sub('', self.code, 1)
				return (title, match.group(0))
		return None
	def tokens(self):
		rv = []
		while True:
			x = self.token()
			if not x: break
			rv.append(x)
class Parser(object):
	def __init__(self, tokens):
		self.tokens = tokens
	def parse(self):
		return self.tokens
class Environment(object):
	def __init__(self, ast):
		self.ast = ast
	def run(self):
		print self.ast
