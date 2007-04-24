from MiniPerl.Lex import Lex

class Parse:
	def __init__(self):
		self.lex = Lex()
		self.tokadd('SINGLE_QUOTED_STRING', r"""'(.*?[^\\])'""")
	def set_input(self, text):
		self.lex.set_input(text)
	def parse(
