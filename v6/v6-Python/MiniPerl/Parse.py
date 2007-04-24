import ply.yacc as yacc
from MiniPerl.Lex import tokens
from MiniPerl.ParseNodes import *

start = 'class'

def p_class(p):
	"class : CLASS IDENT LBRACE statements RBRACE"
	p[0] = ClassNode(p[2], p[4])
def p_statements(p):
	'''statements : statement statements
			| statement'''
	if len(p) == 3:
		p[0] = StatementListNode(p[1], p[2])
	else:
		p[0] = p[1]
def p_statement(p):
	"statement : IDENT SEMICOLON"
	p[0] = StatementNode(p[1])
def p_error(p):
    print "Syntax error in input!"
yacc.yacc()
