import ply.yacc as yacc
from MiniPerl.Lex import tokens
from MiniPerl.ParseNodes import *

start = 'class'

def p_class(p):
	"class : CLASS IDENT LBRACE statements RBRACE"
	p[0] = ClassNode(p[2], p[4])
def p_statements(p):
	'''statements : statement SEMICOLON statements'''
	p[0] = StatementListNode(p[1], p[3])
def p_statement_single(p):
	'''statements : statement'''
	p[0] = p[1]
def p_statement(p):
	"statement : expression"
	p[0] = StatementNode(p[1])
def p_expression(p):
	"expression : SQSTR"
	p[0] = p[1]
def p_assignment(p):
	"statement : DOLLAR IDENT SET expression"
	p[0] = AssignmentNode(p[2], p[4])
def p_error(p):
    print "Syntax error in input!"
yacc.yacc()
