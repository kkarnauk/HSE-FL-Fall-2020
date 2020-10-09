import ply.yacc as yacc 

from lex_impl import tokens 

def apply_tabs(str):
  lines = str.splitlines(True)
  result = ""

  for line in lines:
    result += '|\t' + line.strip('\n') + '\n'
  
  return result.strip('\n')


def p_program(p):
  '''program : relation
             | program relation'''
  if len(p) == 2:
    p[0] = p[1]
  elif len(p) == 3:
    p[0] = p[1] + '\n\n' + p[2]



def p_relation(p):
  '''relation : atom DOT
              | atom TURNSTILE disjunction DOT'''
  if len(p) == 3:
    p[0] = '|relation\n|head\n' + apply_tabs(p[1])
  elif len(p) == 5:
    p[0] = '|relation\n' + apply_tabs('|head\n' + apply_tabs(p[1])) + '\n' + apply_tabs('|body\n' + apply_tabs(p[3]))



def p_atom(p):
  '''atom : IDENTIFIER
          | IDENTIFIER atomend'''
  if len(p) == 2:
    p[0] = '|identifier = \'' + p[1] + '\''
  elif len(p) == 3:
    p[0] = '|atom\n|\t|identifier = \'' + p[1] + '\'\n' + apply_tabs(p[2])



def p_atomarg(p):
  '''atomarg : atomcontinue
             | LEFTBRACKET atomarg RIGHTBRACKET'''
  if len(p) == 2:
    p[0] = p[1]
  elif len(p) == 4:
    p[0] = "|atom\n" + apply_tabs(p[2])



def p_atomend(p):
  '''atomend : atomcontinue
             | LEFTBRACKET atomarg RIGHTBRACKET
             | LEFTBRACKET atomarg RIGHTBRACKET atomend'''
  if len(p) == 2:
    p[0] = p[1]
  elif len(p) == 4:
    p[0] = "|atom\n" + apply_tabs(p[2])
  elif len(p) == 5:
    p[0] = "|atom\n" + apply_tabs(p[2]) + '\n' + p[4]



# only for correct appearance
def p_atomcontinue(p):
  '''atomcontinue : IDENTIFIER
                  | IDENTIFIER atomend'''
  if len(p) == 2:
    p[0] = '|identifier = \'' + p[1] + '\''
  elif len(p) == 3:
    p[0] = '|identifier = \'' + p[1] + '\'\n' + p[2]



def p_disjunction(p):
  '''disjunction : conjunction SEMICOLON disjunction
                 | conjunction'''
  if len(p) == 4:
    p[0] = '|disjunction\n' + apply_tabs(p[1]) + '\n' + apply_tabs(p[3])
  elif len(p) == 2:
    p[0] = p[1] 



def p_conjunction(p):
  '''conjunction : element COMMA conjunction
                 | element'''
  if len(p) == 4:
    p[0] = '|conjunction\n' + apply_tabs(p[1]) + '\n' + apply_tabs(p[3])
  elif len(p) == 2:
    p[0] = p[1]



def p_element(p):
  '''element : LEFTBRACKET disjunction RIGHTBRACKET
             | atom'''
  if len(p) == 4:
    p[0] = p[2]
  elif len(p) == 2:
    p[0] = p[1]



def p_error(p): 
  raise SyntaxError

parser = yacc.yacc()

def parse(expr):
  return parser.parse(expr)
