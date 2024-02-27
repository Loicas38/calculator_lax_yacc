import ply.lex as lex
import ply.yacc as yacc

# the things which will be use, wth the operations and variable
tokens = [
    "INT", 
    "FLOAT", 
    "NAME", 
    "PLUS", 
    "MINUS",
    "DIVIDE",
    "MULTIPLY",
    "EQUAL"
]

# representation of the things used for calculs
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EQUAL = r'\='

t_ignore = r' '


# definition of what is each thing
def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t


def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'NAME'
    return t

def t_error(t):
    print("illegal character")
    t.lexer.skip(1)

lexer = lex.lex()

# priority of the operations
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE')
)


def p_calc(p):
    """
    calc : expression
         | var_assign
         | empty
    """
    print(run(p[1]))

# variable assignment
def p_var_assign(p):
    """
    var_assign : NAME EQUAL expression
    
    """
    p[0] = ("=", p[1], p[3])

# creation of tuple with the sign first and then the values like ('+', 1, 2) for 1+2
def p_expression(p):
    """
    expression : expression DIVIDE expression
               | expression MULTIPLY expression
               | expression MINUS expression
               | expression PLUS expression
    """
    p[0] = (p[2], p[1], p[3])


def p_expression_int_float(p):
    """
    expression : INT
               | FLOAT
    """
    p[0] = p[1]

# for variable assignment
def p_expression_var(p):
    """
    expression : NAME
    """
    p[0] = ('var', p[1])

# for the errors
def p_error(p):
    print("Syntax error find !")

def p_empty(p):
    """
    empty :
    """
    p[0] = None


parser = yacc.yacc()

# storage of the vraiables
env = {}
def run(p):
    global env
    message = "Unassign variable !"
    # make the operations
    if type(p) == tuple:
        if p[0] == '+':
            nb_1 = run(p[1])
            nb_2 = run(p[2])
            # avoid error when a variable isn't assign
            if nb_1 != message and nb_2 != message:
                return nb_1 + nb_2
            else:
                return message
            
        elif p[0] == '*':
            nb_1 = run(p[1])
            nb_2 = run(p[2])
            if nb_1 != message and nb_2 != message:
                return nb_1 * nb_2
            else:
                return message
            
        elif p[0] == '-':
            nb_1 = run(p[1])
            nb_2 = run(p[2])
            if nb_1 != message and nb_2 != message:
                return nb_1 - nb_2
            else:
                return message
        
        elif p[0] == '/':
            nb_1 = run(p[1])
            nb_2 = run(p[2])
            if nb_1 != message and nb_2 != message:
                return nb_1 / nb_2
            else:
                return message
            
        # variable assignment
        elif p[0] == "=":
            env[p[1]] = run(p[2])
            return f"{p[1]} = {env[p[1]]}"
        
        elif p[0] == "var":
            if p[1] not in env:
                return message
            
            else:
                return env[p[1]]
    else:
        return p



while True:
    try:
        s = input('calcul : ')
    except EOFError:
        break
    parser.parse(s)