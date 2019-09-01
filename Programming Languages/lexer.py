import ply.lex as lex

tokens = (
    'LANGLE',
    'LANGLESLAH',
    'RANGLE',
    'EQUAL',
    'STRING',
    'WORD',
)

t_ignore = ' '

def t_LANGLESLAH(token):
    r'</'
    return token

def t_LANGLE(token):
    r'<'
    return token

def t_RANGLE(token):
    r'>'
    return token

def t_EQUAL(token):
    r'='
    return token

def t_STRING(token):
    r'"[^"]*"'
    token.value = token.value[1:-1]
    return token

def t_WORD(token):
    r'[^ <>\n]+'
    return token

htmllexer = lex.lex()
htmllexer.input("This is <b>my</b> webpage!")

while True:
    tok = htmllexer.token()
    if not tok: break
    print tok
