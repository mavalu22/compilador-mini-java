import ply.yacc as yacc
import ply.lex as lex

class MyLexer(object):
    
    # Lista de tokens
    tokens = [
        'ABSTRACT', 'BOOLEAN', 'BREAK', 'BYTE', 'CASE', 'CATCH', 'CHAR', 'CLASS',
        'CONST', 'CONTINUE', 'DEFAULT', 'DO', 'DOUBLE', 'ELSE', 'EXTENDS', 'FINAL',
        'FINALLY', 'FLOAT', 'FOR', 'GOTO', 'IF', 'IMPLEMENTS', 'IMPORT', 'INSTANCEOF',
        'INT', 'INTERFACE', 'LONG', 'NATIVE', 'NEW', 'PACKAGE', 'PRIVATE', 'PROTECTED',
        'PUBLIC', 'RETURN', 'SHORT', 'STATIC', 'SUPER', 'SWITCH', 'SYNCHRONIZED',
        'THIS', 'THROW', 'THROWS', 'TRANSIENT', 'TRY', 'VOID', 'VOLATILE', 'WHILE',
        'IDENTIFIER', 'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'LPAREN', 'RPAREN',
        'RBRACE', 'LBRACE', 'SEMICOLON', 'ASPAS', 'TIMESEQUAL','DIVEQUAL','MODEQUAL','PLUSEQUAL',
        'MINUSEQUAL','LSHIFTEQUAL','RSHIFTEQUAL','URSHIFTEQUAL','ANDEQUAL','XOREQUAL','OREQUAL','MOD',
        'OR','AND','EQUALS','NOTEQUAL','LESS','LESSEQUAL','GREATER','GREATEREQUAL', 'COMMA', 'COLON', 'EQUAL',
        'DOT', 'REAL', 'INTEGER'
    ]

    reserved = {token.lower(): token for token in tokens}

    def t_IDENTIFIER(self, t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = self.reserved.get(t.value,'IDENTIFIER')
        return t

    # def t_IDENTIFIER1(self, t):
    #     r'[a-zA-Z_][a-zA-Z_0-9]*|\d+|\d+\.\d+'
    #     t.type = self.reserved.get(t.value,'IDENTIFIER1')
    #     return t

    def t_REAL(self, t):
        r'\d+\.\d+'
        t.value = float(t.value)
        return t

    def t_INTEGER(self, t):
        r'\d+'
        t.value = int(t.value)
        return t

    t_PLUS    = r'\+'
    t_MINUS   = r'-'
    t_TIMES   = r'\*'
    t_MOD   = r'\%'
    t_DIVIDE  = r'/'
    t_LPAREN  = r'\('
    t_RPAREN  = r'\)'
    t_LBRACE  = r'\{'
    t_RBRACE  = r'\}'
    t_SEMICOLON  = r'\;'
    t_ASPAS  = r'\''
    t_TIMESEQUAL = r'\*='
    t_DIVEQUAL = r'/='
    t_MODEQUAL = r'%='
    t_PLUSEQUAL = r'\+='
    t_MINUSEQUAL = r'-='
    t_LSHIFTEQUAL = r'<<='
    t_RSHIFTEQUAL = r'>>='
    t_URSHIFTEQUAL = r'>>>='
    t_ANDEQUAL = r'&='
    t_XOREQUAL = r'\^='
    t_OREQUAL = r'\|='
    t_COLON = r':'
    t_COMMA = r'\,'
    t_OR = r'\|\|'  
    t_AND = r'&&'
    t_EQUAL = r'='
    t_EQUALS = r'=='
    t_NOTEQUAL = r'!='
    t_LESS = r'<'
    t_LESSEQUAL = r'<='
    t_GREATER = r'>'
    t_GREATEREQUAL = r'>='
    t_DOT = r'\.'




    # Ignorar espaços e tabs
    t_ignore = ' \t'

    # Expressão regular para comentários de várias linhas (/* */)
    def t_COMMENT_MULTI(self, t):
        r'/\*(.|\n)*?\*/'
        t.lexer.lineno += t.value.count('\n')

    # Expressão regular para comentários de uma única linha (//)
    def t_COMMENT_SINGLE(self, t):
        r'\/\/.*'
        pass

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    # Regra para tratar erros
    def t_error(self, t):
        print(f"Illegal character '{t.value[0]}'")
        t.lexer.skip(1)

    def input(self, data):
        self.lexer.input(data)

    def __init__(self):
        self.lexer = lex.lex(module=self)
        self.lexer.lineno = 1
    
    def reset_lineno(self):
        self.lexer.lineno = 1

    def token(self):
        return self.lexer.token()

# Construir o lexer
lexer = MyLexer()
tokens = lexer.tokens

# Teste do lexer
def test_lexer(input_string):
    lexer.input(input_string)
    result = []
    while True:
        tok = lexer.token()
        if not tok: 
            break
        result.append((tok.type, tok.lineno))
    lexer.reset_lineno()
    return result

# -------------------------------------------------------------- PARSER ----------------------------------------------------------

def p_program(p):
    '''
    program : packageDeclaration importDeclaration classDeclaration
            | importDeclaration packageDeclaration classDeclaration
            | packageDeclaration importDeclaration
            | importDeclaration packageDeclaration
            | packageDeclaration classDeclaration
            | importDeclaration classDeclaration
            | packageDeclaration
            | importDeclaration
            | classDeclaration
            |
    '''

def p_packageDeclaration(p):
    '''
    packageDeclaration : PACKAGE IDENTIFIER SEMICOLON
    '''

def p_importDeclaration(p):
    '''
    importDeclaration : IMPORT IDENTIFIER SEMICOLON
    '''

def p_classDeclaration(p):
    '''
    classDeclaration : classModifier CLASS IDENTIFIER classBody
    '''
def p_classModifier(p):
    '''
    classModifier : PUBLIC
                  | ABSTRACT
                  | FINAL
                  | empty
    '''

def p_classBody(p):
    '''
    classBody : LBRACE classBodyDeclaration_opt RBRACE
    '''

def p_classBodyDeclaration_opt(p):
    '''
    classBodyDeclaration_opt : classBodyDeclaration
                            | empty
    '''

def p_classBodyDeclaration(p):
    '''
    classBodyDeclaration : Modifiers Type IDENTIFIER fieldMethodDeclaration classBodyDeclaration_opt
    '''

def p_Modifiers(p):
    '''
    Modifiers : PUBLIC
              | PROTECTED
              | PRIVATE
              | FINAL
              | STATIC
              | empty
    '''

def p_fieldMethodDeclaration(p):
    '''
    fieldMethodDeclaration : variableDeclarators SEMICOLON
                          | methodDeclaration
    '''

def p_variableDeclarators(p):
    '''
    variableDeclarators : EQUAL expression
                       | COMMA IDENTIFIER variableDeclarators
                       | empty
    '''

def p_methodDeclaration(p):
    '''
    methodDeclaration : methodDeclarator block
    '''

def p_methodDeclarator(p):
    '''
    methodDeclarator : LPAREN formalParameterList RPAREN
    '''

def p_formalParameter(p):
    '''formalParameter : Type IDENTIFIER'''


def p_formalParameterList(p):
    '''formalParameterList : formalParameterList COMMA formalParameter
                           | formalParameter
                           | empty'''


def p_Type(p):
    '''
    Type : BYTE
         | SHORT
         | INT
         | LONG
         | CHAR
         | FLOAT
         | DOUBLE
         | BOOLEAN
         | VOID
    '''

def p_block(p):
    '''
    block : LBRACE blockStatement_opt RBRACE
    '''

def p_blockStatement_opt(p):
    '''
    blockStatement_opt : blockStatement
                     | empty
    '''

def p_blockStatement(p):
    '''
    blockStatement : localVariableDeclaration SEMICOLON blockStatement_opt
                  | statement blockStatement_opt
    '''

def p_localVariableDeclaration(p):
    '''
    localVariableDeclaration : Type IDENTIFIER variableDeclarators
    '''

def p_statement(p):
    '''
    statement : block
             | emptyStatement
             | IDENTIFIER statementExpression SEMICOLON
             | doStatement
             | breakStatement
             | continueStatement
             | returnStatement
             | ifStatement
             | whileStatement
             | tryStatement
             | classInstanceCreationExpression
    '''

def p_emptyStatement(p):
    '''
    emptyStatement : SEMICOLON
    '''

def p_labeledStatement(p):
    '''
    labeledStatement : COLON statement
    '''

def p_statementExpression(p):
    '''
    statementExpression : assignment
                      | methodInvocation
                      | labeledStatement
    '''

def p_ifStatement(p):
    '''
    ifStatement : IF LPAREN expression RPAREN statement ifThenElseStatement_opt
    '''

def p_ifThenElseStatement_opt(p):
    '''
    ifThenElseStatement_opt : ELSE statement
                          | empty
    '''

def p_whileStatement(p):
    '''
    whileStatement : WHILE LPAREN expression RPAREN statement
    '''

def p_doStatement(p):
    '''
    doStatement : DO statement WHILE LPAREN expression RPAREN SEMICOLON
    '''

def p_breakStatement(p):
    '''
    breakStatement : BREAK identifier_opt SEMICOLON
    '''

def p_continueStatement(p):
    '''
    continueStatement : CONTINUE identifier_opt SEMICOLON
    '''

def p_identifier_opt(p):
    '''
    identifier_opt : IDENTIFIER
                 | empty
    '''

def p_returnStatement(p):
    '''
    returnStatement : RETURN expression_opt SEMICOLON
    '''

def p_expression_opt(p):
    '''
    expression_opt : expression
                  | empty
    '''

def p_tryStatement(p):
    '''
    tryStatement : TRY block catchesStatement
    '''

def p_catchesStatement(p):
    '''
    catchesStatement : catchClause finally_opt
                   | finally
    '''

def p_IDENTIFIER1(p):
    '''
    IDENTIFIER1 : IDENTIFIER
                | REAL
                | INTEGER
    '''

def p_catchClause(p):
    '''
    catchClause : CATCH LPAREN formalParameter RPAREN block catchClause
               | empty
    '''

def p_finally_opt(p):
    '''
    finally_opt : finally
               | empty
    '''
def p_finally(p):
    '''
    finally : FINALLY block
    '''

def p_assignment(p):
    '''
    assignment : assignmentOperator expression
    '''

def p_assignmentOperator(p):
    '''
    assignmentOperator : EQUAL
                      | TIMESEQUAL
                      | DIVEQUAL
                      | MODEQUAL
                      | PLUSEQUAL
                      | MINUSEQUAL
                      | LSHIFTEQUAL
                      | RSHIFTEQUAL
                      | URSHIFTEQUAL
                      | ANDEQUAL
                      | XOREQUAL
    '''

def p_expression(p):
    '''
    expression : comparationExpression expression2
    '''

def p_expression2(p):
    '''
    expression2 : OR comparationExpression expression2
               | AND comparationExpression expression2
               | empty
    '''

def p_comparationExpression(p):
    '''
    comparationExpression : operationalExpression relationalExpression
    '''

def p_relationalExpression(p):
    '''
    relationalExpression : EQUALS operationalExpression relationalExpression
                       | NOTEQUAL operationalExpression relationalExpression
                       | LESS operationalExpression relationalExpression
                       | LESSEQUAL operationalExpression relationalExpression
                       | GREATER operationalExpression relationalExpression
                       | GREATEREQUAL operationalExpression relationalExpression
                       | empty
    '''

# def p_relationalExpressionOpt(p):
#     '''
#     relationalExpressionOpt : relationalExpression
#                             | empty
#     '''


def p_operationalExpression(p):
    '''
    operationalExpression : term additiveExpression
    '''

def p_additiveExpression(p):
    '''
    additiveExpression : PLUS term additiveExpression
                      | MINUS term additiveExpression
                      | empty
    '''


# def p_additiveExpressionOpt(p):
#     '''
#     additiveExpressionOpt : additiveExpression
#                       | empty
#     '''



def p_term(p):
    '''
    term : unaryExpression multiplicativeExpression
    '''

def p_multiplicativeExpression(p):
    '''
    multiplicativeExpression : TIMES unaryExpression multiplicativeExpression
                           | DIVIDE unaryExpression multiplicativeExpression
                           | MOD unaryExpression multiplicativeExpression
                           | empty
    '''

# def p_multiplicativeExpressionOpt(p):
#     '''
#     multiplicativeExpressionOpt : multiplicativeExpression
#                       | empty
#     '''

def p_unaryExpression(p):
    '''
    unaryExpression : PLUS IDENTIFIER1 methodInvocation_opt
                   | MINUS IDENTIFIER1 methodInvocation_opt
                   | IDENTIFIER1 methodInvocation_opt
    '''

def p_methodInvocation_opt(p):
    '''
    methodInvocation_opt : methodInvocation
                        | empty
    '''

def p_methodInvocation(p):
    '''
    methodInvocation : LPAREN argumentList_opt RPAREN
                   | SUPER DOT IDENTIFIER LPAREN argumentList_opt RPAREN
    '''

def p_classInstanceCreationExpression(p):
    '''
    classInstanceCreationExpression : NEW IDENTIFIER LPAREN argumentList_opt RPAREN SEMICOLON
    '''

def p_argumentList_opt(p):
    '''
    argumentList_opt : argumentList
                    | empty
    '''

def p_argumentList(p):
    '''
    argumentList : expression
                | argumentList_opt COMMA expression
    '''

def p_empty(p):
    '''
    empty :
    '''

def p_error(p):
    global parse_error_message
    if p:
        parse_error_message = f"Syntax error at token '{p.type}' on line {p.lineno}"
    else:
        parse_error_message = "Syntax error at EOF"

parser = yacc.yacc()

def test_parser(input_string):
    global parse_error_message
    parse_error_message = None
    result = parser.parse(input_string, lexer=lexer)
    if parse_error_message:
        print(parse_error_message)
        lexer.reset_lineno()
        return parse_error_message
    else:
        print("Parsing successful.")
        lexer.reset_lineno()
        return result
# --------------------------------------------------------------- END PARSER ----------------------------------------------------------