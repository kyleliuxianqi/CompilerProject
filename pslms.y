%{

#include <stdio.h>

extern int yylex(void);
extern int yyparse(void);
void yyerror(char *s){
	printf("\n invalid : %s\n",s);
}

%}

%token COMMENT
%token IDENTIFIER
%token INTEGER 		REAL 		BOOLEAN 	CHAR 		STRING
%token INTEGER_VAL	REAL_VAL	BOOLEAN_VAL	CHAR_VAL	STRING_VAL
%token COLON COMMA OPEN_PAREN CLOSE_PAREN
%token ASSIGN_OP EQ_OP LT_OP GT_OP LE_OP GE_OP NE_OP AND_OP OR_OP NOT_OP
%token PLUS_OP MINUS_OP MUL_OP DIV_OP MOD_OP MATH_FUN_OP
%token CODE_BEGIN CODE_END
%token VAR 
%token IF THEN ELSE
%token WHILE DO 
%token READ PRINT
%token FUNCTION FUNCTION_END
%token VOID RETURN

%right COLON
%right ASSIGN_OP
%left  PLUS_OP MINUS_OP MUL_OP DIV_OP MOD_OP 
%left  EQ_OP LT_OP GT_OP LE_OP GE_OP NE_OP AND_OP OR_OP NOT_OP

%%

program: 
    CODE_BEGIN code CODE_END {return 0;} 
    ;

code:
    code stmts
    | code funcs
    | stmts
    | funcs
    ;

stmts:
    stmts stmt
    | stmt
    ;

stmt:
    stmt_declare 
    | stmt_if
    | stmt_while
    | stmt_io
    | RETURN vals
    ;

stmt_declare: 
    val_type assignment
    | assignment
    ;

stmt_if:
    IF OPEN_PAREN expression_boolean CLOSE_PAREN stmts
    | IF OPEN_PAREN expression_boolean CLOSE_PAREN ELSE stmts
    ;

stmt_while:
    WHILE OPEN_PAREN expression_boolean CLOSE_PAREN stmts
    ;

stmt_io:
    PRINT OPEN_PAREN expression_boolean CLOSE_PAREN 
    | READ OPEN_PAREN IDENTIFIER CLOSE_PAREN 
    ;

val_type: STRING | CHAR | INTEGER | REAL | BOOLEAN;

assignment: 
    IDENTIFIER ASSIGN_OP val_cal
    | val_cal
    ;

val_cal:
    val_cal op_cal val_cal
    | OPEN_PAREN val_cal CLOSE_PAREN
    | val
    | IDENTIFIER
    | func_call
    | func_math
    ;

op_cal: PLUS_OP | MINUS_OP | MUL_OP | DIV_OP | MOD_OP ;

vals: 
    vals COMMA val 
    | val 
    | IDENTIFIER;

val: INTEGER_VAL | REAL_VAL | BOOLEAN_VAL | CHAR_VAL | STRING_VAL ;

expression_boolean:
    expression_boolean boolean_type expression_boolean
    | OPEN_PAREN expression_boolean CLOSE_PAREN 
    | IDENTIFIER
    | val
    ;

boolean_type: EQ_OP | NE_OP | GE_OP | LE_OP | GT_OP | LT_OP | AND_OP | OR_OP | NOT_OP ;

func_call:
    IDENTIFIER OPEN_PAREN vals CLOSE_PAREN
    ;

func_math: MATH_FUN_OP OPEN_PAREN vals CLOSE_PAREN ;

funcs: 
    FUNCTION val_type IDENTIFIER OPEN_PAREN args CLOSE_PAREN stmts FUNCTION_END
    | FUNCTION val_type IDENTIFIER OPEN_PAREN CLOSE_PAREN stmts FUNCTION_END
    ;

args: 
    args COMMA arg
    | arg
    ;

arg: 
    val_type IDENTIFIER
    ;

%%

int main() {
	printf("\nStart entering :\n");
	yyparse();
	return 0;
}
