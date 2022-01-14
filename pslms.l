%{
	#include <stdio.h>
%} 

blank [ \t]
line \n

alphabet [a-zA-Z]
numeral [0-9]
variable {alphabet}|{numeral}|"_"
identifier {alphabet}{variable}*

integer {numeral}{numeral}*
real {numeral}{numeral}*.{numeral}*
boolean "true"|"false"
char '{alphabet}'|'{numeral}'
string \"({variable}?{blank}?[_!@#$%^&*()+-=\\{}|;:,./<>?]?)*\"
value {integer}|{real}|{boolean}|{char}|{string}

data_type "integer"|"real"|"boolean"|"char"|"string"

assignment_op ":="
arithmetic_op "*"|"+"|"-"|"/"|"div"|"mod"
boolean_op "=="|"<"|">"|">="|"<="|"<>"|"and"|"or"|"not"
mathematical_function_op "abs"|"exp"|"log"|"sqrt"|"pow"|"round"|"floor"|"ceil"
operator {arithmetic_op}|{boolean_op}|{mathematical_function_op}

var "var"
declaration {var}{blank}{blank}*{identifier}(,{identifier})*:{data_type}

expression {identifier}{operator}({identifier}|{value})({operator}({identifier}|{value}))*

assignment {identifier}(,{identifier})*{assignment_op}({identifier}|{expression}|{value})

condition_stmt "if"|"then"|"else"

loop_stmt "while"|"do"

io_stmt "read"|"print"

function_stmt "Function"|"retrun"|"void"|"null"

comment "{"[^"}"]*"}"

%% 

{integer} {printf("INTEGER\n");}
{real} {printf("REAL\n");}
{boolean} {printf("BOOLEAN\n");}
{char} {printf("CHAR\n");}
{string} {printf("STRING\n");}

{assignment_op} {printf("ASSIGNMENT OPERATOR\n");}
{arithmetic_op} {printf("ARITHMETIC OPERATOR\n");}
{boolean_op} {printf("BOOLEAN OPERATOR\n");}
{mathematical_function_op} {printf("MATHMATICAL FUNCTION OPERATOR\n");}

{declaration} {printf("DECLARATION\n");}

{expression} {printf("EXPRESSION\n");}

{assignment} {printf("ASSIGNMENT\n");}

{condition_stmt} {printf("IF ELSE STATEMENT\n");}

{loop_stmt} {printf("DO WHILE STATEMENT\n");}

{io_stmt} {printf("IO STATEMENT\n");}

{function_stmt} {printf("FUNCTION STATEMENT\n");}

{comment} {printf("COMMENT\n");}

%%

int main()
{
	printf("start entering: \n");
	yylex();
	return 1;
}

