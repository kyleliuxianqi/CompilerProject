%option noyywrap

%{

#include <stdio.h>
#include "y.tab.h"

%} 

blank [ \t]
line \n
comment "{"[^"}"]*"}"

alphabet 	[a-zA-Z]
numeral 	[0-9]
variable 	{alphabet}|{numeral}|"_"
symbol      [_!@#$%^&*()+-=\\{}|;:,./<>?]?
identifier 	{alphabet}{variable}*

integer_val {numeral}{numeral}*
real_val 	{numeral}{numeral}*.{numeral}*
boolean_val "true"|"false"
char_val 	'{alphabet}'|'{numeral}'|'{symbol}'
string_val 	\"({variable}?{blank}?{symbol})*\"

math_fun_op "abs"|"exp"|"log"|"sqrt"|"pow"|"round"|"floor"|"ceil"

%% 

{comment}		{return(COMMENT);}

":"				{return(COLON);}
","				{return(COMMA);}
":="			{return(ASSIGN_OP);}

"("				{return(OPEN_PAREN);}
")"				{return(CLOSE_PAREN);}

"+"				{return(PLUS_OP);}
"-"				{return(MINUS_OP);}
"*"				{return(MUL_OP);}
"/"				{return(DIV_OP);}
"%"				{return(MOD_OP);}
{math_fun_op}	{return(MATH_FUN_OP);}

"=="			{return(EQ_OP);}
"<"				{return(LT_OP);}
">"				{return(GT_OP);}
"<="			{return(LE_OP);}
">="			{return(GE_OP);}
"<>"			{return(NE_OP);}
"and"			{return(AND_OP);}
"or"			{return(OR_OP);}
"not"			{return(NOT_OP);}

"code begins"   {return(CODE_BEGIN);}
"code ends"     {return(CODE_END);}
"if"			{return(IF);}
"else"			{return(ELSE);}
"while"			{return(WHILE);}
"read"			{return(READ);}
"print"			{return(PRINT);}
"Function"  	{return(FUNCTION);}
"Function ends" {return(FUNCTION_END);}
"void"			{return(VOID);}
"return"		{return(RETURN);}

"integer"   	{return(INTEGER);}
"real"      	{return(REAL);}
"boolean"   	{return(BOOLEAN);}
"char"      	{return(CHAR);}
"string"    	{return(STRING);}

{integer_val}   {return(INTEGER_VAL);}
{real_val}      {return(REAL_VAL);}
{boolean_val}   {return(BOOLEAN_VAL);}
{char_val}		{return(CHAR_VAL);}
{string_val}    {return(STRING_VAL);}

{identifier}	{return(IDENTIFIER);}

%%

