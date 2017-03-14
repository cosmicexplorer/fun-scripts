%{
#include <string>
extern "C" int yylex(void);
%}

%token T_IDENTIFIER T_NUMBER
%token T_LPAREN T_RPAREN

%left T_PLUS T_MINUS
%left T_MUL T_DIV
%left UNARY

%start program

%%

program : expr
;

expr : T_NUMBER
     | expr T_PLUS expr
     | expr T_MINUS expr
     | expr T_MUL expr
     | expr T_DIV expr
     | T_MINUS expr   %prec UNARY
     | T_LPAREN expr T_RPAREN
;
