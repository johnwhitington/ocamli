/* File parser.mly */
%token <int> INT
%token LPAREN
%token RPAREN
%token EOL
%start main             /* the entry point */
%type <token> main
%%
main:
    expr EOL                { EOL }
;
expr:
    INT                     { INT $1 }
  | LPAREN                  { LPAREN }
  | RPAREN                  { RPAREN }
;
