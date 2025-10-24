%{
open Syntax
%}


%token STAR
%token OR
%token LPAREN
%token RPAREN
%token <string> LOWER
%token <string> UPPER
%token EOF

%type <Syntax.regexp> parse
%type <Syntax.regexp> regexp
%type <Syntax.regexp> char

%start parse

// %nonassoc LOWER UPPER LPAREN
%left OR
%left SEQ
%left STAR

%%
parse:
    | regexp EOF { $1 }

regexp:
    | regexp OR regexp { Or ($1, $3) }
    | regexp regexp { Seq ($1, $2) } %prec SEQ 
    | regexp STAR { Star $1 }
    | LPAREN regexp RPAREN { $2 }
    | char { $1 }

char:
    | LOWER { Lower $1 }
    | UPPER { Upper $1 }