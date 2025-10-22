%{
open Syntax
%}


%token STAR
%token <string> LOWER
%token <string> UPPER
%token EOF

%type <Syntax.regexp> parse
%type <Syntax.regexp> regexp
%type <Syntax.regexp> char

%start parse

%left SEQ
%left STAR

%%
parse:
    | regexp EOF { $1 }

regexp:
    | regexp STAR { Star $1 }
    | regexp regexp { Seq ($1, $2) } %prec SEQ 
    | char { $1 }

char:
    | LOWER { Lower $1 }
    | UPPER { Upper $1 }