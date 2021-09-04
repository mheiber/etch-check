%{
    open Node
%}
%token FUN
%token LET IN
%token <int> INT
%token IF THEN ELSE
%token PLUS
%token EQ
%token <string> VAR
%token TRUE
%token FALSE
%token COLON
%token LPAREN
%token RPAREN
%token ARROW
%token COLEQ
%token BOOL_TY
%token INT_TY
%token EOF

%right ARROW

%start <Node.expr> prog
%%

prog: | expr EOF { $1 }

expr:
    | app { $1 }
    | non_app { $1 }
    
app:
    | app non_app { App ($1, $2) }
    | non_app non_app { App ($1, $2) }

non_app:
    | FUN VAR COLON ty COLEQ non_app
      { Fun (Some $2, $4, $6) }
    | LET binding IN non_app {
      let v, e = $2 in
      Let (v, e, $4)
    }
    | IF non_app THEN non_app ELSE non_app { If ($2, $4, $6) }
    | arith { $1 }
    | boolean { $1 }
    | cont { $1 }

cont:
    | short { $1}
    | LPAREN expr RPAREN { $2 }

short:
    | VAR { Var $1 }
    | INT { Int $1 }
    | TRUE { Bool true }
    | FALSE { Bool false }

binding: | VAR COLEQ expr { ($1, $3) }

arith:
    | cont PLUS cont { Plus ($1, $3) }
    
boolean:
    | cont EQ cont { Eq ($1, $3) }

ty:
  | ty ARROW ty { FunTy ($1, $3) }
  | INT_TY { IntTy }
  | BOOL_TY { BoolTy }
  
