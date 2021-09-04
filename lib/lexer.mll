{
    open Lexing
    open Parser

    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            {
                pos with pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + 1
            }
}

let inte = '-'? ['0'-'9'] ['0'-'9']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule read =
    parse
    | white { read lexbuf }
    | "let" { LET }
    | "=" { EQ }
    | "+" { PLUS }
    | ":=" { COLEQ  }
    | "in" { IN }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | ":" { COLON }
    | "fun" { FUN }
    | "true" { TRUE }
    | "false" { FALSE }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | "->" { ARROW }
    | newline { next_line lexbuf; read lexbuf}
    | inte { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "Bool" { BOOL_TY }
    | "Int" { INT_TY }
    | id { VAR (Lexing.lexeme lexbuf)}
    | _ as c { 
      let msg = Printf.sprintf "unexpected char '%c'" c in
        raise (SyntaxError msg)
    }
    | eof { EOF }
