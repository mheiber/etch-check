module Node = Node

val check : Node.expr -> Node.expr list

val parse : Lexing.lexbuf -> Node.expr
