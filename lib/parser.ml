(* Parser of Boring files and expressions; produces Ast *)

open MParser
open Ast

(*
 * BASE TOKENS
 * All of which consume subsequent spaces
 *)

(* A single character optionally followed by spaces *)
let charToken c = char c << spaces

(* A multi-character token optionally followed by spaces *)
let strToken str = string str << spaces

(* A multi-character token necessarily followed by spaces *)
let strTokenSp str = string str << spaces1

(* An identifiers used for types, variables, and functions *)
let identifier = many1_chars alphanum << spaces

(* A positive integer token - returns a string *)
let intToken = many1_chars digit << spaces

(*
 * IDIOMATIC STRUCTURES
 *)

(* Parse a block contained within parentheses *)
let parens p = charToken '(' >>? p << charToken ')'

(* Infix operator stub for expressions *)
let infix parseOp op =
  Infix ((parseOp |>> fun _ a b -> BinOp (op, a, b)), Assoc_left)

(* Operators and precedence in expressions *)
let operators =
  [
    [ infix (charToken '^') Exp ];
    [ infix (charToken '*') Mul; infix (charToken '/') Div ];
    [ infix (charToken '+') Add; infix (charToken '-') Sub ];
    [
      infix (charToken '=') Eq;
      infix (strToken "!=") NotEq;
      infix (charToken '<') LT;
      infix (charToken '>') GT;
      infix (strToken "<=") LTE;
      infix (strToken ">=") GTE;
    ];
    [ infix (charToken '&') And; infix (charToken '|') Or ];
  ]

(* clause 'label identifier =', where label is e.g. let or type  *)
let labelledAssign label = strTokenSp label >> identifier << charToken '='

let int = intToken |>> int_of_string |>> fun i -> Literal (Int i)

(* A type identifier - native type or a type name *)
let borType =
  identifier |>> fun x ->
  match x with
  | "i32" -> NativeType I32
  | "f32" -> NativeType F32
  | "string" -> NativeType String
  | x -> NamedType x

(* An identifier and a type, eg 'size: i32' *)
let typedIdentifier = pair (identifier << charToken ':') borType

(* Type definition of a record *)
let recordTypeDef =
  charToken '{' >> sep_end_by1 typedIdentifier (charToken ',') << charToken '}'
  |>> fun list -> Record list

(* Parse fn with an optional ".key" record-item suffix *)
let withProperty exprParser =
  pipe2 exprParser (option (charToken '.' >> identifier)) makeRecordProp

(* Parse a single expression: value terms -> value result *)
let rec expr s = (expression operators term) s

(* Parse a term of an expression *)
and term s =
  (int <|> attempt fnDef
  <|> withProperty (ifElse <|> newRecord <|> fnCall <|> parens expr)
  <|> int)
    s

and fnCall s =
  (pipe2 identifier
     (option
        (charToken '(' >> sep_end_by expr (charToken ',') << charToken ')'))
     makeFnCall)
    s

(* Parse a function def *)
and fnDef s =
  (pipe3
     (charToken '('
     >> sep_end_by typedIdentifier (charToken ',')
     << charToken ')')
     (charToken ':' >> borType << strToken "=>")
     exprOrBlock makeFnDef)
    s

(* Parse an if-else expression *)
and ifElse s =
  (pipe3
     (strTokenSp "if" >> expr << charToken '{')
     expr
     (charToken '}' >> strToken "else" >> charToken '{' >> expr << charToken '}')
     makeIfElse)
    s

(* (expr) or { block } *)
and exprOrBlock s =
  (attempt (charToken '{' >> body << charToken '}') <|> (expr |>> exprToBody)) s

(* Parse a let x = (expr) or let x = { block } expression *)
and letDef s = (pipe2 (labelledAssign "let") expr makeLetDef) s

and typeDef s = (pipe2 (labelledAssign "type") recordTypeDef makeTypeDef) s

and body s = many1 (letDef <|> typeDef <|> (expr |>> exprToBodyItem)) s

(* A property: value *)
and propVal s = (pair (identifier << charToken ':') expr) s

and newRecord s =
  (charToken '{'
  >> sep_end_by1 propVal (charToken ',')
  << charToken '}' |>> makeRecord) s

(* Parse the contents of a file from a string *)
let parse s : body result = parse_string (spaces >> body << eof) s ()

(* Parse the contents of an expression from a string *)
let parseExpr s : expr result = parse_string (spaces >> expr << eof) s ()
