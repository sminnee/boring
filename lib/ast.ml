(* Boring lang AST *)

(* Different binary operators - arithemtic, comparison, and boolean *)
type binOp =
  | Mul
  | Div
  | Add
  | Sub
  | Exp
  | Eq
  | NotEq
  | LT
  | LTE
  | GT
  | GTE
  | And
  | Or

(* Different unary operators - arithemtic and boolean *)
type unOp = Negate | Not

(* A type native to Boring *)
type nativeType = I32 | F32 | String

(* A type definition *)
type borType = NativeType of nativeType | NamedType of string

type typeDef = Record of (string * borType) list

type arg = string * borType

type literal = Int of int | Float of float | String of string

(* Single expression - no lets or types *)
type expr =
  | BinOp of binOp * expr * expr
  | UnOp of unOp * expr
  | Literal of literal
  | Var of string
  | Property of string * expr
  | NewRecord of (string * expr) list
  | FnCall of string * expr list
  | IfElse of expr * expr * expr
  | FnDef of arg list * borType * body

and bodyItem =
  | Expr of expr
  | LetDef of string * expr
  | TypeDef of string * typeDef

(* A block of expressions and preamble *)
and body = bodyItem list

let makeLetDef name value = LetDef (name, value)

let makeTypeDef name record = TypeDef (name, record)

let makeFnDef args ret body = FnDef (args, ret, body)

let makeIfElse cond ifExpr elseExpr = IfElse (cond, ifExpr, elseExpr)

let exprToBodyItem expr = Expr expr

let exprToBody expr = [ Expr expr ]

(* Makes a var ref or a function call based on whether args are passed *)
let makeFnCall name optArgs =
  match optArgs with Some args -> FnCall (name, args) | None -> Var name

(* References a record property, or not, based on whether an property name was passed *)
let makeRecordProp expr optName =
  match optName with Some name -> Property (name, expr) | None -> expr

let makeRecord values = NewRecord values