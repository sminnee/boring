open OUnit2
open Boring.Parser
open Boring.Ast

let tests =
  "Expression tests"
  >::: [
         ( "BEDMAS, optional spaces, all ops" >:: fun _ ->
           assert_equal
             (MParser.Success
                (BinOp
                   ( Add,
                     BinOp
                       ( Add,
                         Literal (Int 5),
                         BinOp
                           ( Mul,
                             BinOp (Exp, Literal (Int 3), Literal (Int 2)),
                             BinOp (Add, Literal (Int 4), Literal (Int 1)) ) ),
                     BinOp (Div, Literal (Int 8), Literal (Int 2)) )))
             (parseExpr "5 + 3^2 * (4+1) + 8/2") );
         ( "Function definitions" >:: fun _ ->
           assert_equal
             (MParser.Success
                (FnDef
                   ( [ ("a", NativeType I32); ("b", NativeType I32) ],
                     NativeType I32,
                     [ Expr (BinOp (Mul, Var "a", Var "b")) ] )))
             (parseExpr "(a: i32, b: i32): i32 => a * b") );
         ( "If statments" >:: fun _ ->
           assert_equal
             (MParser.Success
                (IfElse
                   ( BinOp (LT, Var "a", Literal (Int 10)),
                     Literal (Int 15),
                     Literal (Int 20) )))
             (parseExpr "if a < 10 { 15 } else { 20 }") );
         ( "Comparison and boolean operators" >:: fun _ ->
           assert_equal
             (MParser.Success
                (Boring.Ast.BinOp
                   ( Boring.Ast.And,
                     Boring.Ast.BinOp
                       ( Boring.Ast.And,
                         Boring.Ast.BinOp
                           ( Boring.Ast.Or,
                             Boring.Ast.BinOp
                               ( Boring.Ast.Or,
                                 Boring.Ast.BinOp
                                   ( Boring.Ast.And,
                                     Boring.Ast.BinOp
                                       ( Boring.Ast.LT,
                                         Boring.Ast.Var "a",
                                         Boring.Ast.Var "b" ),
                                     Boring.Ast.BinOp
                                       ( Boring.Ast.GT,
                                         Boring.Ast.Var "c",
                                         Boring.Ast.Var "d" ) ),
                                 Boring.Ast.BinOp
                                   ( Boring.Ast.LTE,
                                     Boring.Ast.Var "e",
                                     Boring.Ast.Var "f" ) ),
                             Boring.Ast.BinOp
                               ( Boring.Ast.GTE,
                                 Boring.Ast.Var "g",
                                 Boring.Ast.Var "h" ) ),
                         Boring.Ast.BinOp
                           ( Boring.Ast.Eq,
                             Boring.Ast.Var "i",
                             Boring.Ast.Var "j" ) ),
                     Boring.Ast.BinOp
                       (Boring.Ast.NotEq, Boring.Ast.Var "k", Boring.Ast.Var "l")
                   )))
             (parseExpr "a<b & c>d | e<=f | g>=h & i=j & k!=l ") );
         ( "Record literals" >:: fun _ ->
           assert_equal
             (MParser.Success
                (Boring.Ast.NewRecord
                   [
                     ("x", Boring.Ast.Literal (Boring.Ast.Int 1));
                     ("y", Boring.Ast.Literal (Boring.Ast.Int 2));
                     ("z", Boring.Ast.Literal (Boring.Ast.Int 3));
                   ]))
             (parseExpr "{x: 1, y: 2, z: 3,}") );
       ]

let () = run_test_tt_main tests
