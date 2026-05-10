open Types
open UnionFind

type term = 
  | Expr of expr
  | Heap of expr
  | Bottom
;;