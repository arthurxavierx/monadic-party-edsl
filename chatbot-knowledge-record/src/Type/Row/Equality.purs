module Type.Row.Equality where

class RowEquals (lhs :: # Type) (rhs :: # Type) | lhs -> rhs, rhs -> lhs

instance rowEqualsRefl :: RowEquals x x
