instance Show Expr where
    show (V x) = show x
    show (VO op e1 e2) =  "{" ++ show op ++ ", " ++ show e1 ++ ", " ++ show e2 ++ "}"
    show (SO op e1 e2) =  "{" ++ show op ++ ", " ++ show e1 ++ ", " ++ show e2 ++ "}"

instance Show IntExpr where
    show (I x) = show x
    show (NO n e) = "{" ++ show n ++ ", " ++ show e ++ "}"

instance Show VectorOp where
    show Add = "'add'"
    show Sub = "'sub'"
    show Dot = "'dot'"

instance Show ScalarOp where
    show Mul = "'mul'"
    show Div = "'div'"

instance Show NormOp where
    show NormOne = "'norm_one'"
    show NormInf = "'norm_inf'"