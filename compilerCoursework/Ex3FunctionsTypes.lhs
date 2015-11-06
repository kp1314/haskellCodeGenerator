> module Ex3FunctionsTypes where

> type Prog = [Function]
> data Function = Defun String String Exp
> data Exp = Const Int | Var String | Minus Exp Exp | 
>            Apply String Exp

Haskell data type for 68000 instruction set

> data Instr = Define String  -- "label:"
>            | Jsr String     -- jump to subroutine, push PC
>            | Ret            -- return from subroutine, pop PC from stack
>            | Mov Operand Operand -- "mov.l xxx yyy" (yyy:=xxx)
>            | Sub Operand Operand -- "sub.l xxx yyy" (yyy:= yyy-xxx)
> instance Show Instr where 
>   show (Define s)   = s ++ ": "
>   show (Jsr s)      = "\tjsr  " ++ s
>   show (Ret)        = "\tret"
>   show (Mov o Push) = "\tpush " ++ show o
>   show (Mov Pop o)  = "\tpop " ++ show o
>   show (Mov o1 o2)  = "\tmov " ++ show o1 ++ ", " ++ show o2
>   show (Sub o1 o2)  = "\tsub " ++ show o1 ++ ", " ++ show o2

> data Operand = Reg Register -- specifies data or address register
>              | Push         -- "-(a7)" as in "mov.w d0,-(a7)" to push d0            
>              | Pop          -- "(a7)+" (so "Mov Pop (Reg D0) = mov.w (a7)+,d0)
>              | ImmNum Int   -- "#n"
> -- deriving show
> instance Show Operand where 
>   show (Reg r) = show r 
>   show (ImmNum i) = "$" ++ show i 

> data Register = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | A7
>      deriving (Eq, Show)

> paramReg        = D0
> funcDestReg     = D1
> initialFreeRegs = [D0,D1,D2,D3,D4,D5,D6,D7] 
