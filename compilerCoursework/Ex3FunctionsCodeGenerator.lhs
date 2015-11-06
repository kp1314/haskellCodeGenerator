> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes
> import Data.List

---------------------------------------------------
Code Generator for Compilers Exercise 3 

Kiran Patel (kp1314)
---------------------------------------------------

Part (1): translate the function declaration

> translateFunction (Defun funName paramName funBody)
>   = [Define funName] ++ (transExp funBody (funcDestReg:initialFreeRegs)) ++ 
>     [Ret] 

Part (2): saving the registers before function is called.

> saveRegisters regsNotInUse
>   = map (\x -> (Mov (Reg x) Push)) registersInUse 
>     where registersInUse = allRegs\\regsNotInUse    
 
Part (3): translate the expresions

> transExp :: Exp -> [Register] -> [Instr]
> transExp (Const i) (dest:rest) 
>   = [(Mov (ImmNum i)(Reg dest))]
> transExp (Var s) (rest) 
>   = [(Mov (Reg paramReg) (Reg funcDestReg))]
> transExp (Minus e1 e2) (dest:r:rest)
>   |(weight e1) > (weight e2) = (transExp e1 (dest:r:rest)) ++ 
>     (transExp e2 (r:rest)) ++ [(Sub (Reg r) (Reg dest))]
>   |otherwise                 = (transExp e2 (r:dest:rest)) ++ 
>     (transExp e1 (dest:rest)) ++ [(Sub (Reg r) (Reg dest))]
> transExp (Apply s e) (dest:rest) 
>   = (saveRegisters (dest:rest)) ++ (transExp e (rest)) ++ 
>       [Mov (Reg funcDestReg) (Reg paramReg)] ++ [Jsr s] ++ 
>         [Mov (Reg funcDestReg) (Reg dest)] ++ (restoreRegisters (dest:rest)) 

> weight :: Exp -> Int
> weight (Const n) = 1
> weight (Var s) = 1
> weight (Minus e1 e2) = minimum[cost1, cost2]
>   where 
>     cost1 = maximum[weight e1, (weight e2) + 1]
>     cost2 = maximum[(weight e1)+1,(weight e2)]
> weight (Apply s e1) = (weight e1) + 1  

After expresions have been translated

> restoreRegisters regsNotInUse 
>   = map (\x -> (Mov Pop (Reg x))) registersInUse
>     where registersInUse = allRegs\\regsNotInUse    
 
