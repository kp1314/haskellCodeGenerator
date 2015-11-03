> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes

---------------------------------------------------
Code Generator for Compilers Exercise 3 

Kiran Patel (kp1314)
---------------------------------------------------

Part (1): translate the function declaration

> translateFunction (Defun funName paramName funBody)
>   = [Define funName] ++ (saveRegisters initialFreeRegs) ++ 
>       (transExp funBody initialfreeRegs) ++ restoreRegisters 

Part (2): saving the registers before function is called.

> saveRegisters regsNotInUse
>   = map (\x -> (Mov (Reg x) Push)) registersInUse
>     where registersInUse = initialFreeRegs\\regsNotInUse    
 
Part (3): translate the expresions

> transExp :: Exp -> [Register] -> [Instr]
> transExp (Const i) (dest:rest) 
>   = [(Mov (ImmNum i)(Reg dest)]
> transExp (Var s) (dest:rest) 
>   =  
> transExp (Minus e1 e2) (dest:r:rest)
>   |(weight e1) > (weight e2) = (transExp e1 (dest:rest)) ++ 
>     (transExp e2 (r:rest)) ++ (Sub (Reg r) (Reg dest))
>   |otherwise                 = (transExp e2 (r:rest)) ++ 
>     (transExp e1 (dest:rest) ++ (Sub (Reg r) (Reg dest))
> transExp (Apply s e) (dest:rest) 
>  = 

> weight :: Exp -> Int
> weight (Const Int) = 1
> weight (Var s) = 1
> weight (Minus e1 e2) = weight e2 + weight e1 + 1
> weight (Apply s e1) = weight e1 + 1  


After expresions have been translated
> restoreRegisters regsNotInUse 
>   = map (\x -> (Mov Pop (Reg x))) registersInUse
>     where registersInUse = initialFreeRegs\\regsNotInUse    
> 
