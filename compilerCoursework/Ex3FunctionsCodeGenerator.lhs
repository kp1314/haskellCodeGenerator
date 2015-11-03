> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes

---------------------------------------------------
Code Generator for Compilers Exercise 3 

Kiran Patel (kp1314)
---------------------------------------------------

Part (1): translate the function declaration

> translateFunction (Defun funName paramName funBody)
>   = [Define funName] ++ saveRegisters freeRegs++ 
>       (transExp funBody freeRegs)++restoreRegisters 

Part (2): saving the registers before function is called.

> saveRegisters regsNotInUse
>   =  
>  
 
Part (3): translate the expresions

> transExp :: Exp -> [Register] -> [Instr]
> transExp (Const i) (dest:rest) = [(Mov (ImmNum i)(Reg dest)]
> transExp (Var s) (dest:rest) = 
> transExp (Minus e1 e2) (dest:rest)
    |weight (e1 > e2) = Sub transExp  
> transExp (Apply s e) (dest:rest) = 

> weight :: Exp -> Int 
> 

After expresions have been translated
> restoreRegisters  

