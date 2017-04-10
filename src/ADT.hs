{-# LANGUAGE GADTs, StandaloneDeriving #-}
module ADT where
-- Discriminated union for the 4 registers of the SVM
data Register = Reg1
              | Reg2
              | Reg3
              | Reg4
deriving instance Show (Register)
deriving instance Eq (Register)

-- We change every instance of float in the AST to Double since Float should be avoided (see https://wiki.haskell.org/Performance/Floating_point)
-- Data structures representing the constant values of the language. 
-- Address may contain the Integer representing the memory address or the register from which the address is read.
data Literal = LitInt Integer
             | LitFloat Double
             | LitString String
--             | Neg Literal
             | LitAdress Literal
             | LitRegister Register
deriving instance Show (Literal)
deriving instance Eq (Literal)

-- Instructions supported by the SVM. See the documentation for further details.            
-- We still need address and Register somewhere            
data Instruction = Nop
                | Mov Literal Literal
                | And Register Literal
                | Or Register Literal
                | Not Register
                | Mod Register Literal
                | Add Register Literal
                | Sub Register Literal
                | Mul Register Literal
                | Div Register Literal
                | Cmp Register Literal
                | Jmp String
                | Jc String Register
                | Jeq String Register
                | LabelI String
                | Program [Instruction]
deriving instance Show (Instruction)
deriving instance Eq (Instruction)