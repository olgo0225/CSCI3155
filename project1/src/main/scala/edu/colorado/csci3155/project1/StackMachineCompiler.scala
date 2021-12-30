package edu.colorado.csci3155.project1

object StackMachineCompiler {



    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
            e match{
                case Const(f) =>  PushIns(f) :: Nil
                case Ident(s) => List(StoreIns(s))
                case Plus(e1,e2) => {
                    val l1 = compileToStackMachineCode(e1)
                    val l2 = compileToStackMachineCode(e2)
                    l1 ++ l2 ++ List(AddIns)
                }
                case Minus(e1, e2) => {
                    val l1 = compileToStackMachineCode(e1)
                    val l2 = compileToStackMachineCode(e2)
                    l1 ++ l2 ++ List(SubIns)
                }
                case Mult(e1, e2) => {
                    val l1 = compileToStackMachineCode(e1)
                    val l2 = compileToStackMachineCode(e2)
                    l1 ++ l2 ++ List(MultIns)
                }
                case Div(e1, e2) => {
                    val l1 = compileToStackMachineCode(e1)
                    val l2 = compileToStackMachineCode(e2)
                    l1 ++ l2 ++ List(DivIns)
                }
                case Exp(e1) => {
                    val l1 = compileToStackMachineCode(e1)
                    l1 ++ List(ExpIns)
                }
                case Log(e1) => {
                    val l1 = compileToStackMachineCode(e1)
                    l1 ++ List(LogIns)
                }
                case Sine(e1) => {
                    val l1 = compileToStackMachineCode(e1)
                    l1 ++ List(SinIns)
                }
                case Cosine(e1) => {
                    val l1 = compileToStackMachineCode(e1)
                    l1 ++ List(CosIns)
                }
                case Let(x, e1, e2) =>{
                    val l1 = compileToStackMachineCode(e1)
                    val l2 = compileToStackMachineCode(e2)
                    l1 ++ (LoadIns(x)::l2)
                }
            }
        }
    }


