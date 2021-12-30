package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case class LoadIns(s: String) extends StackMachineInstruction
case class  StoreIns(s: String) extends StackMachineInstruction
case class PushIns(f: Double) extends StackMachineInstruction
case object AddIns extends StackMachineInstruction
case object SubIns extends StackMachineInstruction
case object MultIns extends StackMachineInstruction
case object DivIns extends StackMachineInstruction
case object ExpIns extends StackMachineInstruction
case object LogIns extends StackMachineInstruction
case object SinIns extends StackMachineInstruction
case object CosIns extends StackMachineInstruction
case object PopIns extends StackMachineInstruction


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */

    def emulateSingleInstruction(stack: List[Double],
                                 env: Environment.t,
                                 ins: StackMachineInstruction): (List[Double], Environment.t) = {
        ins match {
            case LoadIns(s) => {
                stack match {
                    case Nil => throw new IllegalArgumentException("Empty Stack")
                    case h :: t => {
                        val newenv = (s, h) :: env
                        (t, newenv)
                    }
                }
            }
            case StoreIns(s) => {
                val r = env.filter(x => {
                    x._1 == s
                })
                if (r == List.empty) {
                    throw new IllegalArgumentException("Value DNE")
                }
                else {
                    val newstack = Environment.lookup(s, env) :: stack
                    (newstack, env)
                }
            }
            case PushIns(d) => (d :: stack, env)
            case PopIns => (stack.slice(1, stack.length), env)
            case AddIns => {
                stack match {
                    case x :: y :: rest => {
                        val newstack = (y + x) :: rest
                        (newstack, env)
                    }
                    case _ => throw new IllegalArgumentException("Not enough elements")
                }
            }
            case SubIns => {
                stack match{
                    case x :: y :: rest => {
                        val newstack = (y - x) :: rest
                        (newstack, env)
                    }
                    case _ => throw new IllegalArgumentException("Not enough elements")
                }
            }
            case MultIns => {
                stack match{
                    case x :: y :: rest => {
                        val newstack = (y * x) :: rest
                        (newstack, env)
                    }
                    case _ => throw new IllegalArgumentException("Not enough elements")
                }
            }
            case DivIns => {
                stack match {
                    case x :: y :: rest => {
                        if (x == 0) {
                            throw new IllegalArgumentException("Division by 0")
                        }
                        val newstack = (y / x) :: rest
                        (newstack, env)
                    }
                    case _ => throw new IllegalArgumentException("Not enough elements")
                }
            }
            case LogIns => {
                stack match{
                    case x :: rest => {
                        if (x>0){
                            (scala.math.log(x) :: rest, env)
                        }
                        else{
                            throw new IllegalArgumentException("x is negative")
                        }
                    }
                    case Nil => throw new IllegalArgumentException("Stack is Empty")
                }
            }
            case ExpIns => {
                stack match{
                    case x :: rest => {
                        (scala.math.exp(x)::rest, env)
                    }
                    case Nil => throw new IllegalArgumentException("Stack is Empty")
                }
            }
            case SinIns => {
                stack match{
                    case x :: rest => {
                        (scala.math.sin(x)::rest, env)
                    }
                    case Nil => throw new IllegalArgumentException("Stack is Empty")
                }
            }
            case CosIns => {
                stack match{
                    case x :: rest => {
                        (scala.math.cos(x)::rest, env)
                    }
                    case Nil => throw new IllegalArgumentException("Stack is Empty")
                }
            }
        }
    }
    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Environment.t =
        {
        instructionList.foldLeft(Nil: List[Double], Environment.empty)((acc: (List[Double], Environment.t), instruct: StackMachineInstruction) => {
            emulateSingleInstruction(acc._1, acc._2, instruct)
        })._2
        }
}