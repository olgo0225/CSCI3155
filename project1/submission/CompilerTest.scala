package edu.colorado.csci3155.project1
import org.scalatest.FunSuite


class CompilerTest extends FunSuite {
    def testValueOfIdent(env: Environment.t, x: String, f: Double) = {
        assert( math.abs(Environment.lookup(x, env) - f) <= 1E-05)
    }
    
    test ("simple expression 1") {
        val e = Const(1.0)
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        assert(lst == List(PushIns(1.0)))
    }

    test("simple expression 2") {
        val e = Plus(Const(1.0), Const(2.5))
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        assert(lst == List(PushIns(1.0), PushIns(2.5), AddIns))
    }

    test("simple expression 3"){
        val e = Minus(Plus(Const(1.5), Const(2.4)), Mult(Const(2.5), Const(2.5)))
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        assert(lst == List(PushIns(1.5), PushIns(2.4), AddIns, PushIns(2.5), PushIns(2.5), MultIns, SubIns))
    }

    test("simple expression 4") {
        val e1 = Minus(Plus(Const(1.5), Const(2.4)), Mult(Const(2.5), Const(2.5)))
        val e2 = Plus(Const(1.0), Const(2.5))
        val e3 = Log(Plus(Exp(e1), Exp(e2)))
        val lst = StackMachineCompiler.compileToStackMachineCode(e3)
        //println("----")
        //lst.foreach(println)
        //println("----")
        val lst1 = List(PushIns(1.5), PushIns(2.4), AddIns, PushIns(2.5), PushIns(2.5), MultIns, SubIns)
        val lst2 = List(PushIns(1.0), PushIns(2.5), AddIns)
        val lst3 = lst1 ++ List(ExpIns) ++ lst2 ++ List(ExpIns) ++ List(AddIns) ++ List(LogIns)
        assert(lst == lst3)
    }

    test("LetBinding + Ident Test 1") {
        val e1 = Let("x", Const(1.0), Ident("x")) //let x = 1 in x
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)
        //println("----")
        //lst.foreach(println)
        //println("----")
        assert(lst == List(PushIns(1.0), LoadIns("x"), StoreIns("x")))
    }

    test("LetBinding + Ident Test 2") {
        val e1 = Let("x", Const(1.5), Plus(Ident("x"), Ident("x"))) //let x = 1 in x
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)
        //println("----")
        //lst.foreach(println)
        //println("----")
        assert(lst == List(PushIns(1.5), LoadIns("x"), StoreIns("x"), StoreIns("x"), AddIns))
    }

    test("LetBinding + Ident Test 3") {
        val x = Ident("x")
        val y = Ident("y")
        val innerLet = Let("y", Plus(x, Const(1.0)), Mult(x, y))
        val e1 = Let("x", Const(1.5), innerLet) //let x = 1.5 in let y = 1 + x in x * y
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)
        //println("----")
        //lst.foreach(println)
        //println("----")
       // assert(lst == List(PushIns(1.0), LoadIns("x"), StoreIns("x")))
        assert(lst == List(PushIns(1.5),LoadIns("x"), StoreIns("x"),PushIns(1.0), AddIns, LoadIns("y"), StoreIns("x"),StoreIns("y"),MultIns))
    }

    test(testName = "LetBinding + Ident Test 4") {
        val x = Ident("x")
        val innerLet = Let("x", Const(1.0), Plus(x, Const(1)))
        val e1 = Let("x", innerLet, Plus(x, Const(2)))
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)
        val fenv = StackMachineEmulator.emulateStackMachine(lst ++ List(LoadIns("result")))
        //assert(math.abs(fenv("result") - 4.0) <= 1E-05)
        testValueOfIdent(fenv, "result", 4.0)
    }


}
