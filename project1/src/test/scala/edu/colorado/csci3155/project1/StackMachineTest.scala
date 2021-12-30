package edu.colorado.csci3155.project1

import org.scalatest.FunSuite

class StackMachineTest extends FunSuite {

    def testValueOfIdent(env: Environment.t, x: String, f: Double) = {
        assert( math.abs(Environment.lookup(x, env) - f) <= 1E-05)
    }

    test(testName="Load Test"){
        StackMachineEmulator.emulateSingleInstruction(List(20.9), Environment.empty, LoadIns("x"))==20.9
    }


    test("stack machine test 1") {
        val lst1 = List(PushIns(2.5), PushIns(3.5), AddIns, LoadIns("result"))
        val f = StackMachineEmulator.emulateStackMachine(lst1)
        testValueOfIdent(f, "result", 6.0)
        //assert(Environment.lookup("result", f) == 6.0)
    }

    test("stack machine test 2") {
        val lst1 = List(PushIns(2.5), PushIns(3.5), AddIns, ExpIns, LogIns, LoadIns("result"))
        val f = StackMachineEmulator.emulateStackMachine(lst1)
        testValueOfIdent(f, "result", 6.0)
        //assert(math.abs( Environment.lookup("result", f) - 6.0) <= 1e-05)
    }

    test("stack machine test 3") {
        val lst1 = List(PushIns(3.5), PushIns(2.5), PushIns(4.5), PushIns(5.2), AddIns, LoadIns("x"), LoadIns("y"), LoadIns("z"), StoreIns("y"), LoadIns("w"))
        val fenv = StackMachineEmulator.emulateStackMachine(lst1)
        testValueOfIdent(fenv,"x" , 9.7)
        //assert( math.abs(fenv("x") - 9.7 ) <= 1e-05 )
        testValueOfIdent(fenv, "y", 2.5)
        //assert( math.abs(fenv("y") - 2.5 ) <= 1e-05 )
        testValueOfIdent(fenv, "z", 3.5)
        //assert( math.abs(fenv("z") - 3.5 ) <= 1e-05 )
    }

    test("stack machine test 4") {
        val lst4 = List(PushIns(3.5), PushIns(2.5), PushIns(4.5), PushIns(5.2),  LoadIns("x"), LoadIns("y"), LoadIns("z"), LoadIns("w"),
                         StoreIns("y"), StoreIns("w"), AddIns, LoadIns("res1"), StoreIns("x"), StoreIns("z"), MultIns, LoadIns("res2"))
        val fenv = StackMachineEmulator.emulateStackMachine(lst4)
        testValueOfIdent(fenv, "x", 5.2)
        testValueOfIdent(fenv, "y", 4.5)
        testValueOfIdent(fenv, "z", 2.5)
        testValueOfIdent(fenv, "w", 3.5)
        testValueOfIdent(fenv, "res1", 8.0)
        testValueOfIdent(fenv, "res2", 5.2*2.5)
    }

    test("stack machine test 5") {
        val lst1 = List(PushIns(1.5), PushIns(2.4), AddIns, PushIns(2.5), PushIns(2.5), MultIns, SubIns)
        val lst2 = List(PushIns(1.0), PushIns(2.5), AddIns)
        val lst3 = lst1 ++ List(ExpIns) ++ lst2 ++ List(ExpIns) ++ List(AddIns) ++ List(LogIns) ++ List(LoadIns("result"))
        val f = StackMachineEmulator.emulateStackMachine(lst3)
        testValueOfIdent(f, "result", 3.50287)
        //assert(math.abs(f("result") - 3.50287) <= 1E-04)
    }


}
