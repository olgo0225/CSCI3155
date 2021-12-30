package edu.colorado.csci3155s2021.project2

import org.scalatest.FunSuite

class InterpreterTests extends FunSuite {

    def distance(c1:(Double,Double), c2: (Double,Double)) =
        math.sqrt( math.pow((c1._1 - c2._1), 2) + math.pow(c1._2-c2._2, 2))

    def distanceToList(c1: (Double, Double), lst: List[(Double, Double)]): Double = {
        val dList = lst.map { case c => distance(c,c1) }
        dList.foldLeft[Double](Double.PositiveInfinity) (math.min(_,_))
    }



    def comparePolygons(poly1: Polygon, poly2: Polygon): Double = (poly1, poly2) match {
        case (Polygon(lst1), Polygon(lst2)) => {
            val lst3 = lst1.map(distanceToList(_, lst2))
            lst3.foldLeft(Double.NegativeInfinity)(math.max(_,_))
        }
    }

    def compareCircles(cir1: MyCircle, cir2: MyCircle) = (cir1, cir2) match {
        case (MyCircle( (x1: Double, y1: Double), r1: Double), MyCircle((x2, y2), r2)) => {
            math.max(distance((x1, y1), (x2, y2)), math.abs(r1-r2))
        }
    }
    test("RectangleTest"){
        val s = """let x = 2+1 in rectangle(x)""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val lst = canvas.listOfObjects
                println(lst)
                val reqdPoly = new Polygon(List((0.0,0.0), (3.0,0.0), (3.0,3.0), (0.0,3.0)))
                assert(lst.exists {
                    case p@Polygon(_) => comparePolygons(p, reqdPoly) <= 1E-03
                    case _ => false
                })
            }
            case _ => assert(false)
        }
    }

    test("TriangleTest"){
        val s = """let x = (2 - 1) * 1 in triangle(x)""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val lst = canvas.listOfObjects
                println(lst)
                val reqdPoly = new Polygon(List((0.0,0.0), (1.0,0.0), (0.5,math.sqrt(3)/2.0)))
                assert(lst.exists {
                    case p@Polygon(_) => comparePolygons(p, reqdPoly) <= 1E-03
                    case _ => false
                })
            }
            case _ => assert(false)
        }
    }

    test("LineTest"){
        val s = """let x = 1 in let y = 2  in line(x * y)""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val lst = canvas.listOfObjects
                println(lst)
                val reqdPoly = new Polygon(List((0.0,0.0), (2.0,0.0)))
                assert(lst.exists {
                    case p@Polygon(_) => comparePolygons(p, reqdPoly) <= 1E-03
                    case _ => false
                })
            }
            case _ => assert(false)
        }
    }

    test("CircleTest"){
        val s = """let x = 1  in circle(x/x)""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val lst = canvas.listOfObjects
                println(lst)
                val reqdCirc = new MyCircle((1.0, 1.0), 1.0)
                assert(lst.exists {
                    case p@MyCircle(_,_) => compareCircles(p, reqdCirc) <= 1E-03
                    case _ => false
                })
            }
            case _ => assert(false)
        }
    }

    test("OverlayOperatorTest"){
        val s = """let x = 1  in circle(x/x) + rectangle(2 * x)""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val lst = canvas.listOfObjects
                println(lst)
                assert(lst.length == 2)
                val reqdCirc = new MyCircle((1.0, 1.0), 1.0)
                val reqdPoly = new Polygon(List((0.0, 0.0), (0.0, 2.0), (2.0, 2.0), (2.0, 0.0)))
                assert(lst.exists {
                    case p@MyCircle(_,_) => compareCircles(p, reqdCirc) <= 1E-03
                    case _ => false
                })
                assert(lst.exists {
                    case p@Polygon(_) => comparePolygons(p, reqdPoly) <= 1E-03
                    case _ => false
                })
            }
            case _ => assert(false)
        }
    }

    test("PlaceRightTest"){
        val s = """let x = 1  in circle(x/x) * line(2.0)""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val lst = canvas.listOfObjects
                println(lst)
                assert(lst.length == 2)
                val reqdCirc = new MyCircle((1.0, 1.0), 1.0)
                val reqdPoly = new Polygon(List((2.0, 1.0), (4.0, 1.0)))
                assert(lst.exists {
                    case p@MyCircle(_,_) => compareCircles(p, reqdCirc) <= 1E-03
                    case _ => false
                })
                assert(lst.exists {
                    case p@Polygon(_) => comparePolygons(p, reqdPoly) <= 1E-03
                    case _ => false
                })
            }
            case _ => assert(false)
        }
    }

    test("PlaceTopTest"){
        val s = """let x = 10.0  in circle(x/x) / line(2.0)""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val lst = canvas.listOfObjects
                println(lst)
                assert(lst.length == 2)
                val reqdCirc = new MyCircle((1.0, 1.0), 1.0)
                val reqdPoly = new Polygon(List((0.0, 0.0), (2.0, 0.0)))
                assert(lst.exists {
                    case p@MyCircle(_,_) => compareCircles(p, reqdCirc)<= 1E-03
                    case _ => false
                })
                assert(lst.exists {
                    case p@Polygon(_) => {
                        print(p)
                        comparePolygons(p, reqdPoly) <= 1E-03
                    }
                    case _ => false
                })
            }
            case _ => assert(false)
        }
    }


    test("RotationTest"){
        val s = """let x = 10.0  in rectangle(1) / (3.1415/4)""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val lst = canvas.listOfObjects
                println(lst)
                assert(lst.length == 1)
                val s = (math.sqrt(2.0) -1)/2.0
                val reqdPoly = new Polygon(List((0.5,-s),(1 + s,0.5), (0.5,1 + s), (-s,0.5)))
                assert(lst.exists {
                    case p@Polygon(_) => comparePolygons(p, reqdPoly) <= 1E-03
                    case _ => false
                })
            }
            case _ => assert(false)
        }
    }

    test("Program 1")  {
        val s = TestPrograms.program1()
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                assert(canvas.numPolygons == 1)
                assert(canvas.numCircles == 1)
                assert(canvas.numVerticesTotal == 4)
            }
            case _ => assert(false)
        }
    }

    test("Program 2") {
        val s = TestPrograms.program2()
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                assert(canvas.numPolygons == 9)
                assert(canvas.numCircles == 3)
                assert(canvas.numVerticesTotal == 30)
            }
            case _ => assert(false)
        }
    }

    test("Program 3") {
        val s = TestPrograms.program3()
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                assert(canvas.numPolygons == 9)
                assert(canvas.numCircles == 0)
                assert(canvas.numVerticesTotal == 27)
            }
            case _ => assert(false)
        }
    }

    test("Serpinski") {
        val s = TestPrograms.serp()
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                assert(canvas.numPolygons == 243)
                assert(canvas.numCircles == 0)
                assert(canvas.numVerticesTotal == 729)


            }
            case _ => assert(false)
        }
    }

    test("Mandala") {
        val s = TestPrograms.mandala()
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                //print(canvas.numPolygons, canvas.numCircles, canvas.numVerticesTotal)
                assert(canvas.numPolygons==40)
                assert(canvas.numCircles==0)
                assert(canvas.numVerticesTotal==140)
            }
            case _ => assert(false)
        }
    }

    test("Petals") {
        val s = TestPrograms.petals()
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                //print(canvas.numPolygons, canvas.numCircles, canvas.numVerticesTotal)
                assert(canvas.numPolygons==40)
                assert(canvas.numCircles==40)
                assert(canvas.numVerticesTotal==120)
            }
            case _ => assert(false)
        }
    }


}
