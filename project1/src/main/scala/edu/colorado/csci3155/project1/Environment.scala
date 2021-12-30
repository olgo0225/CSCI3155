package edu.colorado.csci3155.project1


object Environment {
    /* An environment type is a list of tuples each tuple being a (String, Double) */
    type t = List[(String, Double)]

    /* lookup simply finds the very first binding for a given string and returns it,
        or throws an exception if none found */
    def lookup(x: String, env: t): Double = {
        val t = env.find(_._1 == x) // Find first element that matches the given x
        if (t.isEmpty){
            throw new IllegalArgumentException(s"Unknown Identifier - $x")
        } else {
            t.get._2 // return second component of t  - we know t is of form Some(v)
        }
    }
    /* extend -- takes the environment and adds a binding from string x to value v */
    def extend(x: String, v: Double, env: t): t = {
        (x,v)::env
    }
    /* the empty environment */
    def empty: t = {
        Nil
    }
}


