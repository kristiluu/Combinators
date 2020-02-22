//Kristi Luu - Homework 3 - Combinators

//Problem 1
object hw {
    def main(args: Array[String]): Unit = {
    }
    
    //COMPLETED problem 1
    def compose[T](f: T=>T, g: T=>T): T=>T = {
        def r(x: T): T = f(g(x));
        r _
    }

    def inc(x: Double) = x + 1
    def double(x: Double) = 2 * x
    def isZero(n: Int) = n == 0
    def inc(n: Int) = n + 1
    def dec(n: Int) = n - 1
/*
    //COMPLETED problem 2
    def selfIter[T] (f: T=>T, n: Int): T => T = if isZero(n) n else selfIter(compose(f _, f _), dec(n))

    //println(selfIter(inc _, double _))

    //COMPLETED problem 3 REQUIRES TESTING
    def countPass[T](arr: T, n: Int): Int = { 
        arr match {
            case T => Boolean: inc(n)
            case _ => n
        }
        n
    }
    
    //problem 4a
    def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
        def r(x: Int): Int = if (isZero(baseVal)) 0 else recur(baseVal, combiner);
        r _ 
    } **/
    //problem 4b
    def recur_sum(n: Int, m: Int): Int = if (isZero(m)) n else recur_sum(inc(n), dec(m)) //n + m = (n+1) + (m-1) 
    def recur_mul(n: Int, m: Int): Int = if (isZero(m)) 0 else recur_sum(n, recur_mul(n, dec(m)))
    def recur_factorial(n: Int): Int = if (n == 1) 1 else recur_mul(n, recur_factorial(dec(n)))


    //problem 5
    //def deOptionize 

    //COMPLETED Problem 6 
    def square(x: Double) = x * x                        

    //PART A
    /*
    def combineIter[T] (f: T=>T): T=>T {
        def t(init: T, n: Int): T=>T = {
            var result = init
            for (i <- 0 to n) result = f(result)
            result
        }
        t _
    } */

    //PART B TESTED COMPLETE
    def combineSquare[T] (f: T=>T): (T, Int) => T = { 
        def s(init: T, n: Int): T = {
            var result = init
            for (i <- 0 to n) result = f(result)
            result 
        }
        s _
    }

    //combineSquare(square)(1,0)
    //combineSquare(square)(6,0)
    //combineSquare(square)(10,0)

    //COMPLETED Problem 7
    def cube(n: Int) = n * n * n

    //it's an array(list) but list = (input, output)
    //want the first parameter to be the function
    //second parameter is the array in a set 
    def unitTest[T,S] (f: T=>S, theArray: Array[(T,S)]): Int = {
        var err = 0
        for ((i,j) <- theArray) {
            if (f(i) != j) { //check if the input is equal to the output; cube the (i) 
                err = err + 1 //if it's not equal, then there is an error
            }
        }
        err
    }
    //unitTest(cube, Array((1, 1), (2, 8), (3, 9), (4, 64), (5, 124)))
}