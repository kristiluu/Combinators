//Kristi Luu - Homework 3 - Combinators

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

    //COMPLETED problem 2
    def selfIter[T] (f: T=>T, n: Int): T => T = if (isZero(n)) f else selfIter(compose(f, f), dec(n))

    //println(selfIter(inc, 1)(1)) 
    //println(selfIter(inc, 3)(9)) 

    //COMPLETED Problem 3
    def countPass[T](arr: Array[T]): Int = { 
        var numOfBools = 0;
        for (element <- arr) {
            element match {
                case a: Boolean => numOfBools = numOfBools + 1
                case _ => 
            }
        }
        numOfBools
    }

    //var a = Array(true, false, 1, true, 2, 3, 4)
    //println(countPass(a))

    //COMPLETED Problem 4A 
    def factorial(init: Int, m: Int): Int = { //init = 1
        var result = 1;
        if (isZero(m)) result
        else {
            for (i <- init to m) {
                result = result * i
            }
        result  
        } 
    }

    def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
        def r(m: Int): Int = {
            if (m == 0) baseVal
            else combiner(m, r(dec(m)))
        }
        r _ 
    } 

    //COMPLETED 4B
    //println(recur(2, factorial)(1)) //2 which is 2! 
    //println(recur(3, factorial)(1)) //3 which is 3! 
    //println(recur(4, factorial)(1)) //24 which is 4!


    //COMPLETED Problem 5
    def parseDigits(digits: String): Option[Int] = if (digits.matches("[0-9]*")) Some(digits.toInt) else None
    
    def deOptionize (f: String => Option[Int]): String => Int = {
        def t(digits: String): Int = {
            if (f(digits) == None) {
                throw new Exception ("Error")
            }
            f(digits).get
        }
        t _
    }
    //println(deOptionize(parseDigits)("2"))

    //COMPLETED Problem 6 
    def square(x: Double) = x * x                        

    //PART A
    def combineIter[T] (f: T=>T): (T, Int) => T = { //Unsure about this, isn't the generic and the one that uses squareIter the same?
        def t(init: T, n: Int): T = {
            var result = init 
            for (i <- 0 to n) result = f(result)
            result 
        }
        t _
    }

    //PART B
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