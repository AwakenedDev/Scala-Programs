/**
  *This is a program that computes Fibonacci numbers
  */

object Fibonacci {

  /**
    * This method reads a postive integer and then prints the nth Fibonacci number.
    *
    * @param n
    * @return
    */
  def fib(n:Int): Int = {
    if (n > 1){
      return fib(n-1)+fib(n-2)
    }
    n
  }

  /**
    * This method reads a postive integer and then prints the nth Fibonacci number.
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    print("\n* * * Fibonacci Printer * * *\n")
    print("\nWhich Fibonacci number would you like to see: ")

    //here we check whether the input is an Interger. If not then output error
    try {
      val number = scala.io.StdIn.readInt() //here we ask for the user input
      //condition check for positive numbers
      if ((number > 0) && (number < 46)) {

        var ti = System.nanoTime() //calculate the start time
        var fibnum = fib(number)
        var tf = System.nanoTime() //calculate the stop time

        printf(f"\nFibonacci number $number is: $fibnum\n")

        var tTotal = tf - ti
        var tTotalS: Double = tTotal * (0.000000001)

        print(f"\nThis calculation required $tTotalS%.6f seconds") //print the total time
      }
      else {
        print("Error: Input is out of range")
      }
    }
    catch {
      case e : NumberFormatException => print("Error: No or Wrong Input")
    }
  }
}
