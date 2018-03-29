import scala.collection.mutable.ListBuffer

/**
  * @author Jigar D. Prajapati
  *
  * In this method we prompt user for commands that does a specific task
  */
object Commands {

  def main(args: Array[String]): Unit = {

    var myList = new ListBuffer[Float]

    print ("\n* * *Floating-point program started * * *")
    print("\nEnter a command: ")
    var response = scala.io.StdIn.readLine().split(" ") //prompt the user to input a command
    var x = response(0)

    //while the user doesnt type "End", this program will keep prompting the user for a command
    while (x != "End") {
      //condition check for "Insert"
      if (x == "Insert") {
        try {
          var y = response(1).toFloat
          myList.append(y) //add the element
          var myList2 = myList.sorted //sort the list
          print("\nThe array currently contains:\n")
          var count = 0
          for (i <- myList2) {
            var floatNumber = i.toFloat
            println(f"Value[$count] = $floatNumber%.5f")
          }
        }catch {
          case _: Throwable => print("Error: No or Wrong Input\n")
        }
      }
      //condition check for "Delete"
      else if (x == "Delete") {
        try {
          var y = response(1).toFloat
          myList -= y //remove the element
          var myList2 = myList.sorted //sort again
          print("\nThe array currently contains:\n")
          var count = 0
          for (i <- myList2) {
            var floatNumber = i.toFloat
            println(f"Value[$count] = $floatNumber%.5f")
          }
        }catch {
          case _: Throwable => print("Error: No or Wrong Input\n")
        }
      }
        //condition check for "Sum"
      else if (x == "Sum") {
        val a = myList.sum
        println(s"The total is $a")
      }
      else {
        //prints an error if user enters an invalid command
        print("\nError: Invalid command entered (Try: Insert <number>, Delete <number>, Sum or End to quit")
      }

      print("\nEnter a command: ")
      //program will keep prompting the user for a command
      response = scala.io.StdIn.readLine().split(" ")
      x = response(0)
    }
  }
}
