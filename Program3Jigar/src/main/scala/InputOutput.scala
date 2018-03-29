/**
  * @author Jigar D. Prajapati
  *
  * In this object, we prompt the user for a command and then read the response and call the Commands' class
  */
object InputOutput {

  def main(args: Array[String]): Unit = {

    print("\n* * * To Do List * * *")
    println("\nEnter a command(Show, Add, Move, Complete) or End:")
    var response = scala.io.StdIn.readLine().split(" ") //ask for user response
    var a = response(0)

    var myToDoList = new Commands()

    //program keeps prompting user to input commands untill user types "End"
    while (a != "End") {
      try {
        //condition check
        if (a == "Show") {
          var listCopy = myToDoList.show()
          for (i <- 0 to (listCopy.length - 1)) {
            println((i + 1).toString + ". " + listCopy(i))
          }
        }
        else if (a == "Add") {
          var temporaryString = response.mkString(" ")
          var responseTemp = temporaryString.split(" ", 2)
          var string = responseTemp(1)
          myToDoList.add(string)
        }
        else if (a == "Move") {
          var num1 = response(1).toInt
          var num2 = response(2).toInt
          myToDoList.move(num1, num2)
        }
        else if (a == "Complete") {
          var num = response(1).toInt
          myToDoList.complete(num)
        }
        else {
          println(a + " is an unrecognized command")
        }
      }
      catch {
        case _: Throwable => println("Error: Wrong or No input") //if exception found like Numberformatexception, then this will print the error to guide the user
      }

      println("\nEnter a command(Show, Add, Move, Complete) or End:")
      response = scala.io.StdIn.readLine().split(" ")
      a = response(0)
    }
  }
}
