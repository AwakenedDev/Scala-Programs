import scala.collection.mutable.ListBuffer

/**
  * @author Jigar D. Prajapati
  *
  * This class contains the to do list (myList) and provides methods that modify the list as required
  */

class Commands {

  //declaration of variable
  var myList = new ListBuffer[String]
  var myString = ""
  var number1 = 0
  var number2 = 0
  var number = 0

  /**
    * this method displays the values in the list
    *
    * @return myList
    */
  def show(): ListBuffer[String] = {
    myList
  }

  /**
    * in this method we add an item(i.e string, number e.t.c) to the end of the list)
    *
    * @param string
    */
  def add(string: String): Unit = {
    this.myString = string
    myList.append(this.myString)
  }

  /**
    * moves item number1 in the list to position number2
    *
    * @param number1
    * @param number2
    */
  def move(number1: Int, number2: Int): Unit = {
    this.number1 = number1
    this.number2 = number2
    if (((0 < this.number1) && (this.number1 <= myList.length)) && ((0 < this.number2) && (this.number2 <= myList.length))) {
      var stringTemp = myList(this.number1 - 1)
      myList.remove(this.number1 - 1)
      myList.insert(this.number2 - 1, stringTemp)
    }
    else {
      println("Error: Move Unsuccessful")
    }
  }

  /**
    *in this method we mark a specified item in the list
    *
    * @param number
    */
  def complete(number: Int): Unit = {
    this.number = number
    if ((0 < this.number) && (this.number <= myList.length)) {
      myList.remove(this.number - 1)
    }
    else {
      println(s"Error: $number is not available. Complete Unsuccessful")
    }
  }
}
