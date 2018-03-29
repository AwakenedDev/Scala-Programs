import scala.io.Source

/**
  * @author Jigar D. Prajapati
  *
  * TicTacToe game (Computer vs. User) where the computer learns from its mistake.
  */
object TicTacToe {
  var board = new Array[Char](9)
  var currentPlayerSymbol = 'X'
  var badMove = Array.ofDim[Char](500, 9)
  var totalBadMoves = 0

  //print board
  def printBoard(): Unit = {
    println(board(0) + " | " + board(1) + " | " + board(2))
    println("---------")
    println(board(3) + " | " + board(4) + " | " + board(5))
    println("---------")
    println(board(6) + " | " + board(7) + " | " + board(8) + "\n")
  }

  //initialize board
  def initializeBoard(): Unit = {
    readBadMoves()
    for (i <- 0 to 8) {
      board(i) = ' '
    }
  }

  /**
    * check if game is draw
    *
    * @return isFull
    */
  def isBoardFull(): Boolean = {
    var isFull = true
    for (i <- 0 to 8) {
      if (board(i) == ' ') {
        isFull = false
      }
    }
    return isFull
  }

  /**
    * checks winner
    *
    * @return boolean
    */
  def checkWinner(): Boolean = {
    //check rows
    if (board(0) != ' ' && board(0) == board(1) && board(1) == board(2)) {
      return true
    }
    if (board(3) != ' ' && board(3) == board(4) && board(4) == board(5)) {
      return true
    }
    if (board(6) != ' ' && board(6) == board(7) && board(7) == board(8)) {
      return true
    }
    //check columns
    if (board(0) != ' ' && board(0) == board(3) && board(3) == board(6)) {
      return true
    }
    if (board(1) != ' ' && board(1) == board(4) && board(4) == board(7)) {
      return true
    }
    if (board(2) != ' ' && board(2) == board(5) && board(5) == board(8)) {
      return true
    }
    //check diagonal \
    if (board(0) != ' ' && board(0) == board(4) && board(4) == board(8)) {
      return true
    }
    //check diagonal /
    if (board(2) != ' ' && board(2) == board(4) && board(4) == board(6)) {
      return true
    }
    return false
  }

  //player selects move
  def playerMove(): Unit = {
    while (true) {
      try {
        print("Enter a move (1-9): ")
        var input = scala.io.StdIn.readInt()
        input -= 1
        if (input >= 0 && input < 9) {
          if (board(input) == ' ') {
            board(input) = 'O'
            return
          } else {
            println("This position is not empty!!!\n")
          }
        } else {
          println("Enter a legal move (1-9)!!!\n")
        }
      }catch {
        case e: Throwable => println("Enter a legal move (1-9)!!!\n")
      }
    }
  }

  //computer selects move
  def computerMove(): Unit = {
    val r = new scala.util.Random
    while (true) {
      var computerPosition = checkBadMoves()
      if (computerPosition < 0) {
        computerPosition = r.nextInt(9)
        if (computerPosition > 0 && computerPosition < 9) {
          if (board(computerPosition) == ' ') {
            board(computerPosition) = 'X'
            return
          }
        }
      } else {
        board(computerPosition) = 'X'
        return
      }
    }
  }

  //check bad moves
  def checkBadMoves(): Int = {
    if (totalBadMoves == 0) {
      return -1
    }
    for (i <- 0 to totalBadMoves - 1) {
      var couterTheSameMoves = 0
      for (j <- 0 to 8) {
        if (badMove(i)(j) == board(j) && badMove(i)(j) != ' ') {
          couterTheSameMoves += 1
        }
      }
      if (couterTheSameMoves == 4) {
        for (j <- 0 to 8) {
          if (badMove(i)(j) == 'O' && board(j) == ' ') {
            return j
          }
        }
      }
    }
    return -1
  }

  //play game
  def play(): Unit = {
    initializeBoard()
    while (true) {
      computerMove()
      printBoard()
      if (checkWinner()) {
        println("Computer is a winner!")
        return
      } else if (isBoardFull()) {
        print("It is a draw!")
        return
      }
      playerMove()
      printBoard()
      if (checkWinner()) {
        //write out the new bad move list just before the program terminates.
        saveBadMoves()
        println("Player is a winner!")
        return
      } else if (isBoardFull()) {
        print("This game ends in a tie!")
        return
      }
    }
  }

  //This method saves bad moves to file
  def saveBadMoves(): Unit = {
    val writer = new java.io.PrintWriter(new java.io.FileOutputStream("src/main/scala/badMoves.txt", true));
    for (i <- 0 to 7) {
      writer.print(board(i) + ",")
    }
    writer.println(board(8))
    writer.flush()
    writer.close()
  }

  //This function reads a file of bad moves (if it is available) at the start of the program
  def readBadMoves(): Unit = {
    var fileName = "src/main/scala/badMoves.txt"
    try {
      for (line <- Source.fromFile(fileName).getLines()) {
        val result = line.split(',')
        var c = 0
        for (str <- result) {
          badMove(totalBadMoves)(c) = str.charAt(0)
          c += 1
        }
        totalBadMoves += 1
      }
    } catch {
      case _: Throwable => print("")
    }

  }

  //main method
  def main(args: Array[String]) {

    while (true) {
      play()
      var answer = ""
      while (answer.compareToIgnoreCase("y") != 0 &&
        answer.compareToIgnoreCase("n") != 0) {
        print("\n\nWould you like to play again? (y/n): ")
        answer = scala.io.StdIn.readLine()
        if (answer.compareToIgnoreCase("n") == 0) {
          return
        }
      }
    }
  }
}