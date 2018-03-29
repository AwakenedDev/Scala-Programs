import scala.io.Source
import java.io.FileNotFoundException
import scala.collection.mutable.ListBuffer

/**
  * @author Jigar D. Prajapati
  *
  * This class has methods that creates an ArrayList of WbanRecords, reads the weather data then uses the Array list data to produce 5 reports
  */
object weatherMain {

  def main(args: Array[String]): Unit = {

    val pennWeather = new WeatherMethods()

    var path = "src/main/scala/" //define the path of the stored files

    print("\nEnter the filename (e.g example.txt): ")
    var response = scala.io.StdIn.readLine() //read filename from user
    var TempList = new ListBuffer[String]()

    try {
      val bufferedSource = Source.fromFile(path + response) //open file
      var i = 0
      for (line <- bufferedSource.getLines) {
        TempList.insert(i, line) //read each line in the file to a list
        i += 1
      }

      bufferedSource.close //close file

      var temp1 = "MaxTemp(F)"
      var temp2 = "Date"
      var temp3 = "Station"
      var temp4 = "Location"

      //rints maxTemp info
      val tMax = pennWeather.maxTemp(TempList) //prints maxTemp info
      println("\n1. Maximum temperature reported by any of the WBAN's is: \n")
      println(f"     $temp1%-12s |$temp2%-20s |$temp3%-20s |$temp4%-30s")
      println("--------------------------------------------------------------------------------------------")
      println(tMax)

      //prints minTemp info
      temp1 = "MinTemp(F)"
      val tMin = pennWeather.minTemp(TempList) //prints minTemp info
      println("\n2. Minimum temperature reported by any of the WBAN's is: \n")
      println(f"     $temp1%-12s |$temp2%-20s |$temp3%-20s |$temp4%-30s")
      println("--------------------------------------------------------------------------------------------")
      println(tMin)

      //prints sverageTemp info
      val tAvg = pennWeather.avgTemp(TempList)
      println(f"\n3. Average average (Tavg) temperature was $tAvg%.2f Fahrenheit\n")

      //prints hottest day info
      var avgTmax = pennWeather.hotDay(TempList).split("_")
      val dateMax = avgTmax(0)
      val hotMax = avgTmax(1).toFloat
      println(f"\n4. The hottest day in Pennsylvania was on $dateMax with average maximum temperature of $hotMax%.2f Fahrenheit\n")

      //printcoldest day info
      var avgTmin = pennWeather.coldDay(TempList).split("_")
      val dateMin = avgTmin(0)
      val coldTmin = avgTmin(1).toFloat
      println(f"\n5. The coldest day in Pennsylvania was on $dateMin with average minimum temperature of $coldTmin%.2f Fahrenheit\n")


    } catch {
      case e: FileNotFoundException => print("Error: File Not Found\n") //prints error if file is not found in the path defined
    }

  }

}
