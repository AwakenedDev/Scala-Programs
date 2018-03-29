import java.text.SimpleDateFormat
import scala.collection.mutable.ListBuffer

/**
  * @author Jigar D. Prajapati
  *
  */
class WeatherMethods {

  /**
    * Max temp reported by any WBAN's during August
    *
    * @param listBuffer
    * @return String that has information on the max temp
    */
  def maxTemp(listBuffer: ListBuffer[String]): String = {
    var tempMax = 0
    var counter = 0
    var i = 0
    //in this while loop we iterate through every line in the list to find the max Temp
    while (i < listBuffer.length) {
      var string = listBuffer(i).split(" ")
      //condition check
      if (string(2).toInt > tempMax) {
        tempMax = string(2).toInt
        counter = i
      }
      i += 1
    }
    var finalString = listBuffer(counter).split(" ")
    var tMax = finalString(2)
    //format date
    var date = finalString(1)
    var simpleDateFormat = new SimpleDateFormat("yyyyMMdd")
    var date3 = simpleDateFormat.parse(date)
    simpleDateFormat = new SimpleDateFormat("MMMM d, yyyy")
    var finalDate = simpleDateFormat.format(date3) //date formatted
    var stationName = finalString(5)
    var locationname = finalString(6)

    f"     $tMax%-12s |$finalDate%-20s |$stationName%-20s |$locationname%-30s\n"
  }

  /**
    * Min temp reported by any WBAN's during August 2015
    *
    * @param listBuffer
    * @return String that has information on the min temp
    */
  def minTemp(listBuffer: ListBuffer[String]): String = {
    var tempMin = 200
    var counter = 0
    var i = 0
    //in this while loop we iterate through every line in the list to find the min Temp
    while (i < listBuffer.length) {
      var string = listBuffer(i).split(" ")
      //condition check
      if (string(3).toInt < tempMin) {
        tempMin = string(3).toInt
        counter = i
      }
      i += 1
    }
    var finalString = listBuffer(counter).split(" ")
    var tMin = finalString(3)
    //format date
    var date = finalString(1)
    var simpleDateFormat = new SimpleDateFormat("yyyyMMdd")
    var date3 = simpleDateFormat.parse(date)
    simpleDateFormat = new SimpleDateFormat("MMMM d, yyyy")
    var finalDate = simpleDateFormat.format(date3) //date formatted
    var stationName = finalString(5)
    var locationname = finalString(6)

    f"     $tMin%-12s |$finalDate%-20s |$stationName%-20s |$locationname%-30s\n"
  }

  /**
    *The average average (Tavg) temperature for all 25 reporting stations in August 2015
    *
    * @param listBuffer
    * @return the value of average TAvg
    */
  def avgTemp(listBuffer: ListBuffer[String]): Float = {
    var avgSum = 0.00
    var i = 0
    //condition check and calculation
    while (i < listBuffer.length) {
      var string = listBuffer(i).split(" ")
      avgSum += string(4).toFloat
      i += 1
    }
    val totalNum = listBuffer.length - 1
    val avgTempAll = avgSum / totalNum
    avgTempAll.toFloat
  }

  /**
    * The hottest day in Pennsylvania in August 2015
    *
    * @param listBuffer
    * @return the string that has information of the hottest day in Pennsylvania
    */
  def hotDay(listBuffer: ListBuffer[String]): String = {
    //in this while loop we extract the Data and Tavg info and then store it in list2
    var list2 = new ListBuffer[String]()
    var i = 0
    while (i < listBuffer.length) {
      var string = listBuffer(i).split(" ")
      var finalString = string(1) + " " + string(2) //get the date and the min temp
      list2.insert(i, finalString)
      i += 1
    }
    list2 = list2.sorted

    //After storing the Date and Tmax, we sort the data in ne list (list2)
    var avgTmax, counter, j, k, m = 0
    var l = 1
    var list3 = new ListBuffer[String]()
    //here we iterate through each index (line) of list 2 and calculate average Tmax for each day
    while (j < list2.length) {
      var string2 = list2(j).split(" ")
      if (l < list2.length) {
        var stringAhead = list2(l).split(" ")
        /**
          * here is the condition to see if the dates are same. If dates are the same, add the temp, when dates are not same, calculate average.
          * remember since list2 is sorted, the dates in the list are also sorted meaning the same date's value will be adjacent untill a certain index, then the next day's values are presented
          */
        if (string2(0).toInt != stringAhead(0).toInt) {
          counter = j
          counter -= k
          k = j
          avgTmax = avgTmax + string2(1).toInt //add all same date's vaues
          var avgT = avgTmax.toFloat / counter.toFloat
          var hotDayTemp = string2(0) + " " + avgT
          list3.insert(m, hotDayTemp)
          m += 1
          avgTmax = 0
        }
        else {
          avgTmax = avgTmax + string2(1).toInt
        }
      }
      l += 1
      j += 1
    }

    //in this loop we find the hottest day by comparing average of Tmax for each day
    var avgTempMax = 0.0
    var maxCounter, n = 0
    while (n < list3.length) {
      var string3 = list3(n).split(" ")
      //condition check for min Temp among the average values
      if (string3(1).toFloat > avgTempMax) {
        avgTempMax = string3(1).toFloat
        maxCounter = n
      }
      n += 1
    }
    var finalString2 = list3(maxCounter).split(" ")
    var avgFinalTmax = finalString2(1)
    //format date
    var date = finalString2(0)
    var simpleDateFormat = new SimpleDateFormat("yyyyMMdd")
    var date3 = simpleDateFormat.parse(date)
    simpleDateFormat = new SimpleDateFormat("MMMM d, yyyy")
    var finalDate = simpleDateFormat.format(date3)//date formatted

    finalDate + "_" + avgFinalTmax
  }

  /**
    * The coldest day in Pennsylvania in August 2015
    *
    * @param listBuffer
    * @return the string that has information of the coldest day in Pennsylvania
    */
  def coldDay(listBuffer: ListBuffer[String]): String = {
    //in this while loop we extract the Data and Tavg info and then store it in list2
    var list2 = new ListBuffer[String]()
    var i = 0
    while (i < listBuffer.length) {
      var string = listBuffer(i).split(" ")
      var finalString = string(1) + " " + string(3) //get the date and the min temp
      list2.insert(i, finalString)
      i += 1
    }
    list2 = list2.sorted

    //After storing the Date and Tmax, we sort the data in ne list (list2)
    var avgTmax, counter, j, k, m = 0
    var l = 1
    var list3 = new ListBuffer[String]()
    //here we iterate through each index (line) of list 2 and calculate average Tmax for each day
    while (j < list2.length) {
      var string2 = list2(j).split(" ")
      if (l < list2.length) {
        var stringAhead = list2(l).split(" ")
        /**
          * here is the condition to see if the dates are same. If dates are the same, add the temp, when dates are not same, calculate average.
          * remember since list2 is sorted, the dates in the list are also sorted meaning the same date's value will be adjacent untill a certain index, then the next day's values are presented
          */
        if (string2(0).toInt != stringAhead(0).toInt) {
          counter = j
          counter -= k
          k = j
          avgTmax = avgTmax + string2(1).toInt //add all same date's vaues
          var avgT = avgTmax.toFloat / counter.toFloat
          var coldDayTemp = string2(0) + " " + avgT
          list3.insert(m, coldDayTemp)
          m += 1
          avgTmax = 0
        }
        else {
          avgTmax = avgTmax + string2(1).toInt
        }
      }
      l += 1
      j += 1
    }

    //in this loop we find the hottest day by comparing average of Tmax for each day
    var avgTempMax = 200.0
    var maxCounter, n = 0
    while (n < list3.length) {
      var string3 = list3(n).split(" ")
      //condition check for min Temp among the average values
      if (string3(1).toFloat < avgTempMax) {
        avgTempMax = string3(1).toFloat
        maxCounter = n
      }
      n += 1
    }
    var finalString2 = list3(maxCounter).split(" ")
    var avgFinalTmax = finalString2(1)
    //format date
    var date = finalString2(0)
    var simpleDateFormat = new SimpleDateFormat("yyyyMMdd")
    var date3 = simpleDateFormat.parse(date)
    simpleDateFormat = new SimpleDateFormat("MMMM d, yyyy")
    var finalDate = simpleDateFormat.format(date3) //date formatted

    finalDate + "_" + avgFinalTmax
  }

}
