package com.github.kantefier.weatherparse

import java.util.Calendar

import scala.util.parsing.combinator.RegexParsers
import java.io._
import scala.io.Source
import MatrixHelper._

object WeatherParser {

  val workingDirPath = "C:\\WeatherParser"
  val dataFilename = "cetdl1772on.dat"
  val resultFilename = s"output_${Calendar.getInstance().getTimeInMillis % 10000}.dat"

  def main (args: Array[String]): Unit = {
    val dataInputFile = new File(s"$workingDirPath\\$dataFilename")
    val dataOutputFile = new File(s"$workingDirPath\\$resultFilename")

    if(!dataInputFile.exists())
      throw new Exception(s"Data file not found: $workingDirPath\\$dataFilename")

    val dataSource = Source.fromFile(dataInputFile).getLines()
    val dataDestination = new PrintWriter(dataOutputFile)
    var linecounter = 0
    val linesBuffer = new StringBuilder

    while(dataSource.hasNext) {
      linecounter += 1
      linesBuffer.append(dataSource.next())
      if(linecounter % 31 == 0) {
        val yearDataMatrix = linesBuffer.mkString.lines.map(line => SimpleLineParser.parseAll(SimpleLineParser.lineOfData, line)).map{
          case SimpleLineParser.Success(resultRow, _) => resultRow
          case SimpleLineParser.Failure(_, _) => Nil
          case SimpleLineParser.Error(_, _) => Nil
        }.toList
        dataDestination.println(parseOneYearData(yearDataMatrix).mkString("\n"))
        linesBuffer.setLength(0)
      } else {
        linesBuffer.append(System.lineSeparator())
      }
    }
    dataDestination.flush()
    dataDestination.close()
  }

  def parseOneYearData(dataMatrix: Matrix): List[String] = {
    val (year, yearlessMatrix) = (dataMatrix.T.head.head, dataMatrix.T.tail)
    val (days, dataByMonthsMatrix) = (yearlessMatrix.head, yearlessMatrix.zipWithIndex.tail)
    val dataByDaysWithMonth = dataByMonthsMatrix.map(dataWithMonthNumber => (dataWithMonthNumber._1.zip(days), dataWithMonthNumber._2))
    val resultSet = dataByDaysWithMonth.flatMap(monthDataTuple => {
      val (dataWithDays, monthNumb) = monthDataTuple
      dataWithDays.flatMap(dayData => if(dayData._1 != -999) Some(s"${dayData._2}/$monthNumb/$year ${dayData._1}") else None)
    })
    resultSet
  }
}

object WeatherLineParser extends RegexParsers {
  def line: Parser[Any] = year ~ spaceSeparator ~ dayOfMonth ~ spaceSeparator ~ repsep(data, spaceSeparator)
  def year: Parser[Int] = """\d{4}""".r ^^ {_.toInt}
  def dayOfMonth: Parser[Int] = """\d{1,2}""".r ^^ {_.toInt}
  def data: Parser[Any] = """-?\d{1,3}""".r
  def spaceSeparator = """\s*""".r
}

object SimpleLineParser extends RegexParsers {
  def lineOfData: Parser[List[Int]] = opt(' ') ~> repsep(data, ' ') ^^ {_.toList}
  def data: Parser[Int] = """-?\d{1,4}""".r ^^ {_.toInt}
}