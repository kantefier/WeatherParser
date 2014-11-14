package com.github.kantefier.weatherparse

import scala.util.parsing.combinator.RegexParsers
import MatrixHelper._

object WeatherParser {
  def main (args: Array[String]): Unit = {
    ???
  }

  def parseOneYearData(dataMatrix: Matrix): List[(String, Int)] = {
    val (year, yearlessMatrix) = (dataMatrix.T.head.head, dataMatrix.T.tail)
    val (days, dataByMonthsMatrix) = (yearlessMatrix.head, yearlessMatrix.zipWithIndex.tail)
    val dataByDaysWithMonth = dataByMonthsMatrix.map(dataWithMonthNumber => (dataWithMonthNumber._1.zip(days), dataWithMonthNumber._2))
    val resultSet = dataByDaysWithMonth.flatMap(monthDataTuple => {
      val (dataWithDays, monthNumb) = monthDataTuple
      dataWithDays.flatMap(dayData => if(dayData._1 != -999) Some((s"${dayData._2}/${monthNumb}/${year}", dayData._1)) else None)
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