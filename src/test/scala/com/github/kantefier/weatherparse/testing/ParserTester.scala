package com.github.kantefier.weatherparse.testing

import com.github.kantefier.weatherparse.SimpleLineParser
import com.github.kantefier.weatherparse.MatrixHelper._
import org.scalatest.FlatSpec

class ParserTester extends FlatSpec {
  val testInputData = """1772    1   32  -15   18   25   87  128  187  177  105  111   78  112
                    | 1772    2   20    7   28   38   77  138  154  158  143  150   85   62
                    | 1772    3   27   15   36   33   84  170  139  153  113  124   83   60
                    | 1772    4   27  -25   61   58   96   90  151  160  173  114   60   47
                    | 1772    5   15   -5   68   69  133  146  179  170  173  116   83   50
                    | 1772    6   22  -45   51   77  113  105  175  198  160  134  134   42
                    | 1772    7   25   12   58   87  106  162  146  194  158  114  134   32
                    | 1772    8    0   47   46  104   84  181  163  177  135   94   95   55
                    | 1772    9    0   56   21   87   67  155  179  155  143  106   70   62
                    | 1772   10   45   28   13   67   59  148  177  153  120  119  122  100
                    | 1772   11   62   20   16   58   87  176  173  136  143  109   95   45
                    | 1772   12   52   40    8  109  111  152  160  139  160  138   92   67
                    | 1772   13   25   57   -2   99  116  143  168  160  133  101   50   82
                    | 1772   14   17   38  -17   48   74  146  203  153  143   91   43   77
                    | 1772   15   30    2  -14   58  101  171  184  163  143  124   73   70
                    | 1772   16   20    5   -2   33  137  171  192  155  128  138   48   87
                    | 1772   17  -18   25    8   45  121  193  165  174  133  134   55   90
                    | 1772   18  -13   -3   41   48   79  171  170  192  123  119   60   60
                    | 1772   19  -18    0   73   -2   92  184  151  194  138  114   36  112
                    | 1772   20  -10    7   53   53   74  195  151  136  108  129   78   97
                    | 1772   21   -6    7   51   53   96  162  151  136  143  119   85   67
                    | 1772   22   15   -3   98   74  111  171  158  155  135   84   26   10
                    | 1772   23   12  -10   76   84  136  179  197  155  110   91   92  -28
                    | 1772   24    5  -10   73  104  118  195  192  165   99  119   53    7
                    | 1772   25   12   30   81   74  131  171  192  134  113  121   45  -40
                    | 1772   26   15   85   71   74  108  208  160  158  105  104   63   -8
                    | 1772   27    0   46   66   74   77  198  156  144   76  104   45    5
                    | 1772   28   15   77   86   64  116  167  151  155   66   84   60   10
                    | 1772   29  -33   56   83   50  113  131  170  182  135  140   63   12
                    | 1772   30  -10 -999   66   77  121  122  179  163  143  143   55   15
                    | 1772   31   -8 -999   46 -999  108 -999  168  144 -999  145 -999   22          """.stripMargin

  info("Feeding one line of data")

  "A line" should "be parsed" ignore {
    val inputData = """1772    1   32  -15   18   25   87  128  187  177  105  111   78  112"""
    val result = SimpleLineParser.parseAll(SimpleLineParser.lineOfData, inputData) match {
      case SimpleLineParser.Success(resultRow, _) => resultRow.toString
      case SimpleLineParser.Failure(msg, _) => msg
      case SimpleLineParser.Error(msg, _) => msg
    }
    println(result)
  }

  info("Trying to feed 31 rows of experimental data")

  "A matrix" should "be built" ignore {
    val result = testInputData.lines.map(line => SimpleLineParser.parseAll(SimpleLineParser.lineOfData, line)).map{
      case SimpleLineParser.Success(resultRow, _) => resultRow
      case SimpleLineParser.Failure(_, _) => Nil
      case SimpleLineParser.Error(_, _) => Nil
    }.toList
    println(result.toMatrixString)
  }

  info("Testing matrixes")

  "A matrix" should "be transposed" ignore {
    val result = testInputData.lines.map(line => SimpleLineParser.parseAll(SimpleLineParser.lineOfData, line)).map{
      case SimpleLineParser.Success(resultRow, _) => resultRow
      case SimpleLineParser.Failure(_, _) => Nil
      case SimpleLineParser.Error(_, _) => Nil
    }.toList
    println(result.T.toMatrixString)
  }

  info("taking first approach to desired result")

  "a proper matrix" should "be here" in {
    val parseResult = testInputData.lines.map(line => SimpleLineParser.parseAll(SimpleLineParser.lineOfData, line)).map{
      case SimpleLineParser.Success(resultRow, _) => resultRow
      case SimpleLineParser.Failure(_, _) => Nil
      case SimpleLineParser.Error(_, _) => Nil
    }.toList
    val (year, yearlessMatrix) = (parseResult.T.head.head, parseResult.T.tail)
    val (days, dataByMonthsMatrix) = (yearlessMatrix.head, yearlessMatrix.zipWithIndex.tail)
    val dataByDaysWithMonth = dataByMonthsMatrix.map(dataWithMonthNumber => (dataWithMonthNumber._1.zip(days), dataWithMonthNumber._2))
    val resultSet = dataByDaysWithMonth.flatMap(monthDataTuple => {
      val (dataWithDays, monthNumb) = monthDataTuple
      dataWithDays.flatMap(dayData => if(dayData._1 != -999) Some((s"${dayData._2}/${monthNumb}/${year}", dayData._1)) else None)
    })
    resultSet.map(println(_))
  }
}
