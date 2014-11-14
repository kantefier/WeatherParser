package com.github.kantefier.weatherparse

import language._

package object MatrixHelper {
  type Row = List[Int]
  type Matrix = List[Row]

  def dotProduct(vector1: Row, vector2: Row) =
    vector1.zip(vector2).map((t: (Int, Int)) => t._1 * t._2).reduceLeft(_ + _)

  def transpose(matr: Matrix): Matrix =
    if(matr.head.isEmpty)
      Nil
    else
      matr.map(_.head) :: transpose(matr.map(_.tail))

  def mXm(m1: Matrix, m2: Matrix) =
    for(m1row <- m1) yield
      for(m2col <- transpose(m2)) yield
        dotProduct(m1row, m2col)

  implicit def matrixToRichMatrix(m: Matrix): RichMatrix = new RichMatrix(m)

  case class RichMatrix(m: Matrix) {
    def T = transpose(m)

    def *(that: RichMatrix) = mXm(this.m, that.m)

    /** Warning: slow method! O(n) */
    def apply(i: Int, j: Int) = m(i)(j)

    def rowCount = m.length
    def colCount = m.head.length

    def toMatrixString = "\n" + m.map{
      _.map{"\t" + _}.reduceLeft(_ + _) + "\n"
    }.reduceLeft(_ + _)
  }

  object Matrix {
    def apply(rowCount: Int, colCount: Int) (f:(Int, Int) => Int) = {
      for(i <- 1 to rowCount) yield
        (for(j <- 1 to colCount) yield f(i,j)).toList
    }.toList
  }
}
