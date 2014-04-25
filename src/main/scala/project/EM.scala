package project

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import breeze.numerics._
object EM {
  def em(lambdas: DenseVector[Double], P: DenseMatrix[Double], epsilon: Double = 0.001): DenseVector[Double] = {
    val N = P.rows
    def em(lambdas: DenseVector[Double])(implicit l1: Double, i: Int = 0): DenseVector[Double] = {
      //update
      // val nlambdas = DenseVector.tabulate(3)(i => (1d / N) * sum(((P(::, i) * lambdas(i)) :/ P * lambdas)))
      val nlambdas = sum((P.t(::, *) :* lambdas).t(::, *) :/ (P * lambdas), Axis._0).toDenseVector * (1d / N)
      //compute likelihood
      //avg. log-likelihood
      val l2 = log(P * nlambdas).sum
      val l = (l2 - l1) / abs(l1) * 100
      println("weights: " + nlambdas)
      printf("avg. log-likelihood (t=%d): %1.4f\n", i + 1, l2)
      printf("log-likelihood ratio (t=%d): %1.4f\n", i, l)
      // println("loglike: " + l)
      //iterate or stop
      if (l > epsilon)
        em(nlambdas)(l2, i + 1)
      else {
      //  printf("avg. log-likelihood (t=%d): %1.4f\n", i + 1, l2)
        nlambdas
      }
    }
    //run em, initialize first log-likelihood
    em(lambdas)(log(P * lambdas).sum)
  }
  def l(P: DenseMatrix[Double], lambdas: Double*): Double = log(P * DenseVector(lambdas: _*)).sum


}