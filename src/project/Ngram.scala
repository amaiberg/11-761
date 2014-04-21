package project

import scala.collection.SortedMap
import Run._
object Ngram {
  def genNGrams(corpus: List[String], n: Int = 3, size: Int = 100000000)  = {
    val ngrams = corpus.flatMap(sent => sent.split(" ").sliding(n)).map(_.toList)
    //maybe use ephemeral stream...
    def genModel(corpus: List[List[String]], model: Map[List[String], Int]): Map[List[String], Int] = corpus match {
      case Nil => model
      case head :: tail =>
        val newModel = if (model.contains(head)) model ++ Map(head -> (model(head) + 1)) else model ++ Map(head -> 1)
        genModel(tail, newModel)
    }
    val counts = genModel(ngrams, Map())
    //compute Good-Turing and discard singletons
    gt(counts).filterNot { case (k, v) => v == 1 }
  }

  /*does Good-Turing smoothing using Katz method for zero counts*/
  def gt(counts: Map[List[String], Int]): Map[List[String], Double] = {
    val counted: SortedMap[Int, Set[List[String]]] =
      SortedMap.empty[Int, Set[List[String]]] ++
        counts.groupBy { case (k, v) => v }.mapValues { _.keySet }

    //the katz constant
    val k = 10
    println(counted.keys)
    val N = counts.size.toDouble

    //TODO: maybe use Gale and Sampson regression instead
    def gtKatzSmooth(c: Int): Double = {
      val x = (c + 1) * counted(c + 1).size.toDouble / counted(c).size.toDouble
      val y = c * (k + 1) * counted(k + 1).size.toDouble / counted(1).size.toDouble
      (x - y) / (1d - y)
    }
    println(counted.keys.take(5))
    val smoothedNgram =
      //MLE (relative frequencies) for i>K
      counted.drop(k).flatMap { case (_, v) => v.map(gram => gram -> v.size / N) } ++
        counted.keys.toList.take(k).flatMap {
          case i =>
            counted(i).map(gram => gram -> gtKatzSmooth(i))
        } // Katz-smoothing for the rest
    smoothedNgram.toMap
  } //> gt: (counts: Map[List[String],Int])Map[List[String],Double]

 
  
}