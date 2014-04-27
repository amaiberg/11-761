package project

import scala.collection.SortedMap
import project.Parse.Document
import Ngram._
import chalk.text.transform.StopWordFilter
import scala.collection.parallel.ParIterable
object Ngram {
 /* def genNGrams(corpus: ParIterable[String], n: Int, size: Int) = {
    val ngrams = corpus.take(size).flatMap(sent => sent.split(" ").sliding(n)).map(_.toList)
    //maybe use ephemeral stream...
    def genModel(corpus: List[List[String]], model: Map[List[String], Int]): Map[List[String], Int] = corpus match {
      case Nil => model
      case head :: tail =>
        val newModel = if (model.contains(head)) model ++ Map(head -> (model(head) + 1)) else model ++ Map(head -> 1)
        genModel(tail, newModel)
    }
    val counts = genModel(ngrams.toList, Map())
    //compute Good-Turing and discard singletons
    gt(counts).filterNot { case (k, v) => v == 1 }
  }

  /*does Good-Turing smoothing using Katz method for zero counts*/
  def gt(counts: Map[List[String], Int]): Map[List[String], Double] = {
    val counted: SortedMap[Int, Set[List[String]]] =
      SortedMap.empty[Int, Set[List[String]]] ++
        counts.groupBy { case (k, v) => v }.mapValues { _.keySet }

    //the katz constant
    val k = 5
    println(counted.keys)
    val N = counts.size.toDouble

    //TODO: maybe use Gale and Sampson regression instead
    def gtKatzSmooth(c: Int): Double = {
      val x = (c + 1) * counted(c + 1).size.toDouble / counted(c).size.toDouble
      val y = c * (k + 1) * counted(k + 1).size.toDouble / counted(1).size.toDouble
      (x - y) / (1d - y)
    }
    println(counted.keys.take(10))
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

class NGramModel(corpus: ParIterable[String], trainSet: List[Document], n: Int, size: Int) extends Model(corpus, trainSet) {
  val sFilter = StopWordFilter("en")
  val trigramModel = genNGrams(corpus, n, size)

  //need to train t
  val t = 2 // what should this value be??

  println("training...")
  /* val klScores = trainSet.map { doc =>
    {
      val filteredSents = doc.sents.map(s => sFilter(s).toList)
      val ngrams = filteredSents.filter(s => !s.isEmpty && trigramModel.contains(s)).flatMap(sent => sent.sliding(n)).map(_.toList)
      //   val ngrams = doc.sents.filterNot(_.isEmpty).flatMap(sent => sent.sliding(n)).map(_.toList)
      (doc.id, math.pow(2, kl(trigramModel, ngrams.filter(trigramModel.contains(_)))), doc.real)
      //  print("kl: (" + doc.id + ")" + kl(model, ngrams.filter(model.contains(_))) + " real? " + doc.real + "\n")
    }
  }

  val sortedKl = klScores.sortBy(_._2)
*/ /*
  sortedKl.foreach { doc =>
    print("kl: (" + doc._1 + ")" + doc._2 + " real? " + doc._3 + "\n")
  }

  println("separation quality")
  println("#real (above median): " + sortedKl.take(sortedKl.length / 2).count(_._3))
  println("#real (below median): " + sortedKl.drop(sortedKl.length / 2).count(_._3))

  */
  override def apply(doc: Document): Double = {
    val filteredSents = doc.sents.map(s => sFilter(s).toList)
    val ngrams = filteredSents.filter(s => !s.isEmpty && trigramModel.contains(s)).flatMap(sent => sent.sliding(n)).map(_.toList)
    val dist = if (ngrams.isEmpty) 0.1 else kl(trigramModel, ngrams)
    //logistic regression
    math.pow(2, dist)
  }
}

class InterpolatedGram(corpus: ParIterable[String], trainSet: List[Document], models: NGramModel*) extends Model {
  val sFilter = StopWordFilter("en")

  val klScores = trainSet.map { doc => (doc.id, apply(doc), doc.real) }
  val sortedKl = klScores.sortBy(_._2)
  sortedKl.foreach { doc =>
    print("kl: (" + doc._1 + ")" + doc._2 + " real? " + doc._3 + "\n")
  }
  println("separation quality")
  println("#real (above median): " + sortedKl.take(sortedKl.length / 2).count(_._3))
  println("#real (below median): " + sortedKl.drop(sortedKl.length / 2).count(_._3))

  def apply(doc: Document): Double =
    models.map(model => model(doc)).reduce((x, y) => sigmoid(y - x))

  def sigmoid(t: Double) = 1d / (1d + math.exp(-t))
}
*/
  /*
object InterpolatedGram {
  def apply(corpus: ParIterable[String], trainSet: List[Document], models: NGramModel*) = new InterpolatedGram(corpus: ParIterable[String], trainSet: List[Document], models: _*)

}

object NGramModel {
  def apply(corpus: ParIterable[String], trainSet: List[Document], n: Int, size: Int) = new NGramModel(corpus, trainSet, n, size)

* 
*/
  
}