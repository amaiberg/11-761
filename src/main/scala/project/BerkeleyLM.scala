package project

import edu.berkeley.nlp.lm.io.LmReaders
import project.Run.Model
import Parse._
import Run._
import scala.collection.JavaConversions._
import edu.berkeley.nlp.lm.ArrayEncodedProbBackoffLm
import BerkeleyLM._
import java.io.File
import EM._
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
object BerkeleyLM {

  def genGram(n: Int) = new BerkeleyNGram(n)

  type EventModel[E] = E => Double

}

sealed class BerkeleyNGram(n: Int) extends EventModel[java.util.List[String]] {
 val path = "/home/mog/dev/11-761/project/workspace/project/"

  val ngramLm = n match {
    case 2 | 3 => LmReaders.readLmBinary(path + "LM-kn" + n + "-train-100MW.binlm").asInstanceOf[ArrayEncodedProbBackoffLm[String]];
    case 4 => LmReaders.readLmBinary(path + "LM-kn" + n + "-train-50MW.binlm").asInstanceOf[ArrayEncodedProbBackoffLm[String]];
    case 5 => LmReaders.readLmBinary(path + "LM-kn" + n + "-train-20MW.binlm").asInstanceOf[ArrayEncodedProbBackoffLm[String]]
  }
  def apply(ngram: java.util.List[String]): Double = {
    math.exp(ngramLm.getLogProb(ngram))
  }
}

class NGramModel(trainSet: List[Document], n: Int) extends Model(trainSet) {
  //val sFilter = StopWordFilter("en")
  val ngramModel = genGram(n)

  //need to train t
  val t = 2 // what should this value be??

  println("training...")
  ///  ngramLM.scoreSentence(arg0);
  override def apply(doc: Document): Double = {
    //  val filteredSents = doc.sents.map(s => sFilter(s).toList)
    val ngrams = doc.sents.filter(s => !s.isEmpty).flatMap(sent => sent.sliding(n)).map(_.toList)
    val prob = ngrams.map(ngram => ngramModel(asJavaList(ngram))).sum / ngrams.length.toDouble
    //logistic regression
    //dist
    prob
  }

  override def KL(doc: Document): Double = {
    //val filteredSents = doc.sents.map(s => sFilter(s).toList)
    val ngrams = doc.sents.filter(s => !s.isEmpty).flatMap(sent => sent.sliding(n)).map(_.toList)
    val dist = kl(ngramModel, ngrams.map(asJavaList(_)))
    //logistic regression
    //dist
    dist
  }
}

object NGramModel {
  def apply(trainSet: List[Document], n: Int) = new NGramModel(trainSet, n)
}

class InterpolatedModel(trainSet: List[Document], models: Model*)(implicit real: Boolean = true) extends Model {
  // val sFilter = StopWordFilter("en")

  val docs = trainSet.filter(_.real == real)

  val p = models.map(m => docs.map(m(_)))
  val N = p.length
  val P = DenseMatrix.tabulate(N, models.length)((x, y) => p(x)(y))

  val initlambdas = DenseVector.tabulate(models.length)(x => 1d / models.length.toDouble)
  val lambdas = em(initlambdas, P).toArray

  def apply(doc: Document): Double = {
    models.zip(lambdas).map { case (m, l) => l * m(doc) }.sum
  }

  override def KL(doc: Document): Double = {
    models.zip(lambdas).map { case (m, l) => l * m.KL(doc) }.sum
  }

  def sigmoid(t: Double) = 1d / (1d + math.exp(-t))
}

object InterpolatedModel {
  def apply(trainSet: List[Document], models: Model*)(implicit real: Boolean = true) = new InterpolatedModel(trainSet: List[Document], models: _*)(real)

}

class KLModel(trainSet: List[Document], models: Model*) extends Model {
  // val sFilter = StopWordFilter("en")

  val klScores = trainSet.map { doc => (doc.id, apply(doc), doc.real) }
  
  val trifourgramScores = trainSet.map { doc => (doc.id, (models(0)(doc),models(1)(doc)) , doc.real) }
  
  val realKlScores = trifourgramScores.filter{_._3}
  val fakeKlScores = trifourgramScores.filter{!_._3}
  println("real:")  
  realKlScores.foreach{ case (id,(triscore,fourscore),_)=>
    
    println(triscore +  "\t" + fourscore )
  }
  
  println("fake:")
  fakeKlScores.foreach{ case (id,(triscore,fourscore),_)=>
    
    println(triscore  + "\t" + fourscore )
  }
 
  /*val sortedKl = klScores.sortBy(_._2)
  
  sortedKl.foreach { doc =>
    print("kl: (" + doc._1 + ")" + doc._2 + " real? " + doc._3 + "\n")
  }

  println("separation quality")
  println("#real (above median): " + sortedKl.take(sortedKl.length / 2).count(_._3))
  println("#real (below median): " + sortedKl.drop(sortedKl.length / 2).count(_._3))
*/
  def apply(doc: Document): Double = {
    models.map(model => model(doc)).reduce((x, y) => {if(y > x) 1d else 0d  })
  }

  override def KL(doc: Document): Double = ???
  def sigmoid(t: Double) = 1d / (1d + math.exp(-t))
} 

object KLModel {
  def apply(trainSet: List[Document], models: Model*) = new KLModel(trainSet: List[Document], models: _*)

}

