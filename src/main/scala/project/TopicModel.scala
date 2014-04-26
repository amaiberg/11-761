package project
import Parse._
import breeze.linalg.VectorBuilder
import chalk.text.tokenize.JavaWordTokenizer
import breeze.util.Index
import chalk.topics.LDA
import chalk.topics.LDA.Params
import java.io.File
import breeze.linalg._
import breeze.numerics._
import breeze.config.CommandLineParser
import breeze.util.Index
import java.io.File
import chalk.text.tokenize.JavaWordTokenizer
import chalk.text.transform.StopWordFilter
import scala.io._
import breeze.util.Implicits._
object TopicModel {
  val sFilter = StopWordFilter()
  def main(args: Array[String]) = {
    val trainDocs = getTrainDocs(true).filter(_.real)
    val fmap = Index[String]()
    // Read in the training data and index it.
    val params = Params(new File(""), numTopics = 20,
      topicSmoothing = .1,
      wordSmoothing = 0.1)

    val almostTrainingData = for {
      doc <- trainDocs
    } yield {
      //remove stopwords
      val text = doc.sents.map(sFilter(_).filterNot(s=>s.length < 3))
      val builder = new VectorBuilder[Double](Int.MaxValue, text.length / 20)
      for (tok <- JavaWordTokenizer(text.flatten.reduce(_ + " " + _)) if tok(0).isLetter) {
        builder.add(fmap.index(tok), 1.0)
      }

      builder
    }

    val trainingData = almostTrainingData.toIndexedSeq.map { b => b.length = fmap.size; b.toSparseVector }

    val lda = new LDA(params.numTopics, params.topicSmoothing, params.wordSmoothing)
    val model = lda.iterations(trainingData).tee(m => println(m.likelihood)).last
    val topKLists = for (k <- 0 until 20) yield model.termWeights.t(::, k).argtopk(50).map(i => fmap.get(i) + " " + model.termWeights(k, i))
    for ((list, k) <- topKLists.zipWithIndex) {
      println("Topic %d:".format(k))
      println(list.mkString("\t", "\n\t", "\n"))
    }
  }
}