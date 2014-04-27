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
import breeze.stats.DescriptiveStats.{ variance => dvar }
import java.io.File
import chalk.text.tokenize.JavaWordTokenizer
import chalk.text.transform.StopWordFilter
import scala.io._
import breeze.util.Implicits._
object TopicModel {
  def apply(corpus: List[Document], trainSet: List[Document]) = new TopicModel(corpus, trainSet)
  //def similarity(h:List[String],top:List[(String,Double)]) = ???
  //def p(w:String,top:List[(String,Double)]) = if(top.contains(w)) top.find(_._1 == w).get._2 else 0.1
  //def cos(l1:List[String],l2:List[String]) = l1.zip(l2).map{case (a,b)  => }
  def sim(s: List[String], topic: List[(String, Double)]) = s.map(w => if (topic.contains(w)) math.log(topic.find(_._1 == w).get._2) else math.log(0.000000001)).reduce(_ + _)
}

class TopicModel(corpus: List[Document], trainSet: List[Document]) extends Model {
  val sFilter = StopWordFilter()
  val trainDocs = getTrainDocs(true).filter(_.real)
  val fmap = Index[String]()
  // Read in the training data and index it.
  val params = Params(new File(""), numTopics = 30,
    topicSmoothing = .1,
    wordSmoothing = 0.1)

  val almostTrainingData = for {
    doc <- trainDocs ++ corpus
  } yield {
    //remove stopwords
    val text = doc.sents.map(s=>sFilter(s).filterNot(w => w.length < 3))
    val builder = new VectorBuilder[Double](Int.MaxValue, text.length / 20)
    for (tok <- JavaWordTokenizer(text.flatten.reduce(_ + " " + _)) if tok(0).isLetter) {
      builder.add(fmap.index(tok), 1.0)
    }

    builder
  }

  val trainingData = almostTrainingData.toIndexedSeq.map { b => b.length = fmap.size; b.toSparseVector }

  val lda = new LDA(params.numTopics, params.topicSmoothing, params.wordSmoothing)
  val model = lda.iterations(trainingData).tee(m => println(m.likelihood)).last

  
  val topics = for (k <- 0 until 30) yield model.termWeights.t(::, k).argtopk(50).map(i => (fmap.get(i), model.termWeights(k, i)))
  for ((list, k) <- topics.zipWithIndex) {
    println("Topic %d:".format(k))
    println(list.mkString("\t", "\n\t", "\n"))
  }
  import TopicModel._
  override def apply(doc: Document): Double = {
    // val wordsWithHistory = doc.sents.flatMap(s=> s.scanLeft(("",List("")))((h,w)=>(w,List(h._1) ++ h._2))).tail
    val words = doc.sents.map(sFilter(_))
    val topProbs = topics.map(top => words.map(w => top.count(_._1 == w) / words.size.toDouble).sum)
    dvar(topProbs)
  }

}