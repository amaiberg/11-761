package project

import Parse._
import BerkeleyLM._
import scala.collection.parallel.ParIterable
import LogisticModel._
object Run {
  val debug = false
  def main(args: Array[String]) {
    //   val devDocs = getDevDocs
    val trainDocs = getTrainDocs(true)
    val testDocs = getTestDocs(args(0))
    lazy val corpus = getCorpus
    //val model = trainModel(trainDocs, corpus)
    // val bigrams = NGramModel(trainDocs, 2)
    val trigrams = NGramModel(trainDocs, 3)
    val fourgrams = NGramModel(trainDocs, 4)
    //val fivegrams = NGramModel(trainDocs, 5)
    // val realModel = InterpolatedModel(trainDocs, bigrams, trigrams, fourgrams, fivegrams)(true)
    // val fakeModel = InterpolatedModel(trainDocs, bigrams, trigrams, fourgrams, fivegrams)(false)
    val model = LogisticModel(trainDocs, trigrams, fourgrams) //KLModel(trainDocs, trigrams, fourgrams)
    //classify(model, devDocs)
    //val (hard, soft) = evaluate(classify(model, devDocs), devDocs)
    val (hard, soft) = evaluate(classify(model, testDocs), testDocs)
    /*for my use*/
    // printf("hard measure: %1.2f%%  correct\n", hard * 100)
    //  println("soft measure (posterior avg-ll): " + soft)
  }

  def trainModel(trainDocs: List[Document], corpus: Iterable[String]): Model = ???

  def classify(model: Model, testDocs: List[Document]): List[Document] = testDocs.map {
    case doc @ Document(id, sents, real, soft) =>
      val softLabel = model(doc)
      Document(id, sents, softLabel > .5, softLabel)
  }

  def trainDummy(docs: List[Document], corpus: List[String]) = dummyModel

  def evaluate(modelDocs: List[Document], gsDocs: List[Document]): Metric = {
    modelDocs.zip(gsDocs).foreach {
      case (d, gsDoc) =>
        if (debug)
          printf("%f %f %d %d\n", 1d - d.softLabel, d.softLabel, if (d.real) 1 else 0, if (gsDoc.real) 1 else 0)
        else
          printf("%f %f %d\n", 1d - d.softLabel, d.softLabel, if (d.real) 1 else 0)
    }
    val testedDocs = test(modelDocs, gsDocs)
    val n = modelDocs.size
    (hardMeasure(testedDocs, n), softMeasure(testedDocs, n))
  }

  def hardMeasure(testedDocs: List[Document], total: Double): Double =
    testedDocs.size.toDouble / total.toDouble

  def softMeasure(testedDocs: List[Document], total: Double): Double =
    testedDocs.map(d => math.log(d.softLabel)).sum / total.toDouble

  def test(modelDocs: List[Document], gsDocs: List[Document]): List[Document] = (modelDocs.zip(gsDocs).filter {
    case (d1, d2) => d1.real == d2.real
  }).map(_._1)

  object dummyModel extends Model(null, null) {
    def apply(doc: Document) = math.random
    override def KL(doc: Document) = 0d
  }

  def kl[E](eModel: E => Double, events: List[E]): Double =
    events.map(e => 1d / lg(eModel(e))).sum / events.size.toDouble

  def entropy(probs: Array[Double]) = probs.reduce((x, y) => x + y * lg(1d / y))
  def lg(x: Double) = math.log(x) / math.log(2d)

  abstract class Model {
    def this(corpus: ParIterable[String], trainingSet: List[Document]) = this
    def this(trainingSet: List[Document]) = this
    def apply(doc: Document): Double
    def KL(doc: Document): Double = ???
  }
  type SModel[E] = E => Double

  type Metric = (Double, Double)

}