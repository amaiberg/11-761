package project

import Parse._
import Ngram._
import Ngram._
import NGramModel._
import scala.collection.parallel.ParIterable
object Run {
  val debug = true
  def main(args: Array[String]) {
    val devDocs = getDevDocs
    val trainDocs = getTrainDocs
    lazy val corpus = getCorpus
    //val model = trainModel(trainDocs, corpus)
    val trigrams = NGramModel(corpus, trainDocs, 3, 100000)
    val fourgrams = NGramModel(corpus, trainDocs, 4, 100000)
    val model = InterpolatedGram(corpus, trainDocs, trigrams, fourgrams)
    val (hard, soft) = evaluate(classify(model, devDocs), devDocs)
    /*for my use*/
    printf("hard measure: %1.2f%%  correct\n", hard * 100)
    println("soft measure (posterior avg-ll): " + soft)
  }

  def trainModel(trainDocs: List[Document], corpus: Iterable[String]): Model = ???

  def classify(model: Model, testDocs: List[Document]): List[Document] = testDocs.map {
    case doc @ Document(id, sents, real, soft) =>
      val softLabel = model(doc)
      Document(id, sents, softLabel > .5, softLabel)
  }

  def trainDummy(docs: List[Document], corpus: List[String]) = dummyModel

  def evaluate(modelDocs: List[Document], gsDocs: List[Document]): Metric = {
    modelDocs.zip(gsDocs).foreach {case (d,gsDoc) =>
      if (debug)
        printf("%f %f %d %d\n", d.softLabel, 1d - d.softLabel, if (d.real) 1 else 0, if(gsDoc.real) 1 else 0)
      else
        printf("%f %f %d\n", d.softLabel, 1d - d.softLabel, if (d.real) 1 else 0)
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
  }

  def kl[E](eModel: E => Double, events: List[E]): Double =
    events.map(e => 1d / lg(eModel(e))).sum / events.size.toDouble

  def entropy(probs: Array[Double]) = probs.reduce((x, y) => x + y * lg(1d / y))
  def lg(x: Double) = math.log(x) / math.log(2d)

  abstract class Model {
    def this(corpus: ParIterable[String], trainingSet: List[Document]) = this
    def apply(doc: Document): Double
  }

  type Metric = (Double, Double)

}