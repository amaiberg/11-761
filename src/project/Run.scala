package project

import Parse._
import Ngram._
object Run {

  def main(args: Array[String]) {
    val devDocs = getDevDocs
    val trainDocs = getTrainDocs
    lazy val corpus = getCorpus
    //val model = trainModel(trainDocs, corpus)
    val model = NGramModel(corpus, trainDocs)
    val (hard, soft) = evaluate(classify(model, devDocs), devDocs)
    /*for my use*/
    printf("hard measure: %1.2f%%  correct\n", hard * 100)
    println("soft measure (posterior avg-ll): " + soft)
  }

  def trainModel(trainDocs: List[Document], corpus: Iterator[String]): Model = ???

  def classify(model: Model, testDocs: List[Document]): List[Document] = testDocs.map {
    case doc @ Document(id, sents, real, soft) =>
      val softLabel = model(doc)
      Document(id, sents, softLabel > .5, softLabel)
  }

  def trainDummy(docs: List[Document], corpus: List[String]) = dummyModel

  def evaluate(modelDocs: List[Document], gsDocs: List[Document]): Metric = {
    modelDocs.foreach { d =>
      printf("%f %f %d\n", d.softLabel, 1d - d.softLabel, if (d.real) 1 else 0)
    }
    val testedDocs = test(modelDocs, gsDocs)
    val n = modelDocs.size
    (hardMeasure(testedDocs, n), softMeasure(testedDocs, n))
  }

  def hardMeasure(testedDocs: List[Document], total: Int): Double =
    testedDocs.filter(d => d.real).size.toDouble / testedDocs.size.toDouble

  def softMeasure(testedDocs: List[Document], total: Int): Double =
    testedDocs.filter(d => d.real).map(d => math.log(d.softLabel)).sum / testedDocs.size.toDouble

  def test(modelDocs: List[Document], gsDocs: List[Document]): List[Document] = (modelDocs.zip(gsDocs).filter {
    case (d1, d2) => d1.real == d2.real
  }).map(_._1)

  object dummyModel extends Model {
    def apply(doc: Document) = math.random
  }

  object NGramModel {
    def apply(corpus: List[String], trainSet: List[Document]) = new NGramModel(corpus, trainSet)
  }

  class NGramModel(corpus: List[String], trainSet: List[Document]) extends Model {

    val trigramModel = genNGrams(corpus, 3)

    //need to train t
    val t = 2 // what should this value be??

    println("training...")
    val klScores = trainSet.map { doc =>
      {
        val ngrams = doc.sents.filterNot(_.isEmpty).flatMap(sent => sent.sliding(3)).map(_.toList)
        (doc.id, -kl(trigramModel, ngrams.filter(trigramModel.contains(_))), doc.real)
        //  print("kl: (" + doc.id + ")" + kl(model, ngrams.filter(model.contains(_))) + " real? " + doc.real + "\n")
      }
    }

    val sortedKl = klScores.sortBy(_._2)

    sortedKl.foreach { doc =>
      print("kl: (" + doc._1 + ")" + doc._2 + " real? " + doc._3 + "\n")
    }

    println("separation quality")
    println("#real (above median): " + sortedKl.take(sortedKl.length / 2).count(_._3))
    println("#real (below median): " + sortedKl.drop(sortedKl.length / 2).count(_._3))

    def apply(doc: Document) = {
      val dist = -kl(trigramModel, doc.sents.filterNot(_.isEmpty))
      //logistic regression
      if (dist > t) 1 else 0
    }

  }
  def kl[E](eModel: E => Double, events: List[E]): Double =
    events.map(e => 1d / lg(eModel(e))).sum / events.size.toDouble

  def entropy(probs: Array[Double]) = probs.reduce((x, y) => x + y * lg(1d / y))
  def lg(x: Double) = math.log(x) / math.log(2d)

  trait TestModel {
    
    def run(doc:Document ) : Double  
    
  }
  type Model = Document => Double
  type Metric = (Double, Double)
}