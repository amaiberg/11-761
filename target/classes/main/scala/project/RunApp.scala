package project

import project.Parse._
import project.BerkeleyLM._
import scala.collection.parallel.ParIterable
import LogisticModel._
import Model._
import java.io.File
import java.io.FileInputStream
object RunApp extends App {
  override def main(args: Array[String]) {
    implicit val debug = if (!args.isEmpty) {
      System.setIn(new FileInputStream(args(0)));
      true
    } else false
    val devDocs = getDevDocs(false)
    val trainDocs = getTrainDocs(false)
    val testDocs = getTestDocs("", debug)
    lazy val corpus = getCorpus
    //val model = trainModel(trainDocs, corpus)
    //N-gram models
  //  val bigrams = NGramModel(trainDocs, 2)
    val trigrams = NGramModel(trainDocs, 3)
    val fourgrams = NGramModel(trainDocs, 4)
//    val fivegrams = NGramModel(trainDocs, 5)
    //stopword model
    val sModel = StopWordModel
    //val tModel = TopicModel(corpus.toList, trainDocs)
    //printScores(testDocs, sModel, trigrams, fourgrams)(false)
    //val fivegrams = NGramModel(trainDocs, 5)
    // val realModel = InterpolatedModel(trainDocs, bigrams, trigrams, fourgrams, fivegrams)(true)
    // val fakeModel = InterpolatedModel(trainDocs, bigrams, trigrams, fourgrams, fivegrams)(false)

    val model = LogisticModel(trainDocs,sModel, trigrams, fourgrams) //KLModel(trainDocs, trigrams, fourgrams)
    //classify(model, devDocs) 
    val (hard, soft) = evaluate(classify(model, testDocs), testDocs)(false)
    //println(devDocs)
    //println(testDocs)
    /*for my use*/
/*
    if (debug) {
      //  val (hard, soft) = evaluate(classify(model, testDocs), testDocs)
      printf("hard measure: %1.2f%%  correct\n", hard * 100)
      println("soft measure (posterior avg-ll): " + soft)
    }*/
  }

  def printScores(docs: List[Document], models: Model*)(implicit andDocs: Boolean = false) = {
    val realDocs = docs.filter(_.real)
    val fakeDocs = docs.filter(!_.real)
    println("real:")
    val printScores = (d: Document) => printf(models.fold("")((x, y) => x + "%f\t") + { if (andDocs) d.sents.toString else "" } + "\n", models.map(m => m(d)): _*)
    realDocs.foreach { printScores(_) }
    println("fake:")
    fakeDocs.foreach { printScores(_) }
  }

  def classify(model: Model, testDocs: List[Document]): List[Document] = testDocs.map {
    case doc @ Document(id, sents, real, soft) =>
      val softLabel = model(doc)
      Document(id, sents, softLabel > .5, softLabel)
  }

  def trainDummy(docs: List[Document], corpus: List[String]) = dummyModel

  def evaluate(modelDocs: List[Document], gsDocs: List[Document], andDocs: Boolean = false)(implicit debug: Boolean = false): Metric = {
    modelDocs.zip(gsDocs).foreach {
      case (d, gsDoc) =>
        if (debug)
          printf("%f %f %d %d doc:%s\n", 1d - d.softLabel, d.softLabel, if (d.real) 1 else 0, if (gsDoc.real) 1 else 0, if (gsDoc.real != d.real) gsDoc.sents.toString else "")
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
    testedDocs.map(d => if (d.real) math.log(d.softLabel) / total.toDouble else math.log(1d - d.softLabel) / total.toDouble).sum

  def test(modelDocs: List[Document], gsDocs: List[Document]): List[Document] = (modelDocs.zip(gsDocs).filter {
    case (d1, d2) => d1.real == d2.real
  }).map(_._1)

  object dummyModel extends Model(null, null) {
    def apply(doc: Document) = math.random
    override def KL(doc: Document) = 0d
  }

  type SModel[E] = E => Double

  type Metric = (Double, Double)

}