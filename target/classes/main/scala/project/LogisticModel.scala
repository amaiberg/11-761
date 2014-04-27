package project

import Parse._
import nak.data.DataMatrix
import nak.classify.LogisticClassifier
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import nak.data.Example
import Model._
import LogisticModel._
import breeze.optimize.FirstOrderMinimizer.OptParams

object LogisticModel {
  def apply(trainSet: List[Document], models: Model*) = new LogisticModel(trainSet: List[Document], models: _*)
  def getDataMatrix(realCols: List[Seq[Double]], fakeCols: List[Seq[Double]]): DataMatrix = {
    val real = realCols.zipWithIndex.map { case (c, i) => Example[Double, Seq[Double]](label = 1, features = c, id = i.toString) }
    val fake = fakeCols.zipWithIndex.map { case (c, i) => Example[Double, Seq[Double]](label = 0, features = c, id = i.toString) }
    val dMatrix = new DataMatrix {
      val rows = (real ++ fake).toSeq
    } 
    dMatrix
  }
}

class LogisticModel(trainSet: List[Document], models: Model*) extends Model(trainSet) {
  //feed data
  val realDocs = trainSet.filter(_.real)
  val fakeDocs = trainSet.filter(!_.real)
  val realCols = realDocs.map(d => models.map(m => m(d)))
  val fakeCols = fakeDocs.map(d => models.map(m => m(d)))
  val realSeq = realCols
  val fakeSeq = fakeCols
  val dataMatrix = getDataMatrix(realSeq, fakeSeq)

  //Train
  val vectors = dataMatrix.rows.map(e => e map ((a: Seq[Double]) => new DenseVector(a.toArray)) relabel (l => l))
  val opt = OptParams(512, 0.0, 0.5, 1000, false, 1E-7, false)
  val classifier = new LogisticClassifier.Trainer[Double, DenseVector[Double]](opt).train(vectors)
  /* 
  for (ex <- vectors) {
    //println(ex.features)
    val guessed = classifier.classify(ex.features)
    //println(guessed, ex.label)
  }*/
  def sigmoid(t: Double) = 1d / (1d + math.exp(-t))
  //classify
  /*
   * val opt = OptParams(batchSize:Int = 512,
                       regularization: Double = 0.0,
                       alpha: Double = 0.5,
                       maxIterations:Int = 1000,
                       useL1: Boolean = false,
                       tolerance:Double = 1E-5,
                       useStochastic: Boolean= false)
  
   */

  override def apply(doc: Document): Double = {
    sigmoid(classifier.scores(new DenseVector(models.map(m => m(doc)).toArray))(1))
    //classifier.classify(new DenseVector(Array(triModel(doc), fourModel(doc))))
  }

} 