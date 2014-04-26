package project

import Run._
import Parse._
import nak.data.DataMatrix
import nak.classify.LogisticClassifier
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import nak.data.Example

import LogisticModel._
import breeze.optimize.FirstOrderMinimizer.OptParams

object LogisticModel {
  def apply(trainSet: List[Document], triModel: Model, fourModel: Model) = new LogisticModel(trainSet: List[Document], triModel: Model, fourModel: Model)
  def getDataMatrix(realCols: List[(Double, Double)], fakeCols: List[(Double, Double)]): DataMatrix = {
    val real = realCols.zipWithIndex.map { case (c, i) => Example[Double, Seq[Double]](label = 1, features = Seq(c._1, c._2), id = i.toString) }
    val fake = fakeCols.zipWithIndex.map { case (c, i) => Example[Double, Seq[Double]](label = 0, features = Seq(c._1, c._2), id = i.toString) }
    val dMatrix = new DataMatrix {
      val rows = (real ++ fake).toSeq
    }
    dMatrix
  }
}
class LogisticModel(trainSet: List[Document], triModel: Model, fourModel: Model) extends Model(trainSet) {
  //feed data
  val realDocs = trainSet.filter(_.real)
  val fakeDocs = trainSet.filter(!_.real)
  val realCols = realDocs.map(d => (triModel(d), fourModel(d)))
  val fakeCols = fakeDocs.map(d => (triModel(d), fourModel(d)))
  val realSeq = realCols
  val fakeSeq = fakeCols
  val dataMatrix = getDataMatrix(realSeq, fakeSeq)

  //Train
  val vectors = dataMatrix.rows.map(e => e map ((a: Seq[Double]) => new DenseVector(a.toArray)) relabel (l => l))
  val opt = OptParams(512, 0.0, 0.5, 1000, false, 1E-5, false)

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
    sigmoid(classifier.scores(new DenseVector(Array(triModel(doc), fourModel(doc))))(1))
    //classifier.classify(new DenseVector(Array(triModel(doc), fourModel(doc))))
  }

} 