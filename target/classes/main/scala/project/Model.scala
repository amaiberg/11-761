package project
import Parse._
import scala.collection.parallel.immutable.ParIterable
object Model {
  def kl[E](eModel: E => Double, events: List[E]): Double =
    events.map(e => 1d / lg(eModel(e))).sum / events.size.toDouble

  def entropy(probs: Array[Double]) = probs.reduce((x, y) => x + y * lg(1d / y))
  def lg(x: Double) = math.log(x) / math.log(2d)

}

abstract class Model {
  def this(corpus: ParIterable[String], trainingSet: List[Document]) = this
  def this(trainingSet: List[Document]) = this
  def apply(doc: Document): Double
  def KL(doc: Document): Double = ???
}
 
