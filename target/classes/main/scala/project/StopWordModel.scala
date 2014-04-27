package project
import Parse._
import chalk.text.transform.StopWordFilter
object StopWordModel extends Model {

  override def apply(doc: Document) = {
    val sFilter = StopWordFilter()
    val sFreq = doc.sents.filterNot(_.isEmpty).map(s => s.count(w => !sFilter(w)) / s.length.toDouble).sum / doc.sents.length.toDouble
   // println(doc.sents)
    1d-sFreq 
  }

}