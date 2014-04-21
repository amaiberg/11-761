package project
import scala.io.Source._
import java.io.File
import scala.collection.SortedMap
object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(171); 
  val path = "/home/mog/dev/11-761/project/workspace/project/";System.out.println("""path  : String = """ + $show(path ));$skip(250); 
  //val File = new File(path).getAbsolutePath
  //val trainingSet = fromFile(path + "trainingSet.dat").mkString.split("~~~~~").tail
  /*
  val trainingLabels = fromFile(path + "trainingSetLabels.dat").mkString.split("\n")
      */
  val size = 10000;System.out.println("""size  : Int = """ + $show(size ));$skip(85); 
  lazy val corpus = fromFile(path + "LM-train-100MW.txt").getLines.take(size).toList;System.out.println("""corpus: => List[String]""");$skip(32); 
  //println(corpus)
  val n = 3;System.out.println("""n  : Int = """ + $show(n ));$skip(1009); 
  // val model = ngrams.map(ngram=> ngram -> ngrams.count(ngram.toList ==_.toList)).toMap.values.toSet

  def gt(counts: Map[List[String], Int]): Map[List[String], Double] = {
    val counted: SortedMap[Int, Set[List[String]]] =
      SortedMap.empty[Int, Set[List[String]]] ++
        counts.groupBy { case (k, v) => v }.mapValues { _.keySet }

    //the katz constant
    val k = 5
    println(counted.keys)
    val N = counts.size.toDouble
    def gtKatzSmooth(c: Int): Double = {
      val x = (c + 1) * counted(c + 1).size.toDouble / counted(c).size.toDouble
      val y = c * (k + 1) * counted(k + 1).size.toDouble / counted(1).size.toDouble
      (x - y) / (1d - y)
    }
    println(counted.keys.take(5))
    val smoothedNgram =
      //MLE for i>K
      counted.drop(k).flatMap { case (_, v) => v.map(gram=> gram -> v.size / N )}++
      counted.keys.toList.take(k).flatMap { case i =>
       counted(i).map(gram=>gram -> gtKatzSmooth(i)) } // Katz-smoothing for the rest
    smoothedNgram.toMap
  };System.out.println("""gt: (counts: Map[List[String],Int])Map[List[String],Double]""");$skip(612); 

  def genNGrams(corpus: List[String], n: Int) = {
    val ngrams = corpus.flatMap(sent => sent.split(" ").sliding(n)).map(_.toList)
    //maybe use ephemeral stream...
    def genModel(corpus: List[List[String]], model: Map[List[String], Int]): Map[List[String], Int] = corpus match {
      case Nil => model
      case head :: tail =>
        val newModel = if (model.contains(head)) model ++ Map(head -> (model(head) + 1)) else model ++ Map(head -> 1)
        genModel(tail, newModel)
    }

    //discard singletons
    val counts = genModel(ngrams, Map())
    gt(counts).filterNot{case (k,v) => v == 1}
  };System.out.println("""genNGrams: (corpus: List[String], n: Int)scala.collection.immutable.Map[List[String],Double]""");$skip(24); val res$0 = 

  genNGrams(corpus, 3);System.out.println("""res0: scala.collection.immutable.Map[List[String],Double] = """ + $show(res$0))}

  /*
  val trainingDocs = trainingSet.zipWithIndex.map {
    case (doc, id) =>
      val real = trainingLabels(id).toInt == 1
      val sents = doc.split("\n").map(s => Sentence(s.split(" ")))
      Document(id, sents, real)
  }
  */
  case class Document(id: Int, sents: Array[Sentence], real: Boolean)
  case class Sentence(words: Array[String])

}
