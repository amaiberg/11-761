package project

import scala.io.Source._
import java.io.File
import chalk.text.transform.StopWordFilter

import scala.collection.JavaConversions._

object Parse {
  //currently relative to workspace, but needs to be cmd-line deployable...
  val path = "conf/"

  val n = 100000
  val sFilter = StopWordFilter("en")
  lazy val corpus = sFilter.apply(fromFile("LM-train-100MW.txt").getLines.toIterable).take(n).mkString.split("~~~~~")

  def getDocs(trainPath: String, fromStream: Boolean = false) = {
    val trainingSet =
      fromStream match {
        case false => fromFile(path + trainPath + ".dat").mkString.split("~~~~~").tail
        case true => fromInputStream(getClass.getResourceAsStream("/" + trainPath + ".dat")).mkString.split("~~~~~").tail
      }

    val trainingLabels = fromFile(path + trainPath + "Labels.dat").mkString.split("\n")
    label(trainingSet.toList, trainingLabels.toList)
  }

  def getTestDocs(testPath: String, debug: Boolean = false): List[Document] = {
    val trainingSet = stdin.getLines.mkString.split("~~~~~").tail.toList
    // val st = stdin.getLines.reduce(_ + _) //fromFile(path + testPath).mkString.split("~~~~~").tail
    if (debug) {
      val trainingLabels = fromFile(path + "developmentSet" + "Labels.dat").mkString.split("\n")
      label(trainingSet.toList, trainingLabels.toList, debug)
    } else {
      trainingSet.zipWithIndex.map {
        case (doc, id) =>
          val sents = doc.split("\n").map(s => if (s.split(" ").toList.isEmpty) s.split(" ").toList else s.split(" ").toList.tail)
          Document(0, sents.toList, false)
      }
    }
  }

  def getTrainDocs(fromStream: Boolean = false) = getDocs("trainingSet", fromStream)

  def getDevDocs(fromStream: Boolean = false) = getDocs("developmentSet")

  def getCorpus = corpus.tail.map(s => new Document(0, s.split("\n").map(w => w.split(" ").toList).toList, false))

  case class Document(id: Int, sents: List[List[String]], real: Boolean, softLabel: Double = 0d) {
    val jSents: java.util.List[java.util.List[String]] = sents.map(l => asJavaList(l))
    def getSentences() = jSents

  }

  //case class Sentence(words: Array[String])

  def label(trainSet: List[String], labels: List[String], debug: Boolean = false): List[Document] = {
    trainSet.zipWithIndex.map {
      case (doc, id) =>
        val real = labels(id).toInt == 1
        val sents = doc.split("\n").map(s => if (s.split(" ").toList.isEmpty || !debug) s.split(" ").toList else s.split(" ").toList.tail)
        Document(id, sents.toList, real)
    }
  }
} 