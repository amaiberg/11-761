package project

import scala.io.Source._
import java.io.File
import chalk.text.transform.StopWordFilter

import scala.collection.JavaConversions._

object Parse {
  //currently relative to workspace, but needs to be cmd-line deployable...
  val path = ""

  val n = 100000
  val sFilter = StopWordFilter()
  lazy val corpus = sFilter.apply(fromFile(path + "LM-train-100MW.txt").getLines.toIterable).par

  def getDocs(trainPath: String, fromStream: Boolean = false) = {
    val trainingSet =
      fromStream match {
        case false => fromFile(path + trainPath + ".dat").mkString.split("~~~~~").tail
        case true => fromInputStream(getClass.getResourceAsStream(trainPath)).mkString.split("~~~~~").tail
      }

    val trainingLabels = fromFile(path + trainPath + "Labels.dat").mkString.split("\n")
    label(trainingSet.toList, trainingLabels.toList)
  }

  def getTestDocs(testPath: String): List[Document] = {
    val trainingSet = stdin.getLines.toList.mkString.split("~~~~~").tail
    // val st = stdin.getLines.reduce(_ + _) //fromFile(path + testPath).mkString.split("~~~~~").tail
    val testDocs = trainingSet.map { doc => Document(0, doc.split("\n").map(s => s.split(" ").toList).toList, true) }
    testDocs.toList
  }

  def getTrainDocs(fromStream:Boolean = false) = getDocs("trainingSet",fromStream)

  def getDevDocs = getDocs("developmentSet")

  def getCorpus = corpus

  case class Document(id: Int, sents: List[List[String]], real: Boolean, softLabel: Double = 0d) {

    val jSents: java.util.List[java.util.List[String]] = sents.map(l => asJavaList(l))
    def getSentences() = jSents

  }

  //case class Sentence(words: Array[String])

  def label(trainSet: List[String], labels: List[String]): List[Document] = {
    trainSet.zipWithIndex.map {
      case (doc, id) =>
        val real = labels(id).toInt == 1
        val sents = doc.split("\n").map(s => s.split(" ").toList)
        Document(id, sents.toList, real)
    }
  }
} 