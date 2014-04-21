package project

import scala.io.Source._
import java.io.File

object Parse {
  //currently relative to workspace, but needs to be cmd-line deployable...
  val path = ""

  val n = 10000
  lazy val corpus = fromFile(path + "LM-train-100MW.txt").getLines.take(n).toList

  def getDocs(trainPath:String) = {
    val trainingSet = fromFile(path + trainPath + ".dat").mkString.split("~~~~~").tail
    val trainingLabels = fromFile(path + trainPath+"Labels.dat").mkString.split("\n")
    label(trainingSet.toList, trainingLabels.toList)
  }
  
  def getTrainDocs = getDocs("trainingSet")

  def getDevDocs = getDocs("developmentSet")

  def getCorpus = corpus

  case class Document(id: Int, sents: List[List[String]], real: Boolean,softLabel:Double=0d)
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