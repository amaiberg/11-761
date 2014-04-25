package project
import edu.berkeley.nlp.lm.io.LmReaders;
import java.io.File
import edu.berkeley.nlp.lm.io.MakeLmBinaryFromArpa
import edu.berkeley.nlp.lm.io.MakeKneserNeyArpaFromText
import edu.berkeley.nlp.lm.ArrayEncodedProbBackoffLm
import scala.collection.JavaConversions._
import Parse._
 
object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(340); 
  val trainDocs = getTrainDocs;System.out.println("""trainDocs  : List[project.Parse.Document] = """ + $show(trainDocs ));$skip(659); 
  /*val f ="dev/11-761/project/workspace/project/LM-kn3-train-100MW.binlm"
  //MakeLmBinaryFromArpa.main(Array("/home/mog/dev/11-761/project/workspace/project/LM-kn3-train-100MW.arpa","LM-train-100MW.binlm"));
   val trigramLM = LmReaders.readLmBinary(f).asInstanceOf[ArrayEncodedProbBackoffLm[String]]
 
 val javaList : java.util.List[String] = List[String]("IN","DINOSAUR","MICROPHONE")
 
 trigramLM.getLogProb(javaList)
    *///  MakeKneserNeyArpaFromText.main(Array("3","/home/mog/dev/11-761/project/workspace/project/LM-kn-train-100MW.arpa","/home/mog/dev/11-761/project/workspace/project/LM-train-100MW.txt"))
	  val trigrams = NGramModel(trainDocs, 3);System.out.println("""trigrams  : project.NGramModel = """ + $show(trigrams ));$skip(45); 
    val fourgrams = NGramModel(trainDocs, 4);System.out.println("""fourgrams  : project.NGramModel = """ + $show(fourgrams ))}
}
