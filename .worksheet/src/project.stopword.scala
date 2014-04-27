package project
import chalk.text.transform.StopWordFilter
import project._
import Parse._
object stopword {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(145); 
  val sFilter = new StopWordFilter();System.out.println("""sFilter  : chalk.text.transform.StopWordFilter = """ + $show(sFilter ));$skip(21); val res$0 = 
  sFilter("monkeys");System.out.println("""res0: Boolean = """ + $show(res$0));$skip(128); val res$1 = 
  StopWordModel( Document(1,List("blah blah and why? I don't know".split(" ").toList, "Is this real".split(" ").toList),false));System.out.println("""res1: Double = """ + $show(res$1))}
}
