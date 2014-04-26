object regress {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(499); 
def main(args: Array[String]) {
    val data = DataMatrix.fromURL(new java.net.URL("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/spam.data"),-1)
    val vectors = data.rows.map(e => e map ((a:Seq[Double]) => new DenseVector(a.toArray)) relabel (_.toInt))

    val classifier = new LogisticClassifier.Trainer[Int,DenseVector[Double]].train(vectors)
    for( ex <- vectors) {
      val guessed = classifier.classify(ex.features)
      println(guessed,ex.label)
    }
  }};System.out.println("""main: (args: Array[String])Unit""")}
