package grammar;

import project.Parse.Document;
import project.Run.*;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Arrays;

import edu.stanford.nlp.parser.lexparser.LexicalizedParser;
import edu.stanford.nlp.trees.Tree;
import grammar.inputParse;
import java.util.List;
  
public class grammarScore extends Model {
  public static void main(String[] args) throws IOException, ClassNotFoundException{
     String fdata = "parseScoreSet.txt";
     String finput = "trainingSet.dat";
     
     //ArrayList<ArrayList<Double>> pss = grammarScore.getParseScoreSet(finput);
     ArrayList<ArrayList<Double>> pss = grammarScore.getParseScoreSetFromFile(fdata);
     System.out.println(pss.size());

  } 
  
  public static ArrayList<ArrayList<Double>> getParseScoreSetFromFile(String filename) 
    throws IOException, ClassNotFoundException {
    ObjectInputStream ois =
            new ObjectInputStream(new FileInputStream(filename));
    ArrayList<ArrayList<Double>> parseScoreSet = (ArrayList<ArrayList<Double>>) ois.readObject();
    return parseScoreSet;
  }
  
  public static  ArrayList<ArrayList<Double>> getParseScoreSet(String filename) throws IOException {
     ArrayList<ArrayList<String>> articleSet = inputParse.getParsedInput(filename);
     LexicalizedParser lp = LexicalizedParser.loadModel(
             "englishPCFG.ser.gz");
     /* example 
     String sent = "I have an apple ";
     Tree parse = lp.parse(sent);
     double score = parse.score();
     System.out.println(score); 
     */
     
     
     
     ArrayList<String> article;
     ArrayList<Double> parseScore;
     double score;
     ArrayList<ArrayList<Double>> parseScoreSet = new ArrayList<ArrayList<Double>>();  
     System.out.println(articleSet.size()); 
     for (int i = 0; i < articleSet.size(); i++) {
       article = articleSet.get(i);
       System.out.println("Article size: " + article.size() + "id = " + i); 
       
       parseScore = new ArrayList<Double>();
       for (int j = 0; j < article.size(); j++) {
         score = grammarScore.getParseScore(article.get(j), lp);
         parseScore.add(score);
       }
       parseScoreSet.add(parseScore);
     }
     System.out.println(parseScoreSet.size());      
     //prepare output score to txt file
     FileOutputStream scoreOut = new FileOutputStream("parseScoreSet.txt");
     ObjectOutputStream scoreOutObj = new ObjectOutputStream(scoreOut);
     scoreOutObj.writeObject(parseScoreSet);
     scoreOutObj.flush();
     
     return parseScoreSet;
  }
  
  public static double getParseScore(String sent, LexicalizedParser lp) {
    
    Tree parse = lp.parse(sent);
    double score = parse.score();
    return score;
  }

  @Override
  public double apply(Document doc) {
	List<List<String>> sentences = doc.getSentences();
	int sentLength, totalLength =0;
	double score=0;
	List<String> sent;
	LexicalizedParser lp = LexicalizedParser.loadModel(
            "englishPCFG.ser.gz");
	StringBuffer sentStr = new StringBuffer();
	// TODO Auto-generated method stub
	for (int i = 0; i<sentences.size(); i++) {
		sent = sentences.get(i);
		sentLength = sent.size();
		for (int j = 0; j<sent.size(); j++) {
			sentStr.append(sent.get(j));
			sentStr.append(" ");
		}
		score += sentLength*grammarScore.getParseScore(sent.toString(), lp);
		totalLength += sentLength;
	}
	return score/totalLength;
  }
  
}
