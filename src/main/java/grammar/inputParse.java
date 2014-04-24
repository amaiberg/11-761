package grammar;


import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;


public class inputParse {
  /*
  public static void main(String[] args) throws IOException{
    // TODO Auto-generated method stub
    String dir = "/Users/yingsheng/Dropbox/cmu2014s/lang_stat_11661/lang_stat_project/data/";
    FileReader fin = new FileReader(dir + "developmentSet.text");
    
    BufferedReader in = new BufferedReader(fin);
    String line = in.readLine();
    int count = 0;
    ArrayList<ArrayList<String>> developSet = new ArrayList<ArrayList<String>>();
    ArrayList<String> article = new ArrayList<String>();
    
    while (line != null) {
        if (line.trim().equals("~~~~~")) {
            if (article.size() != 0) {
              developSet.add(article);
            }
            article = new ArrayList<String>();
            count++;
            System.out.println("article " + count + ":" );

        } else {
           article.add(line);
        }
        line = in.readLine();
    }
    in.close();
    System.out.println("Number of articles: "+ count);
  }
  */
  
  public static ArrayList<ArrayList<String>> getParsedInput(String filename)
    throws IOException {
    
    String dir = "/Users/yingsheng/Dropbox/cmu2014s/lang_stat_11661/lang_stat_project/data/";
    FileReader fin = new FileReader(dir + filename);
    
    BufferedReader in = new BufferedReader(fin);
    String line = in.readLine();
    int count = 0;
    ArrayList<ArrayList<String>> articleSet = new ArrayList<ArrayList<String>>();
    ArrayList<String> article = new ArrayList<String>();
    
    while (line != null) {
        if (line.trim().equals("~~~~~")) {
            if (article.size() != 0) {
              articleSet.add(article);
            }
            article = new ArrayList<String>();
            count++;
            //System.out.println("article " + count + ":" );

        } else {
           article.add(line.substring(4,line.length()-5));
           
        }
        line = in.readLine();
    }
    articleSet.add(article);
    in.close();
    System.out.println("Number of articles: "+ count + "setSize" + articleSet.size());
    return articleSet;   
  }
  

}
