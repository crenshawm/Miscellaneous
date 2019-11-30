import scala.io.Source
import scala.collection.mutable.Map

/*
Name: Mark A Crenshaw
Date: September 8th, 2018
CSE262-011
Dr. James A. Femister
*/

// The program ran in under 5 minutes on the Sunlab but on my personal
// computer it ran in over 5 minutes. I apologize for any inconvenience.
object Ngram {
  def main(args: Array[String]): Unit = {
    
    if (args.length > 0) {

      val lines = Source.fromFile(args(0)).getLines()
      var pairs = List[Tuple2[String, String]]()
      var lst = List[String]()
      var pairCounts = Map[Tuple2[String,String],Int]()

      for (line <- lines) {
        lst = lst ::: line.replaceAll("""[*!?,.;\-':\t()"`]+""","").toLowerCase.split("\\s+").filterNot(_.isEmpty).toList
      }
      pairs = lst.zip(lst.drop(1))
      
      
      // Records the number of times a particular word follows another particular word.
      def countPairs(word1:String, word2:String): Int = 
      {
        if (word2 == "*")
        {
          //var toCompare = Tuple2(word1, word2)
          var count = 0
          
          for (pair <- pairs)
          {
            if (pair._1 == word1)
              {
                pairCounts += pair -> count
                count = count + 1
              }
          }
          count
        }
        else
        {
          var toCompare = Tuple2(word1, word2)
          var count = 0
          
          for (pair <- pairs)
          {
            if (pair == toCompare)
              {
                pairCounts += toCompare -> count
                count = count + 1
              }
          }
          count
          }
      }
    def countAll(word:String): Int = {
      countPairs(word, "*")
    }
    

    def p1(of:String, given:String): Double = {
      try
      {
        countPairs(given, of).toDouble / countAll(given).toDouble
      } catch {
        case e: java.lang.ArithmeticException => 0
      }
    }
    def mostLikelyNextWord(given:String): String = {
      var givenPairs = pairs.filter(x=>x._1 == given)
      var maxP1 = 0.0
      var currP1 = 0.0
      var maxWd = ""
      for (pair <- givenPairs)
      {
        currP1 = p1(pair._2, given)
        if (currP1 > maxP1) 
        {
          maxP1 = currP1
          maxWd = pair._2
        
        }     
      }
      maxWd
    
    }

      for (word <- lst.sorted.distinct)
      {
        println(word + " " + mostLikelyNextWord(word))
      }
  } 

}

}
