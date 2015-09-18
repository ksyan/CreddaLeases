package search

import scala.xml.Node
import scala.io.Source
import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import scala.collection.mutable.PriorityQueue
import java.io._
import scala.io.Source._

/**
 * Allows users to make queries to the index and prints the top 10 matches
 */
class Query(option: String, titleIndex: String, wordIndex: String) {
  
  private val idToName: HashMap[String, String] = new HashMap[String, String]
  private val wordToVals: HashMap[String, PriorityQueue[(String, Double)]] = new HashMap[String, PriorityQueue[(String, Double)]]
  private val titleFile = fromFile(titleIndex).getLines()

  for (line <- titleFile) {
    val idNameArr = line.split(",")
    idToName.update(idNameArr(0), idNameArr(1))
  }

  private val wordFile = fromFile(wordIndex).getLines()

  if (option == "kr") {

    for (line <- wordFile) {
      var wordValsArr = line.split("&&&:")
      var word = ""
      var docID: String = ""
      var docVal: Double = 0.0
      var tuplePQ = new PriorityQueue[(String, Double)]()
      if (wordValsArr.length > 1) {
        word = wordValsArr(0)
        wordValsArr = wordValsArr(1).split(",")
        tuplePQ = new PriorityQueue[(String, Double)]()(Ordering.by(x => x._2))
        for (count <- 0 to wordValsArr.length - 1) {
          if (count % 3 == 0) {
            docID = wordValsArr(count)
          } else if (count % 3 == 2) {
            docVal = wordValsArr(count).toDouble
            tuplePQ.enqueue((docID, docVal))
          }
        }
      }
      wordToVals.update(word, tuplePQ)
    }
  } else {
    for (line <- wordFile) {
      var wordValsArr = line.split("&&&:")
      var word = ""
      var docID: String = ""
      var docVal: Double = 0.0
      var tuplePQ = new PriorityQueue[(String, Double)]()
      if (wordValsArr.length > 1) {
        word = wordValsArr(0)
        wordValsArr = wordValsArr(1).split(",")
        tuplePQ = new PriorityQueue[(String, Double)]()(Ordering.by(x => x._2))
        for (count <- 0 to wordValsArr.length - 1) {
          if (count % 3 == 0) {
            docID = wordValsArr(count)
          } else if (count % 3 == 1) {
            docVal = wordValsArr(count).toDouble
            tuplePQ.enqueue((docID, docVal))
          }
        }
      }
      wordToVals.update(word, tuplePQ)
    }
  }

  private var input: String = ""
  private val inputReader: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
  while (input != ":quit") {
    print("Enter a query: ")
    input = inputReader.readLine()
    if (input != ":quit") {
      makeQuery(input.toLowerCase())
    }
  }
  println("Exiting...")

  /**
   * Makes a query and prints the top 10 matches
   * 
   * @param query the query to be made
   */
  private def makeQuery(query: String): Unit = {
    val queryArr: Array[String] = PorterStemmer.stemArray(query.split(" "))

    val docToVals: HashMap[String, Double] = new HashMap[String, Double]

    for (word <- queryArr) {
      val tuplePQ: Option[PriorityQueue[(String, Double)]] = wordToVals.get(word)

      if (tuplePQ != None) {
        val tupleArray = tuplePQ.get.toArray
        for (tuple <- tupleArray) {
          docToVals.update(tuple._1, docToVals.getOrElse(tuple._1, 0.0) + tuple._2)
        }
      }

    }
    val aggPQ: PriorityQueue[(String, Double)] = new PriorityQueue[(String, Double)]()(Ordering.by { x => x._2 })
    for (doc <- docToVals) {
      aggPQ.enqueue((doc._1, doc._2))
    }

    var counter = 0
    while (counter < Math.min(10, docToVals.size)) {
      println((counter + 1) + ". " + idToName.get(aggPQ.dequeue._1).get)
      counter += 1
    }
    if (docToVals.isEmpty) {
      println("We couldn't find this word in our database")
    }
  }
}

object Query {
  def main(args: Array[String]) {
    //"C:/Users/Benjamin/Documents/Eclipse/CS0180Scala/src/WordIndex"
    
    if (args.length != 2 && args.length != 3) {
      println("Invalid initial arguments!")
      System.exit(0)
    }
    
    try {
      if (args(0) == "--knightrank") {
        val testQuery = new Query("kr", args(1), args(2))
      } else {
        val testQuery = new Query("", args(0), args(1))
      }
    } catch {
      case e : FileNotFoundException => println("The file wasn't found!")
    }
  }
}