package search

import scala.xml.Node
import scala.io.Source
import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import java.io._

/**
 * Indexes the given file
 */
class Index(filename : String) {
  private val DAMPENING: Double = 0.85
  
  private val xmlNode: Node = xml.XML.loadFile(filename)

  private val children: Seq[Node] = xmlNode.child.filter(x => x.label.contains("page"))

  private val wordMap: HashMap[String, Int] = new HashMap[String, Int]
  private val wordDocFreq: HashMap[String, Int] = new HashMap[String, Int]
  private val wordScore: HashMap[String, List[(String, Double)]] = new HashMap[String, List[(String, Double)]]
  private val nameToID: HashMap[String, String] = new HashMap[String, String]
  private val idToKRank: HashMap[String, Double] = new HashMap[String, Double]
  private val toAdd: HashMap[String, Double] = new HashMap[String, Double]
  private val docToLinks: HashMap[String, List[String]] = new HashMap[String, List[String]]

  var numDocs: Int = 0

  private val titleBW = new BufferedWriter(new FileWriter("TitleIndex"))
  private val wordBW = new BufferedWriter(new FileWriter("WordIndex"))

  for (i <- children) {
    numDocs += 1
    val id: String = (i \ "id").text.replaceAll("\n", "")
    val title: String = (i \ "title").text.replaceAll("\n", "")
    nameToID.update(title.toLowerCase(), id)
    titleBW.write(id + "," + title + "\n")

    val page: String = (i \ "text").text.toLowerCase()
    val regex = new Regex("""\[\[[^\[]+?\]\]|[^\W\_]+'[^\W\_]+|[^\W\_]+""")
    val matchesIterator = regex.findAllMatchIn(page)
    var textList = matchesIterator.toList.map { aMatch => aMatch.matched}

    val wordFreq: HashMap[String, Int] = new HashMap[String, Int]

    val titleArr = title.split(" ")
    var titleCount = 0
    while (titleCount < 10) {
      for (i <- titleArr) {
        textList = textList.::(PorterStemmer.stem(i.toLowerCase()))
      }
      titleCount += 1
    }
    
    for (w <- textList) {
      val possLink = (w.replace("[[", "").replace("]]", ""))
      if (possLink != w && possLink != title) {
        docToLinks.update(id, docToLinks.getOrElse(id, Nil).::(possLink))
        val regexLink = new Regex("""\[\[[^\[]+?\]\]|[^\W\_]+'[^\W\_]+|[^\W\_]+""")
        val matchesIteratorLink = regexLink.findAllMatchIn(possLink)
        var textListLink = PorterStemmer.stemArray(matchesIteratorLink.toArray.map { aMatch => aMatch.matched})
        for (wLink <- textListLink) {
          wordFreq.update(wLink, wordFreq.getOrElse(wLink, 0) + 1)
        }
      } else {
        val stemmedW = PorterStemmer.stem(w)
        wordFreq.update(stemmedW, wordFreq.getOrElse(stemmedW, 0) + 1)
      }
    }

    var euclidean: Double = 0.0

    for (wf <- wordFreq) {
      wordDocFreq.update(wf._1, wordDocFreq.getOrElse(wf._1, 0) + 1)
      euclidean += Math.pow(wf._2, 2)
    }

    euclidean = Math.sqrt(euclidean)
    
    for (wf2 <- wordFreq) {
      wordScore.update(wf2._1, wordScore.getOrElse(wf2._1, Nil).::((id, wf2._2 / euclidean)))
    }
  }

  for (ws <- wordScore) {
    var tupleList: List[(String, Double)] = List()
    for (d <- ws._2) {
      if (numDocs.toDouble == (wordDocFreq.apply(ws._1) + 1)) {
      tupleList = tupleList.::(d._1, d._2 * Math.abs(Math.log(numDocs.toDouble / (wordDocFreq.apply(ws._1)))))
      } else {
      tupleList = tupleList.::(d._1, d._2 * Math.abs(Math.log(numDocs.toDouble / (1 + wordDocFreq.apply(ws._1)))))
    }
    }
    wordScore.update(ws._1, tupleList)
  }

  for (i <- children) {
    val id: String = (i \ "id").text.replaceAll("\n", "")
    idToKRank.update(id, 1.0 / numDocs)
  }
  
  private var averageDiff: Double = Double.MaxValue
  private var averageDiff2: Double = 0.0
  while (averageDiff > 0.000001) {
    for (doc <- toAdd) {
      toAdd.update(doc._1, 0.0)
    }
    
    for (dl <- docToLinks) {
      for (link <- dl._2) {
        if (nameToID.get(link) != None) {
          toAdd.update(nameToID.apply(link), toAdd.getOrElse(nameToID.apply(link), 0.0) + (idToKRank.apply(dl._1) / dl._2.length))
        }
      }
    }
    
    for (doc <- toAdd) {
      idToKRank.update(doc._1, doc._2 + idToKRank.apply(doc._1))
    }
    
    for (doc <- idToKRank) {
      idToKRank.update(doc._1, (((1.0 - DAMPENING) / numDocs) + DAMPENING * doc._2))
    }

    var averageDiff3: Double = ((idToKRank.values.foldLeft(0.0)((x, y) => x + y)) / numDocs)
    averageDiff = averageDiff3 - averageDiff2
    averageDiff2 = averageDiff3
  }

  for (ws <- wordScore) {
    var tripleList: List[(String, Double, Double)] = List()
    
    for (l <- ws._2) {
      tripleList = tripleList.::(l._1, l._2, Math.pow(idToKRank.apply(l._1), Math.log10(100000.0/numDocs.toDouble)) * Math.pow(l._2, 1))
    }
    
    wordBW.write(ws._1 + "&&&:")
    
    for (triple <- tripleList) {
      wordBW.write(triple._1.replaceAll("\n", "") + "," + triple._2 + "," + triple._3 + ",")
    }
    
    wordBW.write("\n")
  }

  titleBW.close()
  wordBW.close()

}

object Index {
  def main(args: Array[String]) {
    
    if (args.length != 1) {
      println("Invalid initial arguments!")
      System.exit(0)
    }
    
    try {
      val startTime = System.currentTimeMillis()
      val testindex = new Index(args(0))
      val timeTaken = (System.currentTimeMillis() - startTime)
      println("Indexing completed in " + timeTaken / 1000.0 + " seconds.")
    } catch {
      case e : FileNotFoundException => println("The file wasn't found!")
    }
  }
}