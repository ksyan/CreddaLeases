# Search

Search Engine that performs queries on a corpus of wikipedia pages

Main Objects

Index.scala indexes a given xml file. Query.scala allows users to make queries to what has been indexed.

Techniques Used

Management of multiple Hashmaps to hold different frequencies and values, Regex Matching to filter out certain patterns, Iterative calculation to determine pagerank (it iterates until the average difference becomes less than a certain number), Use of a buffered writer to write data in hashmaps to index files, Infinite while loop so user can input multiple queries, Optional inclusion of pagerank.

Functionality

A user should first run Index.scala and give the program an xml file that should be indexed. Two files will be produced, Title Index (a file that contains (id, document name) pairs), and WordIndex (a file that contains all unique words after stemming that appear in the wiki corpus followed by a sequence of (document id, word score for that document, knight rank score) triples). "Knight Rank" in this project is similar to pagerank for normal search engines - it uses link structure to calculate and prioritize what pages are more important. The user should then run Query.scala, where they can submit inputs, with an optional argument for including the knight rank value (if the user types --knightrank as the second argument). The top 10 matches to the query will be printed out.

Example Command Line

scala search.Index /course/cs018/src/search/ExampleWiki.xml

scala search.Query TitleIndex WordIndex

OR

scala search.Query --knightrank TitleIndex WordIndex
