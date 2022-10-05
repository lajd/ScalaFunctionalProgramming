import scala.io.{ Codec, Source }
import util.chaining.scalaUtilChainingOps

val DEBUG = true

object Dictionary:
  val linuxwordsPath = List("forcomp", "linuxwords.txt").mkString("/", "/", "")
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(linuxwordsPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()


type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]
val dictionary: List[Word] = Dictionary.loadDictionary

def validateOccurrence(occurrences: Occurrences) =
  occurrences.foreach(x => assert(x._1 > 0))
  assert(occurrences == occurrences.sortBy(_._1))

def wordOccurrences(w: Word): Occurrences =
  val occurrences = toOccurrences(w.toLowerCase.groupBy(identity).map(x => (x._1, x._2.length)).toList)
  if DEBUG then validateOccurrence(occurrences)
  occurrences

def toOccurrences(elements: List[(Char, Int)]): Occurrences =
  val occurrences = elements.filter(x => x._1 > 0).sortBy(_._1)
  if DEBUG then validateOccurrence(occurrences)
  occurrences


/** Converts a sentence into its character occurrence list. */
def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

lazy val dictionaryByOccurrences = dictionary.groupBy(wordOccurrences)

def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())



def combinations(occurrences: Occurrences): List[Occurrences] =
  // Strategy here is to build up a list occurrences using foldLeft
  // Note: We use foldRight here to preserve the ordering of occurrences
  val combinationsOccs = occurrences.foldRight(List[Occurrences](Nil)) {
    case ((char, count), acc) =>
      // Accumulator is a list of occurrences
//      println("starting accumulator")
//      println(acc)
      val nextOccurrence = (
        for occAcc <- acc // Existing occurrence lists
            i <- 1 to count // obtain (char, 1), ... (char, count) occurrences
        // For each occurrence list, append the current (char, count) pair
        yield (char, i) :: occAcc
      )
//      println("Next occurrence")
//      println(nextOccurrence)
      val nextAcc = acc ::: nextOccurrence
      nextAcc
  }

  if DEBUG then combinationsOccs.foreach(validateOccurrence)
  combinationsOccs



def subtract(x: Occurrences, y: Occurrences): Occurrences =
  // Strategy to use foldLeft, starting with a map of X occurrences (xMap),
  // Update the xMap according to the values in yMap
  val yMap = y.toMap
  var occurrences = x.foldLeft(x.toMap) {
    case (map, (char, count)) =>
      val decrement = yMap.getOrElse(char, 0)
      if map(char) - decrement > 0 then map.updated(char, count - decrement) else map - char
  }.toList
  occurrences = toOccurrences(occurrences)
  if DEBUG then validateOccurrence(occurrences)
  occurrences

def sentenceAnagrams(sentence: Sentence): List[Sentence] =
  // Strategy is to use recursion
  def recurs(occurrences: Occurrences): List[Sentence] = occurrences match {
    case Nil => List(Nil)
    case _ =>
      for {
        comb <- combinations(occurrences)
        wordAnagram <- dictionaryByOccurrences.getOrElse(comb, Nil)
        ongoingSentence <- recurs(subtract(occurrences, wordOccurrences(wordAnagram)))
      } yield wordAnagram :: ongoingSentence
  }
  recurs(sentenceOccurrences(sentence))


val sentence = List("Yes", "man")

sentenceAnagrams(sentence)
