package forcomp

import scala.io.{ Codec, Source }

object Anagrams extends AnagramsInterface:

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase
    .toList.groupBy(c => c).map((k,list) => (k, list.length)).toList.sortBy(_._1)

  private def combineOccurrences(o1: Occurrences, o2: Occurrences): Occurrences = {
    Seq(o1, o2).flatten.groupBy(_._1).map((c, seq) => (c, seq.map(_._2).sum)).toList.sortBy(_._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    s match {
      case head :: Nil => wordOccurrences(head)
      case head :: tail => combineOccurrences(wordOccurrences(head), sentenceOccurrences(tail))
      case Nil => List.empty
    }
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.map(w => (wordOccurrences(w) ,w)).groupBy(_._1).map((o, seq) => (o, seq.map(_._2)))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val occurrences = wordOccurrences(word)
    dictionaryByOccurrences.get(occurrences) match {
      case Some(list) => list
      case None => List.empty
    }
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def expand(occ: Occurrences): List[Occurrences] = occ match {
      case Nil => List(Nil)
      case (char, n) :: rest =>
        val restCombinations = expand(rest)
        (0 to n).flatMap(i => {
          val currentOcc = if (i > 0) List((char, i)) else Nil
          restCombinations.map(comb => currentOcc ++ comb)
        }).toList
    }

    expand(occurrences)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    //if !combinations(x).contains(y) then throw new IllegalStateException("y should be a subset of x")
    val xMap = x.toMap
    y.foldLeft(xMap){(accMap, yEntry) =>
      val (yChar, yF) = yEntry
      // y is a subset of x, thus the key exists
      val xF = accMap(yChar)
      if xF - yF > 0 then accMap.updated(yChar, xF - yF)
      else accMap - yChar
    }.toList.sortBy(_._1)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    val totalOccurrences: Occurrences = sentence.foldLeft(emptyOccurrences) { (acc, word) =>
      combineOccurrences(acc, Anagrams.wordOccurrences(word))
    }

    var memo: Map[Occurrences, List[Sentence]] = Map.empty

    /** The core recursive function that solves the anagram problem.
     * It uses the `memo` cache for efficiency.
     */
    def solve(remainingOccurrences: Occurrences): List[Sentence] = {

      // memoization
      memo.get(remainingOccurrences) match {
        case Some(solutions) => solutions // Return cached result
        case None =>

          val solutions = remainingOccurrences match {
            case Nil => List(Nil) // Base Case: An empty set of occurrences yields one empty sentence

            case _ =>
              val allCombinations = Anagrams.combinations(remainingOccurrences).filter(_.nonEmpty)

              val result = for {
                thisWordOccurrence <- allCombinations
                nextRemainingOccurrences = Anagrams.subtract(remainingOccurrences, thisWordOccurrence)
                word <- dictionaryByOccurrences.getOrElse(thisWordOccurrence, Nil)
                restOfSentence <- solve(nextRemainingOccurrences) // Recursive call to the memoized solver
              } yield word :: restOfSentence

              result
          }
          memo = memo.updated(remainingOccurrences, solutions)
          solutions
      }
    }
    //println(memo)
    solve(totalOccurrences)
  }

  private def emptyOccurrences : Occurrences = List.empty.asInstanceOf[Occurrences]

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
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
