package forcomp

object Anagrams {

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
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = 
    w.toLowerCase.toList.groupBy(identity).mapValues(_.size).toList.sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = 
    s.flatMap(_.toLowerCase.toList).groupBy(identity).mapValues(_.size).toList.sorted

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
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = 
    dictionary.map(word => wordOccurrences(word) -> word).groupBy(occur => occur._1).mapValues{
     occurToWords => occurToWords.map(_._2)
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)).get

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
    def subSet(occurrence: (Char, Int)): List[Occurrences] = {
      val subSet = for (i <- 1 to occurrence._2) yield List((occurrence._1, i))
      List() :: subSet.toList
    }
    
    def combineTwo(left: List[Occurrences], right: List[Occurrences]): List[Occurrences] = {
      if (List(Nil) == left) right
      else if (List(Nil) == right) left
      else right.flatMap(r => left.map(l => l ++ r))
    }
   
    if (Nil == occurrences) List(Nil) 
    else {
      val occur = occurrences.head
      val occurSubset = subSet(occur)
      combineTwo(occurSubset, combinations(occurrences.tail))
    }
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
    if (Nil == x) Nil 
    else {
      val yMap = y.toMap
      val occur = x.head
      val optVal = yMap.get(occur._1)
      if (optVal.isEmpty) x.head :: subtract(x.tail, y)
      else {
        val count = occur._2 - optVal.get
        if (0 == count) subtract(x.tail, y) 
        else (occur._1, count) :: subtract(x.tail, y)
      }
    }
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
    def anagramsFromOccurrences(occurrences: Occurrences, anas: List[List[Word]]): List[List[Word]] = {
      if (Nil == occurrences) anas 
      else 
        combinations(occurrences).filter(_ != Nil).flatMap {
          comb => 
              val wordAnagram = dictionaryByOccurrences.get(comb)
              if (wordAnagram.isEmpty) List(Nil)
              else {
                val rest = subtract(occurrences, comb)
                val wordAnas = wordAnagram.get
                
                if (Nil == anas) 
                  anagramsFromOccurrences(rest, 
                    wordAnas.foldLeft(List(): List[List[Word]]){(accum, iter) => List(iter)::accum})
                else 
                  anagramsFromOccurrences(rest, 
                    wordAnas.foldLeft(List(): List[List[Word]]){(accum, iter) => accum ++ anas.map(iter::_)})
              }
                
            }
    }
   
    if (Nil == sentence) List(Nil)
    else anagramsFromOccurrences(sentenceOccurrences(sentence), Nil).filter(_ != Nil)
  }

}
