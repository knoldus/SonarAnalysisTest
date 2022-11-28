package objsets

import TweetReader._
import objsets.GoogleVsApple.google

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet extends TweetSetInterface {
  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet =  ???

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet = ???

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet = ???

  def leastRetweeted: Tweet = ???

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = ???

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

object TweetSet {
  def maxDepth(root: TweetSet): Int = {
    root match {
      case _: Empty => 0
      case n: NonEmpty => Math.max(maxDepth(n.left), maxDepth(n.right)) + 1
    }
  }
}

class Empty extends TweetSet {
  override def mostRetweeted: Tweet = throw new NoSuchElementException("The set is Empty")

  override  def leastRetweeted: Tweet = throw new NoSuchElementException("Set is Empty")

  override def descendingByRetweet: TweetList = Nil

  override def union(that: TweetSet): TweetSet = that

  override def filter(p: Tweet => Boolean): TweetSet = new Empty

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  override def toString = "{...}"
}

class NonEmpty(val elem: Tweet,val left: TweetSet,val right: TweetSet) extends TweetSet {
  override def mostRetweeted: Tweet = {
    def getChildNodes(node: TweetSet): List[TweetSet] =
      node match {
        case _: Empty => List.empty
        case n: NonEmpty => List(n)
      }
    def search(currentMax: Tweet, nodesLeft: List[TweetSet]): Tweet = {
      if (nodesLeft.isEmpty) currentMax
      else {
        val head: NonEmpty = nodesLeft.head match {
          case _: Empty => new NonEmpty(currentMax,new Empty,new Empty)
          case n: NonEmpty => n
        }
        val nodesToCheck: List[TweetSet] = getChildNodes(head.left) ++
          getChildNodes(head.right) ++
          nodesLeft.tail
        val headTweet: Tweet = head.elem
        val newMax = if(currentMax.retweets < headTweet.retweets) headTweet else currentMax
        search(newMax,nodesToCheck)
      }
    }
    search(elem,List(this))
  }

  def getChildNodes(node: TweetSet): List[TweetSet] =
    node match {
      case _: Empty => List.empty
      case n: NonEmpty => List(n)
    }

  override def leastRetweeted: Tweet = {

    def search(currentMin: Tweet, nodesLeft: List[TweetSet],iter: Int): Tweet = {
      if (nodesLeft.isEmpty) currentMin
      else {
        val head: NonEmpty = nodesLeft.head match {
          case _: Empty => new NonEmpty(currentMin, new Empty, new Empty)
          case n: NonEmpty => n
        }
        val nodesToCheck: List[TweetSet] = getChildNodes(head.left) ++
          getChildNodes(head.right) ++
          nodesLeft.tail
        val headTweet: Tweet = head.elem
        val newMin = if (currentMin.retweets > headTweet.retweets) headTweet else currentMin
        search(newMin, nodesToCheck, iter+1)
      }
    }

    search(elem, List(this), 1)
  }

    override def descendingByRetweet: TweetList = {
    def createTweetList(node: TweetSet,acc: TweetList): TweetList = {
      node match {
        case _: Empty => acc
        case tSet: NonEmpty =>
          val least = tSet.leastRetweeted
          val remainingTree = tSet.remove(least)
          val tweetsList = new Cons(least,acc)
          createTweetList(remainingTree,tweetsList)
      }
    }
    createTweetList(this, Nil)
  }

  override def union(that: TweetSet): TweetSet = {
    def merge(acc: TweetSet, nodesToAdd: List[TweetSet]): TweetSet = {
      if(nodesToAdd.isEmpty) acc
      else{
        val head: NonEmpty = nodesToAdd.head match {
          case _: Empty => throw new NoSuchElementException("Found Empty TweetSet in list \"nodesToAdd\" in call union ")
          case n: NonEmpty => n
        }
        val newNodesToAdd: List[TweetSet] = getChildNodes(head.left) ++ getChildNodes(head.right) ++ nodesToAdd.tail
        merge(acc.incl(head.elem),newNodesToAdd)
      }
    }
    that match {
      case node: Empty => node.union( this)
      case node: NonEmpty => merge(this,List(node))
    }
  }

  override def filter(p: Tweet => Boolean): TweetSet = {
    filterAcc(p,new Empty)
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if(p(this.elem))
      (acc incl(this.elem) union this.left.filterAcc(p,acc)) union this.right.filterAcc(p,acc)
    else
      this.left.filterAcc(p,acc) union this.right.filterAcc(p,acc)
  }


  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  override def toString: String = "{" + left + "," + elem + "," + right + "}"
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def searchStringForAnyWordInList(text:String, words:List[String]): Boolean = {
    if(words.isEmpty) false
    else if(text.contains(words.head)) true
    else searchStringForAnyWordInList(text,words.tail)
  }

  def findGoogle: Tweet => Boolean = x => searchStringForAnyWordInList(x.text,google)
  def findApple: Tweet => Boolean = x => searchStringForAnyWordInList(x.text,apple)
  def findBoth: Tweet => Boolean = x => searchStringForAnyWordInList(x.text,apple++google)


  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(findGoogle)
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(findApple)

  val test = googleTweets.mostRetweeted

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
  //println (GoogleVsApple.googleTweets.descendingByRetweet)
  //GoogleVsApple.appleTweets foreach println
}
