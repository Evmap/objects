package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

abstract class TweetSet {

  def filter(p: Tweet => Boolean): TweetSet = filter0(p, new Empty)
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet = {
    if (that.isEmpty) this
    else incl(that.head) union that.tail
  }

  def descendingByRetweet: Trending = {
    if (isEmpty) new EmptyTrending
    else {
      val min = findMin
      new NonEmptyTrending(min, remove(min).descendingByRetweet)
    }
  }

  def incl(x: Tweet): TweetSet
  def contains(x: Tweet): Boolean
  def isEmpty: Boolean
  def head: Tweet
  def tail: TweetSet


  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }

  def remove(tw: Tweet): TweetSet

  def findMin0(curr: Tweet): Tweet =
    if (this.isEmpty) curr
    else if (this.head.retweets < curr.retweets) this.tail.findMin0(this.head)
    else this.tail.findMin0(curr)

  def findMin: Tweet =
    this.tail.findMin0(this.head)
}

class Empty extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = accu

  def contains(x: Tweet): Boolean = false
  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)
  def isEmpty = true
  def head = throw new Exception("Empty.head")
  def tail = throw new Exception("Empty.tail")
  def remove(tw: Tweet): TweetSet = this

}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = {
    val leftright = left.filter0(p, right.filter0(p, accu))
    if (p(elem)) leftright.incl(elem)
    else leftright
  }

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def isEmpty = false
  def head = if (left.isEmpty) elem else left.head
  def tail = if (left.isEmpty) right else new NonEmpty(elem, left.tail, right)

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)
  // -------------------------------------------------------------------------
}


abstract class Trending {
  def + (tw: Tweet): Trending
  def head: Tweet
  def tail: Trending
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }
}

class EmptyTrending extends Trending {
  def + (tw: Tweet) = new NonEmptyTrending(tw, new EmptyTrending)
  def head: Tweet = throw new Exception
  def tail: Trending = throw new Exception
  def isEmpty: Boolean = true
  override def toString = "EmptyTrending"
}

class NonEmptyTrending(elem: Tweet, next: Trending) extends Trending {
  def + (tw: Tweet): Trending =
    new NonEmptyTrending(elem, next + tw)
  def head: Tweet = elem
  def tail: Trending = next
  def isEmpty: Boolean = false
  override def toString =
    "NonEmptyTrending(" + elem.retweets + ", " + next + ")"
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")

  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def byKeywords(words: List[String]) = (t:Tweet) => words.exists( (term) => t.text.contains(term) )

  val googleTweets: TweetSet = TweetReader.allTweets.filter( byKeywords(google) )

  val appleTweets: TweetSet = TweetReader.allTweets.filter( byKeywords(apple) )

  val trending: Trending = googleTweets.union(appleTweets).descendingByRetweet
}
object Main extends App {
  GoogleVsApple.trending foreach println
}
