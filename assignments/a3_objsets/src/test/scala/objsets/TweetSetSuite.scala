package objsets

class TweetSetSuite extends munit.FunSuite:
  import TweetReader.*

  def tweetGenerator(n: Int, n0: Int): TweetSet =
    var res = List[Tweet]()
    for (i <- Range(n0, n0 + n)) {
      res = res :+ Tweet(i.toString, i.toString + " body", i)
    }
    res.foldLeft(Empty(): TweetSet)(_.incl(_))

  trait TestSets:
    val set1 = Empty()  // No elements
    val set2 = set1.incl(Tweet("a", "a body", 20))  // 1 elem
    val set3 = set2.incl(Tweet("b", "b body", 20)) // 2 elem
    val c = Tweet("c", "c body", 7)
    val d = Tweet("d", "d body", 9)
    val set4c = set3.incl(c) // 3 elem, a, b, c
    val set4d = set3.incl(d) // 3 elem, a, b, d
    val set5 = set4c.incl(d) // 4 elem

    // Used to validate time complexity
    val set0To100 = tweetGenerator(100, 0)
    val set100To200 = tweetGenerator(100, 100)

  def asSet(tweets: TweetSet): Set[Tweet] =
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets:
      assertEquals(size(set1.filter(tw => tw.user == "a")), 0)
  }

  test("filter: a on set5") {
    new TestSets:
      assertEquals(size(set5.filter(tw => tw.user == "a")), 1)
  }

  test("filter: twenty on set5") {
    new TestSets:
      assertEquals(size(set5.filter(tw => tw.retweets == 20)), 2)
  }

  test("filter: even on set0To100") {
    new TestSets:
      assertEquals(size(set0To100.filter(tw => tw.retweets % 2 == 0)), 50)
  }

  test("union: set4c union with itself") {
    new TestSets:
      val s4c = set4c.union(set4c)
      assertEquals(size(s4c), size(set4c))
  }

  test("union: set4c and set4d") {
    new TestSets:
      val x = set4c.union(set4d)
      assertEquals(size(x), 4)
  }

  test("union: with empty set1") {
    new TestSets:
      assertEquals(size(set5.union(set1)), 4)
  }

  test("union: with empty set2") {
    new TestSets:
      assertEquals(size(set1.union(set5)), 4)
  }

  test("union: set0To1000 with set1000To2000") {
    new TestSets:
      val s200 = set0To100.union(set100To200)
      assertEquals(size(s200), 200)
  }

  test("descending: set5") {
    new TestSets:
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a")
  }

  test("descending: set0To100") {
    new TestSets:
      val trends = set0To100.descendingByRetweet
      assertEquals(trends.head.user, "99")
      assertEquals(trends.tail.head.user,  "98")
      assertEquals(trends.tail.tail.head.user,  "97")
  }

  test("remove: set5") {
    new TestSets:
      assertEquals(size(set5), 4)
      val s5RemovedC = set5.remove(c)
      assertEquals(size(s5RemovedC), 3)
  }

  test("contains: set3/set5") {
    new TestSets:
      assert(set5.contains(c))
      assert(!set3.contains(c))
  }

  test("foreach: set5") {
    new TestSets:
      var l = Array[Tweet]()
      set5.foreach(x => {l = l :+ x})
      assert(l.length == 4)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
