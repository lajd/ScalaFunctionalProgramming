package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)

    val s123 = setFrom(1, 2, 3)
    val s234 = setFrom(2, 3, 4)
    val evenSet = setFrom(2, 4, 6, 8, 10)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one".ignore) {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("test intersect") {
    new TestSets:
      val iSet = intersect(s123, s234)

      for (i <- List(2, 3)) {
        assert(contains(iSet, i))
      }

      for (i <- List(1, 4)) {
        assert(!contains(iSet, i))
      }
  }

  test("test diff") {
    new TestSets:
      val iSet = diff(s123, s234)

      assert(contains(iSet, 1))
      for (i <- List(2, 3, 4, 5)) {
        assert(!contains(iSet, i))
      }
  }

  test("test filter") {
    new TestSets :
      val fSet = filter(s123, x => Set(2, 3).contains(x))
      assert(!contains(fSet, 1))
      assert(contains(fSet, 2))
      assert(contains(fSet, 3))
  }

  test("test forall") {
    new TestSets :
      assert(forall(evenSet, x => x % 2 == 0))

      val nonEvenSet = union(evenSet, singletonSet(1))
      assert(!forall(nonEvenSet, x => x % 2 == 0))
  }
  test("test exists") {
    new TestSets :
      assert(exists(s1, x => x == 1))
      assert(!exists(s2, x => x == 1))
      assert(exists(s234, x => x == 2))

      // Test bounds
      assert(exists(setFrom(-1000, -900, -500, 0, 500, 1000), x => x == -1000))
      assert(exists(setFrom(-1000, -900, -500, 0, 500, 1000), x => x -1000 == 0))
      assert(!exists(setFrom(-1000, -900, -500, 0, 500, 1000), x => x -1001 == 0))

  }

  test("test map") {
    new TestSets :
      val s234Plus100 = map(s234, x => x + 100)
      assert(contains(s234Plus100, 102))
      assert(contains(s234Plus100, 103))
      assert(!contains(s234Plus100, 101))
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
