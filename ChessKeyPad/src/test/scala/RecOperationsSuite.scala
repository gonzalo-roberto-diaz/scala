import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnitRunner])
class RecOperationsSuite extends FunSuite with RecOperations[Char]{

  test("find the n-th level"){
    val lev3_1 = Rec[Char](ListBuffer('a', 'b'), ListBuffer())
    val lev3_2 = Rec[Char](ListBuffer('c'), ListBuffer())
    val lev3_3 = Rec[Char](ListBuffer('d'), ListBuffer())
    val lev2_1 = Rec[Char](ListBuffer('e'), ListBuffer(lev3_1, lev3_2))
    val lev2_2 = Rec[Char](ListBuffer('f'), ListBuffer(lev3_3))
    val lev1 = Rec[Char](ListBuffer('g'), ListBuffer(lev2_1, lev2_2))

    val rl1 =  recsLevel(lev1, 0)
    assert(rl1.size == 1)
    assert(rl1(0) == lev1)

    val rl2 = recsLevel(lev1, 1)
    assert(rl2.size == 2)
    assert(rl2.contains(lev2_1))
    assert(rl2.contains(lev2_2))

    val rl3 = recsLevel(lev1, 2)
    assert(rl3.size == 3)
    assert(rl3.contains(lev3_1))
    assert(rl3.contains(lev3_2))
    assert(rl3.contains(lev3_3))
  }

  test("isUnary"){
    val rec1 = Rec[Char](ListBuffer('a', 'b'), ListBuffer())
    assert(!rec1.isUnary)

    val rec2 = Rec[Char](ListBuffer(), ListBuffer())
    assert(!rec2.isUnary)

    val rec3 = Rec[Char](ListBuffer('a'), ListBuffer(rec1))
    assert(!rec3.isUnary)

    val rec4 = Rec[Char](ListBuffer('a'), ListBuffer())
    assert(rec4.isUnary)
  }



  test("tailUnaries"){

    val filler1 = Rec[Char](ListBuffer('1'), ListBuffer())
    val filler2 = Rec[Char](ListBuffer('2'), ListBuffer())

    val rec1 = Rec[Char](ListBuffer('a', 'b'), ListBuffer(filler1))
    assert(rec1.allUnary)

    val rec2 = Rec[Char](ListBuffer(), ListBuffer(filler1, filler2))
    assert(rec2.allUnary)


    val rec3 = Rec[Char](ListBuffer(), ListBuffer(rec2))
    assert(!rec3.allUnary)
  }

  test("enrichOne 1: one gets enriched"){

    def enrichingFunction[T](ch: T): List[T] ={
      return List('b', 'c').asInstanceOf[List[T]]
    }

    val unary = Rec[Char](ListBuffer('a'), ListBuffer())
    val rec = Rec[Char](ListBuffer('x'), ListBuffer(unary))

    assert(rec.allUnary)
    rec.enrichOne(enrichingFunction)
    assert(!rec.allUnary)

  }

  test("enrichOne 2: reusing tails"){

    def enrichingFunction[T](ch: T): List[T] ={
      return List('1', '2').asInstanceOf[List[T]]
    }


    val rec = Rec[Char](ListBuffer('x'), ListBuffer(createUnaryRec('a'), createUnaryRec('c')))

    rec.enrichOne(enrichingFunction)
    rec.enrichOne(enrichingFunction)

    assert(rec.recTail.head.recHead == ListBuffer('a', 'c'))
    println(rec)
    // note that I can't reuse 'a' y 'c' to test this, as they were modified bu the enrichment
    assert(rec.recTail.head.recTail == ListBuffer(createUnaryRec('1'), createUnaryRec('2')))
  }


  test("enrichOne 3: eliminating unaries if no derivate found"){

    def enrichingFunction[T](ch: T): List[T] ={
      if (ch == 'a') return Nil.asInstanceOf[List[T]];
      else return List('1', '2').asInstanceOf[List[T]]
    }


    val rec = Rec[Char](ListBuffer('x'), ListBuffer(createUnaryRec('a'), createUnaryRec('c')))

    rec.enrichOne(enrichingFunction)
    rec.enrichOne(enrichingFunction)

    assert(rec.recTail.head.recHead == ListBuffer('c'))
    println(rec)
    assert(rec.recTail.head.recTail == ListBuffer(createUnaryRec('1'), createUnaryRec('2')))
  }

  test("cardinality"){
    val lev3_1 = Rec[Char](ListBuffer('a'), ListBuffer())
    val lev3_2 = Rec[Char](ListBuffer('c'), ListBuffer())
    val lev3_3 = Rec[Char](ListBuffer('d'), ListBuffer())
    val lev2_1 = Rec[Char](ListBuffer('e'), ListBuffer(lev3_1, lev3_2))
    val lev2_2 = Rec[Char](ListBuffer('f'), ListBuffer(lev3_3))
    val lev1 = Rec[Char](ListBuffer('g'), ListBuffer(lev2_1, lev2_2))


    assert(lev1.cardinality == 3)

    assert(lev2_1.cardinality == 2)
    assert(lev2_2.cardinality == 1)
  }

  test("last level recs"){
    val lev3_1 = Rec[Char](ListBuffer('c'), ListBuffer())
    val lev3_2 = Rec[Char](ListBuffer('c'), ListBuffer())
    val lev3_3 = Rec[Char](ListBuffer('d'), ListBuffer())
    val lev2_1 = Rec[Char](ListBuffer('e'), ListBuffer(lev3_1, lev3_2))
    val lev2_2 = Rec[Char](ListBuffer('f'), ListBuffer(lev3_3))
    val lev1 = Rec[Char](ListBuffer('g'), ListBuffer(lev2_1, lev2_2))


    val last = lev1.lastLevelRecs

    assert(last.size == 2)
    assert(last.contains(lev2_1))
    assert(last.contains(lev2_2))

  }



}