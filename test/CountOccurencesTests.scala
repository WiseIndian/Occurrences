import org.scalatest.FunSuite
import controllers._
import scala.collection.mutable.Buffer


/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
class CountOccurencesTests extends FunSuite {

	test("separatePunctuation should return Buffer(Other(\"i am someone\"))") {
		val co = new CountOccurences 
		val f = co.separatePunctuation _
		val inpt = "i am someone"
		assert(f(inpt) == Buffer(Other(inpt)))
	}
	test("separatePunctuation should return Buffer(Other(\"hello world\"), Punctuation(\"!\"))") {
		val co = new CountOccurences 
		val f = co.separatePunctuation _
		val inpt = "hello world!"
		val expected: Seq[ImportantTextPart] = 
			Seq(
				Other("hello world"), 
				Punctuation("!")
			)
		val testRes = 
			f(inpt)
			.zip(expected)
			.foldLeft(true) { case (z, (e1,e2)) => 
				z && e1 == e2
			}

		assert(testRes)
	}

	test(s"""separatePunctuation should return Buffer(Other("hello dude"), Punctuation(","), Other(" it is me mario"))""") {
		val inpt = "hello dude, it is me mario"
		val co = new CountOccurences
		val res = co.separatePunctuation(inpt)
		val expected = 
			Seq(
				Other("hello dude"),
				Punctuation(","),
				Other(" it is me mario")
			)
		assert(res == expected)
	}

	test(s"""separatePunctuation multiple punctuation following each other""") {
		val inpt = "hey!!pizza?spaghetti..reggiano"
		val expected = 
			Seq(
				Other("hey"), 
				Punctuation("!"),
				Punctuation("!"),
				Other("pizza"),
				Punctuation("?"),
				Other("spaghetti"),
				Punctuation("."),
				Punctuation("."),
				Other("reggiano")
			)
		val res = new CountOccurences().separatePunctuation(inpt)
		assert(expected == res)
	}
}

