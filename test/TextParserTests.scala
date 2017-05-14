import org.scalatest.FunSuite
import controllers._

class TextParserSuite extends FunSuite{
	val input1 = "hello this is me"
	test(s"""wordOccurrenceMap on "$input1"""")  {
		val p = new TextParser() 	
		val parts: Seq[ImportantTextPart] = p.wordOccurrenceMap(input1)
		val expected = Seq[ImportantTextPart](
			Word("hello", 1), Spaces(" "), Word("this", 1), Spaces(" "), Word("is", 1),
			Spaces(" "), Word("me", 1)
		)
	}

	val input2= "nonsensical stuff...in this sentence!"
	test(s"""wordOccurrenceMap on "$input2"""")  {
		val p = new TextParser() 	
		val parts: Seq[ImportantTextPart] = p.wordOccurrenceMap(input2)
		val expected = Seq[ImportantTextPart](
			Word("nonsensical", 1), Spaces(" "), Word("stuff", 1), 
			Punctuations("..."), Word("in", 1), Spaces(" "),
			Word("this", 1), Spaces(" "), Word("sentence", 1),
			Punctuations("!")
		)
	}

	val input3= ""
	test(s"""wordOccurrenceMap on "$input3"""")  {
		val p = new TextParser() 	
		val parts: Seq[ImportantTextPart] = p.wordOccurrenceMap(input3)
		val expected = Seq[ImportantTextPart]()
	}

	val input4= "   what is up?\n\n\t this is a test. It IS"
	test(s"""wordOccurrenceMap on "$input4"""")  {
		val p = new TextParser() 	
		val parts: Seq[ImportantTextPart] = p.wordOccurrenceMap(input4)
		val expected = Seq[ImportantTextPart](
			Spaces("   "), Word("what", 1), Spaces(" "), Word("is", 3), 
			Spaces(" "), Word("up", 1), Punctuations("?"), Spaces("\n\n\t "),
			Word("this", 1), Spaces(" "), Word("is", 3), Spaces(" "), Word("a", 1),
			Spaces(" "), Word("test", 1), Punctuations("."), Spaces(" "),
			Word("It", 1), Word("IS", 3)
		)
	}
}
