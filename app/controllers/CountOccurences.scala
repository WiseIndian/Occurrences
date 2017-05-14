package controllers


import util.parsing.combinator.RegexParsers
import scala.collection.mutable.Buffer
import javax.inject._
import play.api._
import play.api.mvc._
import play.twirl.api._
import play.api.libs.json._
import scala.io.Source
import java.awt.Color
import services.Logger


trait ImportantTextPart {
	def content: String
}
case class Word (val content: String, nbOccur: Int) extends ImportantTextPart 
case class Punctuations (val content: String) extends ImportantTextPart 
case class Spaces(val content: String) extends ImportantTextPart

case class InputText(text: String)



class TextParser extends RegexParsers {
	override def skipWhitespace = false 

	lazy val spacesParser1: Parser[Spaces] = "\\s+".r ^^ { s => Spaces(s) }
	lazy val punctuation: Parser[Punctuations] = "\\p{Punct}+".r ^^ { s => Punctuations(s) }
	lazy val word: Parser[Word] = "[^\\s\\p{Punct}]+".r ^^ { s => Word(s, 1) }
	lazy val token: Parser[ImportantTextPart] = spacesParser1 ||| word ||| punctuation
	lazy val textParser: Parser[List[ImportantTextPart]] = rep(token)

        def wordOccurrenceMap(text: String): Seq[ImportantTextPart] = {
		val parts: List[ImportantTextPart] = parseAll(textParser, text) match {
			case Success(e, _) => 
				e
			case f: NoSuccess => 
				System.err.println(f)
				List[ImportantTextPart]()	
		}

		//the different words to the number of their occurrences
                val wordsOccurrences: Map[String, Int] =
			parts
			.filter{ 
				case w: Word => true 
				case _ => false
			}
			.groupBy(w => w.content.toLowerCase)
			.mapValues(_.size)

                parts.map { 
			case Word(content, _) =>
				val wNbOcc = 
					wordsOccurrences
					.getOrElse(content.toLowerCase, 0)
				Word(content, wNbOcc)
			case other => other
                }
        }

	def colorForWord(nbOccur: Int, maxNbOccurs: Int): Color = {
		val green: Float = 0.2f
		val greenRatio = 
			(maxNbOccurs - (nbOccur-1)).toFloat / maxNbOccurs
		val h = greenRatio * green
		//finding the word color from the hue and arbitrary brightness and saturation parameter 
		Color.getHSBColor(h, 1f, .9f)
	}


	def wordInfoDiv(w: Word, wordColor: Color): String = {
		val r = wordColor.getRed()
		val g = wordColor.getGreen()
		val b = wordColor.getBlue()
		val strRgb = s"""color:rgb($r,$g,$b)"""

		s"""
		<div class="wordInfo">
			<div style="$strRgb" class="wordDiv">
				${w.content}
			</div>
			<div class="nbOcc coolBorder">
				<div style="$strRgb">${w.nbOccur} occurences</div>
			</div>
		</div>"""
	}

	def convertNewlinesToBr(ls: List[String]): String =  
		ls.foldLeft(""){ case (str,_) => str+"<br>" }
	def convertTabs(ls: List[String]): String =
		ls.foldLeft(""){ case(str,_) => str+"&nbsp&nbsp&nbsp&nbsp&nbsp" }
	def convertWhiteSpaces(ls: List[String]): String = 
		ls.foldLeft("") { case(str,_) => str + "&nbsp" }
	//could do a parser for vertical tab but who cares about this character?	
	def convertFormFeed(ls: List[String]): String = 
		ls.foldLeft("") { case (str,_) => str + "<br><br>" }

	lazy val lf: Parser[String] = rep1("\n") ^^ { convertNewlinesToBr }
	lazy val crlf: Parser[String] = rep1("\r\n") ^^ { convertNewlinesToBr }
	lazy val cr: Parser[String] = rep1("\r") ^^ { convertNewlinesToBr } // for old macintoshs
	lazy val tabs: Parser[String] = rep1("\t") ^^ { convertTabs }
	lazy val whiteSpaces: Parser[String] = rep1(" ") ^^ { convertWhiteSpaces }
	lazy val formFeeds: Parser[String] = rep1("\f") ^^ { convertFormFeed }
	
	lazy val spacesParser2: Parser[String] = 
		rep1(lf ||| crlf ||| cr ||| tabs ||| whiteSpaces ||| formFeeds) ^^ {
			ls => ls.mkString
		}


	def whitespaceConverter(s: String): String = {
		parseAll(spacesParser2, s) match {
			case Success(e, rest) => 
				e
			case f: NoSuccess => 
				System.err.println(f)
				""
		}
	}

	/*this function builds the resulting coloured text from
	* an array of the words of the text with the number of time they occur.
	*/
	def htmlResultFromMapping(tokens: Seq[ImportantTextPart]): String = { 
		val maxNbOccurs: Int = 
			if (tokens.isEmpty) 
				0
			else 
				tokens	
				.flatMap{
					case w: Word => Some(w.nbOccur)
					case _ => None
				}.max

		val black = "color:rgb(0,0,0)"

		tokens.foldLeft("") { 
			case (str, w @ Word(_, nbOccur)) => 
				val col = colorForWord(nbOccur, maxNbOccurs)
				str + wordInfoDiv(w, col)
			case (str, Punctuations(content)) => 
				s"""$str<div style="$black; display:inline;">$content</div>"""
			case (str, Spaces(content)) => 
				str + whitespaceConverter(content)
		}
	}
}

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class CountOccurences @Inject()(logger: Logger) extends Controller {
	implicit val InputTextReads: Reads[InputText] =
		Json.reads[InputText]

	val textParser: TextParser = new TextParser()

	def occurs() = Action(BodyParsers.parse.json) { request => 
		val textResult: JsResult[InputText] = 
			request.body.validate[InputText]

		val textOpt: Option[String] = textResult.asOpt.map(_.text)

		//logging text
		textOpt.foreach { t =>
			logger.log(t, "logFile")
		}

		textOpt	
		.map { t =>
			val wordToNbOccur = textParser.wordOccurrenceMap(t)
			val htmlResult = textParser.htmlResultFromMapping(wordToNbOccur)
			Ok(htmlResult)
		}
		.getOrElse {
			Ok("")	
		}
	}

	def homepage() = Action { request => 
		Ok(views.html.index(hostName=request.host))
	}


}
