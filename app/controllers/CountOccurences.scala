package controllers

import scala.collection.mutable.Buffer
import javax.inject._
import play.api._
import play.api.mvc._
import play.twirl.api._
import play.api.libs.json._
import java.awt.Color


trait ImportantTextPart {
	def content: String
}
case class Word (val content: String) extends ImportantTextPart 
case class Punctuation (val content: String) extends ImportantTextPart 
//this can be used for whatever substring that doesn't contain any Punctuation
case class Other(val content: String) extends ImportantTextPart

case class InputText(text: String)
/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class CountOccurences @Inject() extends Controller {
	implicit val InputTextReads: Reads[InputText] =
		Json.reads[InputText]

	def occurs() = Action(BodyParsers.parse.json) { request => 
		val textResult: JsResult[InputText] = 
			request.body.validate[InputText]

		val textOpt: Option[String] = textResult.asOpt.map(_.text)
		val wordToNbOccur: Seq[(ImportantTextPart, Int)] =
			textOpt	
			.map (t => wordOccurenceMap(t))
			.getOrElse {
				System.err.println("wrong json input")
				Seq[(ImportantTextPart, Int)]()
			}
		val htmlResult: String = htmlResultFromMapping(wordToNbOccur)

		Ok(htmlResult)
	}

	def homepage() = Action { request => 
		Ok(views.html.index(hostName=request.host))
	}


	//this function does the following on the following inputs:
	/** s.equals("i am a man")
	* output: Other("i am a man") :: Nil
	*
	* s.equals("hello world! this is me mario!!")
	*output: Other("hello world") :: Punctuation("!") :: Other(" this is me mario") :: Punctuation("!") :: Punctuation("!") :: Nil
	*/
	
	def separatePunctuation(s: String): Seq[ImportantTextPart] =  {
		var lastSeenIndex = -1;
		var result = Buffer[ImportantTextPart]()
		(0 until s.length).foreach { i => 
			if (s.substring(i,i+1).matches("\\p{Punct}")) {
				if ((i-1) - lastSeenIndex > 0)
					result += Other(s.substring(lastSeenIndex+1, i))
				result += Punctuation(s.substring(i, i+1))
				lastSeenIndex = i
			} else if (i == s.length-1) {
				result += Other(s.substring(lastSeenIndex+1, s.length))
			}
		}

		result
	}


        def wordOccurenceMap(text: String): Seq[(ImportantTextPart, Int)] = {
		val wordsAndPunctuation: Seq[ImportantTextPart] = 
			separatePunctuation(text).flatMap {
				case p: Punctuation => 
					Array(p).toSeq
				case Other(content) => 
					val words = content.split("\\s+")
					words.map(Word(_)).toSeq
			}

                val wordsOccurences: Map[String, Int] =
                        wordsAndPunctuation
			.groupBy(w => w.content.toLowerCase)
			.mapValues(_.size)
                wordsAndPunctuation.map { w =>
                        (w, wordsOccurences.getOrElse(w.content.toLowerCase, 0))
                }
        }

	def colorForWord(nbOccur: Int, maxNbOccurs: Int): String = {
		val green: Float = 0.2f
		val greenRatio = 
			(maxNbOccurs - (nbOccur-1)).toFloat / maxNbOccurs
		val h = greenRatio * green
		//finding the word color from the hue and arbitrary brightness and saturation parameter 
		val col = Color.getHSBColor(h, 1f, 0.5f)
		val r = col.getRed()
		val g = col.getGreen()
		val b = col.getBlue()
		s"""color:rgb($r,$g,$b)"""
	}
	/*this function builds the resulting coloured text from
	* an array of the words of the text with the number of time they occur.
	*/
	def htmlResultFromMapping(wordsToNbOccur: Seq[(ImportantTextPart, Int)]): String = { 
		val maxNbOccurs: Int = wordsToNbOccur.maxBy(_._2)._2 

		wordsToNbOccur.foldLeft("") { case (str, (w,nb)) =>
			//computing the hue for each word
			val strRgb = w match {
				case Word(_) => colorForWord(nb, maxNbOccurs)
				case Punctuation(_) => "color:rgb(0,0,0)"
			}

			str + 
				s"""
				<div class="wordInfo">
					<div style="$strRgb" class="wordDiv">
						${w.content}
					</div>
					<div class="nbOcc coolBorder">
						<div style="$strRgb">$nb occurences</div>
					</div>
				</div>"""     
		}
	}
}
