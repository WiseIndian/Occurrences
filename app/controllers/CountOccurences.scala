package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.twirl.api._
import java.awt.Color


/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class CountOccurences @Inject() extends Controller {
	def occurs(inptText: Option[String]) = Action { request => 
		val wordToNbOccur: Array[(String, Int)] =
			inptText
			.map(wordOccurenceMap)
			.getOrElse(Array[(String,Int)]())
		val htmlResultStr: Html = htmlResultFromMapping(wordToNbOccur)
		Ok(views.html.index(hostName=request.host, htmlResultStr, inptText.getOrElse("")))
	}

        def wordOccurenceMap(text: String): Array[(String, Int)] = {
                val wordsArray: Array[String] = text.split("\\s+")
                val wordsOccurences: Map[String, Int] =
                        wordsArray.groupBy(w => w.toLowerCase).mapValues(_.size)
                wordsArray.map { w =>
                        (w, wordsOccurences.getOrElse(w.toLowerCase, 0))
                }
        }

	/*this function builds the resulting coloured text from
	* an array of the words of the text with the number of time they occur.
	*/
	def htmlResultFromMapping(wordToNbOccur: Array[(String, Int)]): Html = { 
		val maxNbOccurs: Int = wordToNbOccur.maxBy(_._2)._2 
		val green: Float = 0.2f
		val coloredText = wordToNbOccur.foldLeft("") { case (str, (w,nb)) =>
			//computing the hue for each word
			val greenRatio = 
				(maxNbOccurs - (nb-1)).toFloat / maxNbOccurs
			val h = greenRatio * green
			//finding the word color from the hue and arbitrary brightness and saturation parameter 
			val col = Color.getHSBColor(h, 1f, 0.5f)
			val r = col.getRed()
			val g = col.getGreen()
			val b = col.getBlue()
			val strRgb = s"""color:rgb($r,$g,$b)"""
			str + 
				s"""
				<div class="wordInfo">
					<div style="$strRgb" class="wordDiv">
						$w 
					</div>
					<div class="nbOcc coolBorder">
						<div style="$strRgb">$nb occurences</div>
					</div>
				</div>"""     
		}
		
		HtmlFormat.raw(coloredText)
	}
}
