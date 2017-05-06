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
		val htmlResult: Html = htmlResultFromMapping(wordToNbOccur)
		Ok(views.html.index(hostName=request.host, htmlResult))
	}

        def wordOccurenceMap(text: String): Array[(String, Int)] = {
                val wordsArray: Array[String] = text.split(" +")
                val wordsOccurences: Map[String, Int] =
                        wordsArray.groupBy(w => w.toLowerCase).mapValues(_.size)
                wordsArray.map { w =>
                        (w, wordsOccurences.getOrElse(w.toLowerCase, 0))
                }
        }

	def htmlResultFromMapping(wordToNbOccur: Array[(String, Int)]): Html = { 
		val maxNbOccurs: Int = wordToNbOccur.maxBy(_._2)._2 
		wordToNbOccur.foldLeft("") { case (str, (w, nb)) =>
			str + " ("+w+","+nb+")"
		}

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
			str + s"""<div style="color:rgb($r,$g,$b)" class="wordDiv">$w </div>"""     
		}
		
		HtmlFormat.raw(coloredText)
	}
}
