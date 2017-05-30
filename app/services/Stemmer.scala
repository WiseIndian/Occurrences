package services;

import utils.Stemmer;

trait StemmerUser {
	val s: Stemmer;

	def stemWord(w: String): String = {
		s.add(w.toCharArray, w.length)
		s.stem()	
		s.toString
	}
}


class ConcreteStemmerUser extends StemmerUser {
	override val s: Stemmer = new Stemmer()
}

