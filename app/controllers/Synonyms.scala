package controllers


import java.sql.{PreparedStatement, ResultSet, Connection}

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import services.Logger

import play.api.db._

//TODO put that in the model package
case class Synset(id: Int, definition: String)
/**
 * This controller creates an `Action` to handle POST HTTP requests 
 * for synonyms of a word. 
 */
@Singleton
class Synonyms @Inject()(logger: Logger, db: Database) extends Controller {

	val selectSynsetForWordStr: String =
		s"""
		select synsetid from senses where wordid=(
			select wordid from words where lemma= ?
		)
		"""

	val selectSynsetDefinionStr: String = 
		s"""
		select definition from synsets where synsetid=?
		"""

	val wordsOfSynsetQueryStr = 
		s"""
		select lemma 
		from words, (select wordid from senses where synsetid=?) synwordids 
		where synwordids.wordid=words.wordid AND lemma != ?
		"""

	/*
	 *execute wordsOfSynsetQueryStr on database connection and return a comma separated string 
	 *of words contained in the synset but that are not word */
	def wordsOfSynsetAsJsonFromConn(conn: Connection, word: String)(s: Synset): String = {
		val selectLemmasOfSynset: PreparedStatement =
			conn.prepareStatement(wordsOfSynsetQueryStr)

		selectLemmasOfSynset.setInt(1, s.id)
		selectLemmasOfSynset.setString(2, word)

		val rs: ResultSet = 
			selectLemmasOfSynset.executeQuery()

		def lemmasStrm: Stream[String] =
			if (rs.next()) rs.getString("lemma") #:: lemmasStrm
			else Stream.empty[String]

		lemmasStrm
		.map(l => s""""$l"""" )
		.mkString(",")
	}


	def synonyms() = Action(BodyParsers.parse.json) { request => 
		val word: String = (request.body \ "word").as[String]

		val conn: Connection = db.getConnection()

		val outString: String = 
		
		try {
			val selectSynsetForWord: PreparedStatement = 
				conn
				.prepareStatement(selectSynsetForWordStr)

			selectSynsetForWord.setString(1, word)
	
			val rsSynsets: ResultSet = selectSynsetForWord.executeQuery()

			val selectSynsetDefinition: PreparedStatement = 
				conn.prepareStatement(selectSynsetDefinionStr)

			def synsetStrm: Stream[Synset] = 
				if (rsSynsets.next()) {
					val sid = rsSynsets.getInt("synsetid")
					selectSynsetDefinition.setInt(1, sid)
					val rs: ResultSet = 
						selectSynsetDefinition
						.executeQuery()
					val defin: String = 
						if (rs.next()) rs.getString("definition")
						else "error could not get definition for this set of synonyms" 
					Synset(sid, defin) #:: synsetStrm 
				} else {
					Stream.empty[Synset]
				}
			


			def selectLemmasOfSynset(s: Synset): String = wordsOfSynsetAsJsonFromConn(conn, word)(s)
				
			synsetStrm	
			.flatMap { case synset @ Synset(sid, defin) => 

				val synonymsOfWord = selectLemmasOfSynset(synset)
				if (!synonymsOfWord.isEmpty) Some(
					s"""
					{
						"sense": "$defin",
						"synonynms": [${synonymsOfWord}]
					}
					"""
				) else None			
			}
			.mkString("{\"result\": [", "," ,"]}")
			
		} finally {
			conn.close()
		}

		Ok(outString)	
	}
}
