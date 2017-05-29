package services

import javax.inject._
import java.nio.charset.Charset
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.lang.InterruptedException
import java.lang.NullPointerException
import java.lang.IllegalArgumentException
import java.lang.ClassCastException



//a Runnable class writing an inputText to a certain file
object WriteUtils {
        def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
                val fw = new java.io.FileOutputStream(f, true) // true allows appending data to current file content
		val osw = new java.io.OutputStreamWriter(
				fw, 
				Charset.forName("UTF-8").newEncoder()
		)
                val bw = new java.io.BufferedWriter(osw)
                val p = new java.io.PrintWriter(bw)
                try { op(p) } finally { p.close() }
        }
}

/* 
 * using the consumer producer concurrent design pattern described at 
 * http://javarevisited.blogspot.ch/2012/02/producer-consumer-design-pattern-with.html
 */
class Consumer(val sharedQueue: BlockingQueue[String], filename: String) extends Runnable{
	import WriteUtils._

	val file = new java.io.File(filename)	

	def println(str: String) = printToFile(file)(pw => pw.println(str))

	override def run() {
		while(true){
			try {
				val input: String = 
					sharedQueue.take()
				println(input)
			} catch {
				case e: InterruptedException => 
					System.err.println(e)
			}
		}
	}
}

trait Logger {
	def log(inputText: String): Unit
}

/**
 * This class has a `Singleton` annotation because we need to make
 * sure we only use one logger per application. Without this
 * annotation we would get a new instance every time a [[Logger]] is
 * injected.
 */
@Singleton
class ConcurrentSingleFileLogger() extends Logger {  
	val sharedQueue: BlockingQueue[String] = new LinkedBlockingQueue[String]()

	new Thread(new Consumer(sharedQueue, filename="logFile")).start()

	override def log(inputText: String) {
		try {
			sharedQueue.put(inputText)
		} catch {
			case e: InterruptedException => 
				System.err.println(e)
			case e: NullPointerException => 
				System.err.println(e)
			case e: ClassCastException => 
				System.err.println(e)
			case e: IllegalArgumentException => 
				System.err.println(e)
		}
	}
}
