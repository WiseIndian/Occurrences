package services

import javax.inject._
import java.util.concurrent.ExecutorService
import java.nio.charset.Charset


//a Runnable class writing an inputText to a certain file
class WriterJob(val inputText: String, val fileName: String) extends java.util.concurrent.Callable[Unit] {
        def call() {
                printToFile(new java.io.File(fileName))(_.println("r=> "+inputText))
        }

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

trait Logger {
	val threadPool: ExecutorService

	def log(inputText: String, fileName: String) {
		threadPool.submit(new WriterJob(inputText, fileName))
	}
}
/**
 * This class has a `Singleton` annotation because we need to make
 * sure we only use one logger per application. Without this
 * annotation we would get a new instance every time a [[Logger]] is
 * injected.
 */
@Singleton
class ConcreteLogger extends Logger {  
	override val threadPool: ExecutorService =
		java.util.concurrent.Executors.newFixedThreadPool(20)
}
