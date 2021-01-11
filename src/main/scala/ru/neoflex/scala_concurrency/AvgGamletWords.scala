package ru.neoflex.scala_concurrency

import java.nio.file.{Files, Path}


object AvgGamletWords extends App {
  val avg = new AvgGamlet
 new ThreadsReadingPartsOfBook(3)

  val result = avg.getSumWord() / avg.getWordCount()
  println("Average length of English words: " + String.format("%.1f",result) + " letters")
  result

  trait ReaderGamlet {
    val fileForReader: Path = Path.of("./textGamlet/Gamlet.txt")

    def readWords(): Array[String] = {
      Files.readString(fileForReader).replaceAll("\\p{Punct}", "").replaceAll("\n", " ").split(" ").filter(_.nonEmpty).filter(_ (0).toInt != 13)
    }
  }

  class ThreadsReadingPartsOfBook(part: Int) extends ReaderGamlet {
    val arrayThreads: Array[Thread] = new Array[Thread](part)
    val partBookForThread: Array[String] = Array.fill(readWords().length / part)("")
    for (numberThreadWorkingWithPartText <- 0 until part) {
      Array.copy(readWords(), numberThreadWorkingWithPartText * partBookForThread.length, partBookForThread, 0, partBookForThread.length)
      arrayThreads.update(
        numberThreadWorkingWithPartText, new Thread(s"Thread # $numberThreadWorkingWithPartText") {
          override def run(): Unit = {
            for (word <- partBookForThread) {
              avg.inkrementWord()
              avg.sumWord(word.length.toDouble)
            }
          }
        }
      )
        arrayThreads(numberThreadWorkingWithPartText).start()
      Thread.sleep(1000)
    }
    for (numThread <- 0 until part) {
      arrayThreads(numThread).join()
    }
    val tailOfBook: Int = readWords().length - (part * partBookForThread.length)
    if (tailOfBook != 0) {
      readWords().drop(readWords().length - tailOfBook).foreach(x => avg.inkrementWord())
      readWords().drop(readWords().length - tailOfBook).foreach(x => avg.sumWord(x.length.toDouble))
    }
  }

  class AvgGamlet extends ReaderGamlet {
    @volatile private var wordCount: Int = 0
    @volatile private var sumWord: Double = 0.0

    def inkrementWord(): Unit = {
      wordCount += 1
    }

    def getWordCount(): Double = {
      wordCount.toDouble
    }

    def sumWord(wordLength: Double): Unit = {
      sumWord += wordLength
    }

    def getSumWord(): Double = {
      sumWord
    }
  }
}