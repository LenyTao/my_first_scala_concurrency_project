package ru.neoflex.scala_concurrency

import java.nio.file.{Files, Path}


object AvgGamletWords extends App {
  val fileForReader: Path = Path.of("./textGamlet/Gamlet.txt")

  def readWords(): Array[String] = {
    Files
      .readString(fileForReader)
      .replaceAll("\\p{Punct}", "")
      .replaceAll("\n", " ")
      .split(" ")
      .filter(_.nonEmpty)
      .filter(_ (0).toInt != 13)
  }

  val clearGamlet = readWords()
  val avg = new AvgGamlet
  ThreadsReadingPartsOfBook(5)

  val result: Double = avg.getSumWord() / avg.getWordCount()
  println("Average length of English words: " + String.format("%.1f", result) + " letters")
  result


  def ThreadsReadingPartsOfBook(numberOfParts: Int) {
    val arrayThreads: Array[Thread] = new Array[Thread](numberOfParts)
    val partBookForThread: Array[String] = Array.fill(clearGamlet.length / numberOfParts)("")
    for (numberThreadWorkingWithPartText <- 0 until numberOfParts) {
      Array.copy(clearGamlet, numberThreadWorkingWithPartText * partBookForThread.length, partBookForThread, 0, partBookForThread.length)

       arrayThreads.update(
         numberThreadWorkingWithPartText, new Thread(s"Thread # $numberThreadWorkingWithPartText") {
           override def run(): Unit = {
             for (word <- partBookForThread) {
               avg.inkrementWord()
               avg.sumWord(word.length)
             }
           }
         }
       )
      arrayThreads(numberThreadWorkingWithPartText).start()
            Thread.sleep(10)
    }
    for (numThread <- 0 until numberOfParts) {
      arrayThreads(numThread).join()
    }
    val tailOfBook: Int = clearGamlet.length - (numberOfParts * partBookForThread.length)
    if (tailOfBook != 0) {
      clearGamlet.drop(clearGamlet.length - tailOfBook).foreach(x => avg.inkrementWord())
      clearGamlet.drop(clearGamlet.length - tailOfBook).foreach(x => avg.sumWord(x.length))
    }
  }

  class AvgGamlet {
    @volatile private var wordCount: Int = 0
    @volatile private var sumWord: Int = 0

    def inkrementWord(): Int = {
      wordCount += 1
      wordCount
    }

    def getWordCount(): Int = {
      wordCount
    }

    def sumWord(wordLength: Int): Int = {
      sumWord += wordLength
      sumWord
    }

    def getSumWord(): Double = {
      sumWord
    }
  }

}
