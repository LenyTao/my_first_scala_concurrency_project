package ru.neoflex.scala_concurrency

import java.nio.file.{Files, Path}

trait ReaderGamlet {
  val fileForReader: Path = Path.of("./textGamlet/Gamlet.txt")

  def readWords(): Array[String] = {
    Files.readString(fileForReader).replaceAll("\\p{Punct}", "").replaceAll("\n", " ").split(" ")
  }
}

object AvgGamlet extends App with ReaderGamlet {
  val firstHalfThread = new ThreadReadingHalfOfBook(1)
  val secondHalfThread = new ThreadReadingHalfOfBook(2)
  firstHalfThread.start()
  secondHalfThread.start()
  firstHalfThread.join()
  secondHalfThread.join()

  val result = (firstHalfThread.getResultAvgHalf() + secondHalfThread.getResultAvgHalf()) / 2.0
  println("Average length of English words: " + result + " letters")
  result
}

class ThreadReadingHalfOfBook(half: Int) extends Thread with ReaderGamlet {
  private val halfOfBook: Array[String] = {
    if (half == 1) {
      readWords().dropRight(readWords().length / 2)
    }
    else {
      readWords().drop(readWords().length / 2)
    }
  }
  private val wordCount: Array[Double] = Array(0.0)
  private val sumWord: Array[Double] = Array(0.0)
  private val avgHalf: Array[Double] = Array(0.0)

  override def run(): Unit = {
    for (word <- halfOfBook) {
      if (word.nonEmpty && word(0).toInt != 13) {
        println(s"Hello, I am $half Thread, working with the word: " + word.trim)
        wordCount.update(0, wordCount(0) + 1.0)
        sumWord.update(0, sumWord(0) + word.length)
      }
    }
    avgHalf.update(0, sumWord(0) / wordCount(0))
  }

  def getResultAvgHalf(): Double = {
    avgHalf(0)
  }
}
