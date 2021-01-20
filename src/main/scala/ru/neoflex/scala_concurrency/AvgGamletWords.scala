package ru.neoflex.scala_concurrency

import java.nio.file.{Files, Path}

object AvgGamletWords extends App {

  val bookSacrifice: Path = Path.of("./textGamlet/Gamlet.txt")
  val arrayOnlyWordsFromTheBook = Files
    .readString(bookSacrifice)
    .replaceAll("[^a-zA-Z\\s]", "")
    .replaceAll("[\\r\\n\\a\\t\\v\\f\\e]", "")
    .split(" ")
    .filter(_ != "")

  val avg = new AvgGamlet
  countsSumOfWordsParts(5)
  val result: Double = avg.getSumWords().toDouble / avg.getWordCount().toDouble
  println("Average length of English words: " + String.format("%.1f", result) + " letters")
  result

  def countsSumOfWordsParts(numberOfParts: Int): Unit = {
    val partBookForThread = arrayOnlyWordsFromTheBook
      .sliding(arrayOnlyWordsFromTheBook.length / numberOfParts, arrayOnlyWordsFromTheBook.length / numberOfParts)
      .toList
    val listThreads: List[Thread] = partBookForThread.map(x => new ThreadCalculateSumOfWords(x))
    listThreads.foreach(x => x.start())
    listThreads.foreach(x => x.join())
  }

  class ThreadCalculateSumOfWords(partOfBook: Array[String]) extends Thread {
    val avgInsideClass = new AvgGamlet

    override def run(): Unit = {
      partOfBook.foreach(x => avgInsideClass.countAndSumWordsInsideThread(x.length))
      avg.countAndSumWords(avgInsideClass.getSumWords(), avgInsideClass.getWordCount())
    }
  }

  class AvgGamlet {
    @volatile private var wordCount: Int = 0
    @volatile private var sumWords: Int = 0

    def getWordCount(): Int = {
      wordCount
    }

    def getSumWords(): Int = {
      sumWords
    }

    def countAndSumWords(wordLength: Int, wordCounter: Int): Unit = {
      synchronized {
        wordCount += wordCounter
        sumWords += wordLength
      }
    }

    def countAndSumWordsInsideThread(wordLength: Int): Unit = {
      wordCount += 1
      sumWords += wordLength
    }

  }

}
