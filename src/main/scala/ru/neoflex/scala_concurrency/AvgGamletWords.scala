package ru.neoflex.scala_concurrency

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger


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
    val arrayThreads: Array[Thread] = new Array[Thread](numberOfParts + math.ceil(arrayOnlyWordsFromTheBook.length % numberOfParts / (arrayOnlyWordsFromTheBook.length % numberOfParts + 1).toDouble).toInt)
    val partBookForThread = arrayOnlyWordsFromTheBook
      .sliding(arrayOnlyWordsFromTheBook.length / numberOfParts, arrayOnlyWordsFromTheBook.length / numberOfParts)
      .toList
    partBookForThread.indices.foreach(x => arrayThreads.update(x, new ThreadWorkingWithWords(partBookForThread(x))))
    arrayThreads.foreach(x => x.start())
    arrayThreads.foreach(x => x.join())
  }

  class ThreadWorkingWithWords(partOfBook: Array[String]) extends Thread {
    override def run(): Unit = {
      partOfBook.foreach(x => avg.countAndSumWords(x.length))
    }
  }

  class AvgGamlet {
    private val sumWords: AtomicInteger = new AtomicInteger()
    private val wordCount: AtomicInteger = new AtomicInteger()

    def getWordCount(): Int = {
      wordCount.get()
    }

    def getSumWords(): Int = {
      sumWords.get()
    }

    def countAndSumWords(wordLength: Int): Unit = {
      wordCount.addAndGet(1)
      sumWords.addAndGet(wordLength)
    }
  }

}
