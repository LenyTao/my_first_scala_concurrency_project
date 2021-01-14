package ru.neoflex.scala_concurrency

import java.nio.file.{Files, Path}
import java.util.concurrent._


object AvgGamletWords extends App {

  val semaphoreForControlTreatmentPartBook = new Semaphore(1)
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


  def countsSumOfWordsParts(amountOfParts: Int) = {
    val arrayPartsOfBook = new Array[Array[String]](amountOfParts)
    val arrayThreads: Array[Thread] = new Array[Thread](amountOfParts)
    @volatile var partBookForThread: Array[String] = new Array[String](arrayOnlyWordsFromTheBook.length / amountOfParts)

    for (numberPart <- 0 until amountOfParts) {

      if (numberPart != amountOfParts - 1) {
        Array.copy(arrayOnlyWordsFromTheBook, numberPart * partBookForThread.length, partBookForThread, 0, partBookForThread.length)
      }

      else if (arrayOnlyWordsFromTheBook.length % amountOfParts != 0) {
        partBookForThread = arrayOnlyWordsFromTheBook.drop(arrayOnlyWordsFromTheBook.length - partBookForThread.length - (arrayOnlyWordsFromTheBook.length % amountOfParts))
      }

      else {
        Array.copy(arrayOnlyWordsFromTheBook, numberPart * partBookForThread.length, partBookForThread, 0, partBookForThread.length)
      }
      arrayPartsOfBook.update(numberPart, partBookForThread.toArray)
    }

    for (numberPartForThread <- arrayPartsOfBook.indices) {
      arrayThreads.update(numberPartForThread, new ThreadWorkingWithPartOfBook(arrayPartsOfBook(numberPartForThread)))
      arrayThreads(numberPartForThread).start()
    }
    arrayThreads.foreach(x => x.join())
  }

  class ThreadWorkingWithPartOfBook(partOfBook: Array[String]) extends Thread {
    override def run(): Unit = {
      for (word <- partOfBook) {
        avg.sumWords(word.length)
        avg.counterWord()
      }
    }
  }

  class AvgGamlet {
    @volatile private var wordCount: Int = 0
    @volatile private var sumWords: Int = 0

    def counterWord(): Unit = {
      semaphoreForControlTreatmentPartBook.acquire()
      wordCount += 1
      semaphoreForControlTreatmentPartBook.release()
    }

    def getWordCount(): Int = {
      wordCount
    }

    def sumWords(wordLength: Int): Unit = {
      semaphoreForControlTreatmentPartBook.acquire()
      sumWords += wordLength
      semaphoreForControlTreatmentPartBook.release()
    }

    def getSumWords(): Int = {
      sumWords
    }
  }

}
