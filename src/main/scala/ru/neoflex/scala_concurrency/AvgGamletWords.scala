package ru.neoflex.scala_concurrency

import java.nio.file.{Files, Path}
import java.util.concurrent._


object AvgGamletWords extends App {
  val semaphoreForControlTreatmentPartBook = new Semaphore(1)
  val bookSacrifice: Path = Path.of("./textGamlet/Gamlet.txt")
  val arrayOnlyWordsFromTheBook = Files
    .readString(bookSacrifice)
    .replaceAll("[^a-zA-Z\\s]", "").replaceAll("[\\r\\n]", "")
    .split(" ")
    .filter(_ != "")
  val avg = new AvgGamlet
  countsSumOfWordsParts(5)
  val result: Double = avg.getSumWords().toDouble / avg.getWordCount().toDouble
  println("Average length of English words: " + String.format("%.1f", result) + " letters")
  result


  def countsSumOfWordsParts(numberOfParts: Int) = {

    val arrayThreads: Array[Thread] = new Array[Thread](numberOfParts)
    @volatile var partBookForThread: Array[String] = new Array[String](arrayOnlyWordsFromTheBook.length / numberOfParts)

    for (numberThreadWorkingWithPartText <- 0 until numberOfParts) {

      if (numberThreadWorkingWithPartText != numberOfParts - 1) {
        Array.copy(arrayOnlyWordsFromTheBook, numberThreadWorkingWithPartText * partBookForThread.length, partBookForThread, 0, partBookForThread.length)
      }

      else if (arrayOnlyWordsFromTheBook.length % numberOfParts != 0) {
        arrayThreads.dropRight(1).foreach(x => x.join())
        partBookForThread = arrayOnlyWordsFromTheBook.drop(arrayOnlyWordsFromTheBook.length - partBookForThread.length - (arrayOnlyWordsFromTheBook.length % numberOfParts))
      }
      else {
        arrayThreads.dropRight(1).foreach(x => x.join())
        Array.copy(arrayOnlyWordsFromTheBook, numberThreadWorkingWithPartText * partBookForThread.length, partBookForThread, 0, partBookForThread.length)
      }

      arrayThreads.update(
        numberThreadWorkingWithPartText, new Thread(s"Thread # $numberThreadWorkingWithPartText") {

          override def run(): Unit = {
            semaphoreForControlTreatmentPartBook.acquire()
            for (word <- partBookForThread) {
              avg.sumWords(word.length)
              avg.counterWord()
            }
            semaphoreForControlTreatmentPartBook.release()
          }
        }
      )
      arrayThreads(numberThreadWorkingWithPartText).start()
      Thread.sleep(10)
    }
    arrayThreads(arrayThreads.length - 1).join()
  }

  class AvgGamlet {
    @volatile private var wordCount: Int = 0
    @volatile private var sumWords: Int = 0

    def counterWord(): Unit = {
      wordCount += 1
    }

    def getWordCount(): Int = {
      wordCount
    }

    def sumWords(wordLength: Int): Unit = {
      sumWords += wordLength
    }

    def getSumWords(): Int = {
      sumWords
    }
  }
}
