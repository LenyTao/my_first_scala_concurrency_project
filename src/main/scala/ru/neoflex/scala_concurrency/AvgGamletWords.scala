package ru.neoflex.scala_concurrency

import java.nio.file.{Files, Path}


object AvgGamletWords extends App {
  val bookSacrifice: Path = Path.of("./textGamlet/Gamlet.txt")
  val arrayOnlyWordsFromTheBook =
    Files
      .readString(bookSacrifice)
      .replaceAll("[^a-zA-Z\\s]", "")
      .split(" ")
      .filter(_ != "")

  val avg = new AvgGamlet

  countsSumOfWordsParts(5)

  val result: Double = avg.getSumWords().toDouble / avg.getWordCount().toDouble

  println("Average length of English words: " + String.format("%.1f", result) + " letters")
  result


  def countsSumOfWordsParts(numberOfParts: Int) = {
    val arrayThreads: Array[Thread] = new Array[Thread](numberOfParts)
    val partBookForThread: Array[String] = new Array[String](arrayOnlyWordsFromTheBook.length / numberOfParts)
    avg.sumWords(arrayOnlyWordsFromTheBook
      .drop(arrayOnlyWordsFromTheBook.length - arrayOnlyWordsFromTheBook.length % numberOfParts)
      .map(x => x.length)
      .sum)
    for (numberThreadWorkingWithPartText <- 0 until numberOfParts) {
      Array.copy(arrayOnlyWordsFromTheBook, numberThreadWorkingWithPartText * partBookForThread.length, partBookForThread, 0, partBookForThread.length)
      arrayThreads.update(
        numberThreadWorkingWithPartText, new Thread(s"Thread # $numberThreadWorkingWithPartText") {
          override def run(): Unit = {
            for (word <- partBookForThread) {
              avg.counterWord()
              avg.sumWords(word.length)
            }
          }
        }
      )
      arrayThreads(numberThreadWorkingWithPartText).start()
      arrayThreads(numberThreadWorkingWithPartText).join()
    }
  }

  class AvgGamlet {

    @volatile private var wordCount: Int = 0
    @volatile private var sumWord: Int = 0
    var a = true

    def counterWord(): Unit = {
      wordCount += 1
    }

    def getWordCount(): Int = {
      wordCount
    }

    def sumWords(wordLength: Int): Unit = {
      sumWord += wordLength
    }


    def getSumWords(): Int = {
      sumWord
    }
  }

}
