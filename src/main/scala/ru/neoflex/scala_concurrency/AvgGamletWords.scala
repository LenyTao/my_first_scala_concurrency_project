package ru.neoflex.scala_concurrency

import java.io.File
import java.nio.file.{Files, Path}

  trait ReaderGamlet {
    val file = Path.of("./textGamlet/Gamlet.txt")

    def readStrings(f: File): Array[String] = {
      Files.readString(f.toPath).replaceAll("\\p{Punct}", "").replaceAll("\n", " ").split(" ")
    }

    def loadStrings(): Array[String] =
      readStrings(file.toFile)
  }

object AvgGamlet extends App with ReaderGamlet {
  val firstHalfThread = new MyThread
  val secondHalfThread = new MyThread2
  firstHalfThread.start();
  secondHalfThread.start();
  firstHalfThread.join()
  secondHalfThread.join()
  val result = math.round(((firstHalfThread.avgFirstHalf + secondHalfThread.avgSecondHalf) / 2) * 100.0) / 100.0
  println("Average length of English words " + result + " letters")
  result
}

class MyThread extends Thread with ReaderGamlet {
  var wordCount = 0
  var sumWord = 0
  var avgFirstHalf: Double = 0.0

  override def run() = {
    val firstHalf = loadStrings().dropRight(loadStrings().length / 2)
    for (word <- firstHalf) {
      if (word.nonEmpty && word(0).toInt != 13) {
        println("Hello, I am First Thread, working with the word: " + word.trim)
        wordCount += 1
        sumWord = sumWord + word.length
      }
    }
    avgFirstHalf = sumWord.toDouble / wordCount.toDouble
  }
}

class MyThread2 extends Thread with ReaderGamlet {
  var wordCount = 0
  var sumWord = 0
  var avgSecondHalf: Double = 0.0

  override def run() = {
    val secondHalf = loadStrings().drop(loadStrings().length / 2)
    for (word <- secondHalf) {
      if (word.nonEmpty && word(0).toInt != 13) {
        println("Hello, I am Second Thread, working with the word: " + word.trim)
        wordCount += 1
        sumWord = sumWord + word.length
      }
    }
    avgSecondHalf = sumWord.toDouble / wordCount.toDouble
  }
}
