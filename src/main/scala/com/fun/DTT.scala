package com.fun

import java.io.PrintWriter
import java.util.Properties

import com.fun.model.TrainingRow
import edu.stanford.nlp.classify.{ColumnDataClassifier, Dataset, GeneralDataset}
import edu.stanford.nlp.ling.{BasicDatum, RVFDatum}
import edu.stanford.nlp.stats.IntCounter

import scala.collection.JavaConversions._

object Main {
  def main(args: Array[String]) {
    val rawTrainingData = TrainingDataLoader.trainingData
    val rawTestData = TrainingDataLoader.testData
    val realTestData = TrainingDataLoader.realTestData

    val props = new Properties()
    props.setProperty("sigma", "3")

    val cdc = new ColumnDataClassifier(props)

    val trainingData = new Dataset[String, String]()
    val testData = new Dataset[String, String]()

    def extractFeaturesFromTrainingRow(trainingRow: TrainingRow): BasicDatum[String, String] = {
      new BasicDatum[String, String](
        Seq(trainingRow.sex.toString,
            trainingRow.passengerClass.toString,
            trainingRow.fare.toString,
        trainingRow.survived.map(_.toString).getOrElse(null))
    }

    def rvDatumFor[T](value: T, survived: Option[Boolean]): RVFDatum[String, String] = {
      val counter = new IntCounter[String]()
      counter.incrementCount(value.toString)
      new RVFDatum[String, String](counter, survived.map(_.toString).getOrElse(null))
    }

    rawTrainingData.foreach { rawTrainingDatum =>
      trainingData.add(extractFeaturesFromTrainingRow(rawTrainingDatum))
    }

    rawTestData.foreach { rawTestDatum =>
      testData.add(extractFeaturesFromTrainingRow(rawTestDatum))
    }

    val classifier = cdc.makeClassifier(trainingData)

    println(s"training: \n${trainingData.toSummaryStatistics()}")
    println(s"test: \n${testData.toSummaryStatistics()}")
    println(s"accuracy ${classifier.evaluateAccuracy(testData)}")

    def labelToOutput(label: String) = label match {
      case "true" => "1"
      case "false" => "0"
    }

    val output = new PrintWriter("/Users/entropy/src/t/output.csv")

    output.println("PassengerId,Survived")

    realTestData.foreach { rawTestDatum =>
      val datum = new BasicDatum[String, String]
      output.println(s"${rawTestDatum.passengerId},${labelToOutput(classifier.classOf(extractFeaturesFromTrainingRow(rawTestDatum)))}")
    }

    output.flush()
    output.close()
  }
}