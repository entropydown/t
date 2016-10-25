package com.fun

import java.io.StringReader

import com.fun.model.{Gender, PassengerClass, PortOfEmbarkation, TrainingRow}
import org.apache.commons.csv.CSVFormat
import scala.collection.JavaConversions._

import scala.io.Source

object TrainingDataLoader {
  protected def processAge(input: String): Option[Double] = input match {
    case "" => None
    case _ => Some(input.toDouble)
  }

  protected def processCabin(input: String) = input match {
    case "" => None
    case _ => Some(input)
  }

  protected def processSurvival(input: String) = input match {
    case "0" => false
    case "1" => true
  }

  protected def processFare(input: String) = input match {
    case "" => 0.0
    case a => a.toDouble
  }

  // To train and verify settings for the stanford classifier, the kaggle "train" data is split into two parts.
  // one is called train and the other is called test.csv
  def trainingData = getData("train.kaggle.csv")
  def testData = getData("test.csv")

  def realTestData = getRealTestData("test.kaggle.csv")

  // The kaggle "test" data is missing the "survived" row. So process the kaggle test data assuming the survived row
  // is missing
  protected def getData(file: String): Seq[TrainingRow] = {
    val rawCsv = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(file)).getLines.toList.tail.mkString("\n")
    val parser = CSVFormat.DEFAULT.withRecordSeparator("\n").parse(new StringReader(rawCsv))

    parser.getRecords.map { field =>
      TrainingRow(passengerId = field.get(0).toInt,
        survived = Some(processSurvival(field.get(1))),
        passengerClass = PassengerClass.to(field.get(2)),
        name = field.get(3),
        sex = Gender.to(field.get(4)),
        age = processAge(field.get(5)),
        numberOfSiblingsSpouses = field.get(6).toInt,
        numberOfParentsChildren = field.get(7).toInt,
        ticketNumber = field.get(8),
        fare = field.get(9).toDouble,
        cabin = processCabin(field.get(10)),
        portOfEmbarkation = PortOfEmbarkation.to(field.get(0), field.get(11)))
    }
  }

  protected def getRealTestData(file: String): Seq[TrainingRow] = {
    val rawCsv = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(file)).getLines.toList.tail.mkString("\n")
    val parser = CSVFormat.DEFAULT.withRecordSeparator("\n").parse(new StringReader(rawCsv))

    parser.getRecords.map { field =>
      TrainingRow(passengerId = field.get(0).toInt,
        survived = None,
        passengerClass = PassengerClass.to(field.get(1)),
        name = field.get(2),
        sex = Gender.to(field.get(3)),
        age = processAge(field.get(4)),
        numberOfSiblingsSpouses = field.get(5).toInt,
        numberOfParentsChildren = field.get(6).toInt,
        ticketNumber = field.get(7),
        fare = processFare(field.get(8)),
        cabin = processCabin(field.get(9)),
        portOfEmbarkation = PortOfEmbarkation.to(field.get(0), field.get(10)))
    }
  }
}
