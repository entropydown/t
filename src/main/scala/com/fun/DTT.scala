package com.fun

import java.io.StringReader

import org.apache.commons.csv.CSVFormat

import scala.collection.JavaConversions._
import scala.io.Source

object Main {
  def main(args: Array[String]) {

    object PassengerClass extends Enumeration {
      type PassengerClass = Value
      val first, second, third = Value

      def to(input: String): PassengerClass = {
        input match {
          case "1" => first
          case "2" => second
          case "3" => third
        }
      }
    }

    object Gender extends Enumeration {
      type Gender = Value
      val male, female = Value

      def to(input: String): Gender = {
        input match {
          case "male" => male
          case "female" => female
        }
      }
    }

    object PortOfEmbarkation extends Enumeration {
      type PortOfEmbarkation = Value
      val cherbourg, queenstown, southampton = Value

      def to(pid: String, input: String): Option[PortOfEmbarkation] = input match {
        case "C" => Some(cherbourg)
        case "Q" => Some(queenstown)
        case "S" => Some(southampton)
        case _ => None
      }
    }

    import PassengerClass._
    import Gender._
    import PortOfEmbarkation._

    def processAge(input: String): Either[Double, String] = {
      try {
        Left(input.toDouble)
      } catch {
        case ex: NumberFormatException =>
          Right(input)
      }
    }

    def processCabin(input: String) = input match {
      case "" => None
      case _ => Some(input)
    }

    def processSurvival(input: String) = input match {
      case "0" => false
      case "1" => true
    }

    case class TrainingRow(passengerId: Int,
                           survived: Boolean,
                           passengerClass: PassengerClass,
                           name: String,
                           sex: Gender,
                           age: Either[Double, String], // fractional if less than 1, if age is estimated, it is in the form xx.5
                           numberOfSiblingsSpouses: Int,
                           numberOfParentsChildren: Int,
                           ticketNumber: String,
                           fare: Double,
                           cabin: Option[String],
                           portOfEmbarkation: Option[PortOfEmbarkation])

    val trainingData = {
      val rawCsv = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("train.csv")).getLines.toList.tail.mkString("\n")
      val parser = CSVFormat.DEFAULT.withRecordSeparator("\n").parse(new StringReader(rawCsv))

      parser.getRecords map { field =>
        TrainingRow(passengerId = field.get(0).toInt,
          survived = processSurvival(field.get(1)),
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

    def entropy(trainingRows: Seq[TrainingRow]) = {
      val pSurvived = trainingRows.count(_.survived) / trainingRows.size.toDouble

      val pNotSurvived  = trainingRows.count(_.survived == false) / trainingRows.size.toDouble

      -(pSurvived * math.log(pSurvived)) - (pNotSurvived * math.log(pNotSurvived))
    }

    def splitOn(trainingRow: TrainingRow => Boolean, name: String) = {
      val (a, b) = trainingData.partition(trainingRow(_))
      println(s"split on: $name == entropy a: ${entropy(a)} entropy b: ${entropy(b)}")
    }

    /*
    note: there are a variety of "features" for which to split on --name, ticket number, fare which are numerical, strings etc.
      how to discreteize these and pick an appropiate split?
      numeric: sort values and use the midpoint as the split
      string: no idea, maybe requires some advanced knowldge about the semantics of the string to determine appropiate split:
              semantic splitter, something unique with heuristics for each field
     */

    //val (maleTrue, maleFalse) = trainingData.partition(asd => asd.sex == Gender.male)

    splitOn({ asd => asd.sex == Gender.male}, "gender")
    splitOn({ asd =>
      asd.portOfEmbarkation match {
        case Some(p) => p == southampton
        case None => false
      }
    }, "port == southampton")









  }
}