package com.fun.model

import com.fun.model.Gender.Gender
import com.fun.model.PassengerClass.PassengerClass
import com.fun.model.PortOfEmbarkation.PortOfEmbarkation

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

case class TrainingRow(passengerId: Int,
                       survived: Option[Boolean],
                       passengerClass: PassengerClass,
                       name: String,
                       sex: Gender,
                       age: Option[Double], // fractional if less than 1, if age is estimated, it is in the form xx.5
                       numberOfSiblingsSpouses: Int,
                       numberOfParentsChildren: Int,
                       ticketNumber: String,
                       fare: Double,
                       cabin: Option[String],
                       portOfEmbarkation: Option[PortOfEmbarkation])
