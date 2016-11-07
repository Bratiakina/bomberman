package com.codenjoy.dojo.bomberman.model

import com.codenjoy.dojo.bomberman.model.Implicits.scalaPoint2Point
import com.codenjoy.dojo.client.Direction

import scala.util.Random

case class Movable(
  point: ScalaPoint,
  lastPoint: ScalaPoint,
  typeName: String,
  lastMovementTime: Long = System.currentTimeMillis,
  uid: Long = Random.nextInt(100),
  direction: Direction = Direction.STOP,
  lastDirection: Direction = Direction.STOP,
  lastUpdate: Long = System.currentTimeMillis) {

  def isProbablySame(newPoint: ScalaPoint): Boolean =
    point.distance(newPoint) <= 1

  override def toString: String =
    s"$typeName(uid: $uid, $point, lastUpdate: $lastUpdate, direction: $direction)"

  def setNewPoint(newPoint: ScalaPoint) = this.copy(
    lastPoint = point,
    point = newPoint,
    lastDirection = direction,
    direction = lastPoint.calculateNewDirection(point),
    lastMovementTime = System.currentTimeMillis)

  def listPointsByDirection: List[ScalaPoint] = {
    List(
      point.addVector(-1, 0),
      point.addVector(1, 0),
      point.addVector(0, -1),
      point.addVector(0, 1),
      point,

      direction match {
        case Direction.LEFT => point.addVector(-2, 0)
        case Direction.RIGHT => point.addVector(2, 0)
        case Direction.UP => point.addVector(0, -2)
        case Direction.DOWN => point.addVector(0, 2)
        case _ => point
      }
    )
  }
}

class MeatChopper(point: ScalaPoint) extends Movable(point, point, "MeatChopper")

class Bomberman(point: ScalaPoint) extends Movable(point, point, "Bomber")