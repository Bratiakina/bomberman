package com.codenjoy.dojo.bomberman.model

import com.codenjoy.dojo.client.Direction
import com.codenjoy.dojo.services.Point
import com.codenjoy.dojo.services.PointImpl

case class PathNode(point: ScalaPoint, nextNodes: List[PathNode])

case class ScalaPoint(x: Int, y: Int) {

  def calculateNewDirection(newPoint: ScalaPoint): Direction = {
    val diff = getVector(newPoint)
    diff match {
      case ScalaPoint(0, x) if (x > 0) => Direction.UP
      case ScalaPoint(0, x) if (x < 0) => Direction.DOWN
      case ScalaPoint(x, 0) if (x > 0) => Direction.LEFT
      case ScalaPoint(x, 0) if (x < 0) => Direction.RIGHT
      case ScalaPoint(0, 0) => Direction.STOP
      case _ => Direction.random
    }
  }

  def getVector(b: ScalaPoint): ScalaPoint = {
    ScalaPoint(x - b.x, y - b.y)
  }

  def addVector(ax: Int, ay: Int): ScalaPoint = {
    ScalaPoint(x + ax, y + ay)
  }
}


object Implicits {
  implicit def direction2String(d: Direction): String = d.toString

  implicit def point2ScalaPoint(p: Point): ScalaPoint = ScalaPoint(p.getX, p.getY)

  implicit def scalaPoint2Point(p: ScalaPoint): Point = PointImpl.pt(p.x, p.y)
}