package com.codenjoy.dojo.bomberman.helpers

import com.codenjoy.dojo.bomberman.model.Implicits.point2ScalaPoint
import com.codenjoy.dojo.bomberman.model.Implicits.scalaPoint2Point
import com.codenjoy.dojo.client.Direction
import com.codenjoy.dojo.services.Point

object Helpers {

  implicit class PointHelpers(val p: Point) {
    def getVector(a: Point): Point =
      scalaPoint2Point(point2ScalaPoint(a).getVector(point2ScalaPoint(p)))

    def calculateNewDirection(a: Point): Direction =
      point2ScalaPoint(a).calculateNewDirection(point2ScalaPoint(p))
  }

}