package com.codenjoy.dojo.bomberman

import com.codenjoy.dojo.bomberman.model.Elements
import com.codenjoy.dojo.bomberman.model.Implicits._
import com.codenjoy.dojo.bomberman.model._
import com.codenjoy.dojo.client.Direction
import com.codenjoy.dojo.services.Point

import scala.collection.JavaConversions._

class BSolver {

  val bombRange = 3
  val meatChoppers = scala.collection.mutable.Set[MeatChopper]()
  val otherBombermans = scala.collection.mutable.Set[Bomberman]()
  val previousSteps = scala.collection.mutable.Queue[ScalaPoint]()
  val keepOutOfHere = scala.collection.mutable.Set[ScalaPoint]()

  var curTarget: Option[Bomberman] = None
  var lastDirection = Direction.STOP

  def solve(board: Board): String = {
    previousSteps.drop(previousSteps.size - 7)

    updateMovables[Bomberman](
      "Bomberman",
      new Bomberman(_),
      otherBombermans,
      board.get(Elements.OTHER_DEAD_BOMBERMAN).toList,
      board.getOtherBombermans.toList)

    updateMovables[MeatChopper](
      "MeatChopper",
      new MeatChopper(_),
      meatChoppers,
      board.get(Elements.DEAD_MEAT_CHOPPER).toList,
      board.getMeatChoppers.toList)

    keepOutOfHere.clear
    keepOutOfHere ++= meatChoppers.flatMap(_.listPointsByDirection)
    val currentPos = board.getBomberman
    val barriers = board.getBarriers.map(point2ScalaPoint).toList ++ keepOutOfHere

    if (curTarget.isEmpty) curTarget = findTargets(currentPos).headOption

    val (direction, act) = calcNewDirectionAndAct(board.isGameOver, currentPos, barriers)
    if (!act) lastDirection = direction
    direction.ACT(act)
  }

  private def calcNewDirectionAndAct(
    isGameOver: Boolean,
    currentPos: ScalaPoint,
    barriers: List[ScalaPoint]): (Direction, Boolean) = curTarget match {
    case _ if isGameOver => {
      curTarget = None
      clearSteps(None)
      (Direction.STOP, false)
    }
    case _ if isSurrounded(barriers, currentPos) => (Direction.ACT, false)
    case Some(ct) => {
      val paths = generatePath(barriers, ct.point, currentPos)
      previousSteps ++= paths
      if (ct.point.distance(currentPos) == 1) {
        curTarget = findTargets(currentPos).filterNot(_ == curTarget).headOption
        clearSteps(Some(currentPos))
        if (paths.isEmpty) (Direction.ACT, false)
        else (currentPos.calculateNewDirection(paths.head), true)
      } else if (paths.isEmpty) {
        clearSteps(Some(currentPos))
        if (lastDirection == Direction.STOP || lastDirection == Direction.ACT) (Direction.random, true)
        else (lastDirection.clockwise().clockwise(), true)
      } else (currentPos.calculateNewDirection(paths.head), false)
    }
    case None => (Direction.STOP, false)
  }



  private def isSurrounded(barriers: List[ScalaPoint], currentPos: Point): Boolean =
    barriers.count(barrier => {
      barrier.distance(currentPos) == 1
    }) >= 4


  private def clearSteps(currentPos: Option[ScalaPoint]) = {
    previousSteps.clear
    otherBombermans.clear
    meatChoppers.clear
    currentPos.foreach(previousSteps.enqueue(_))
  }

  private def generatePath(
    barriers: List[ScalaPoint],
    target: ScalaPoint,
    curPos: ScalaPoint) = {

    def generatePathFromPoint(
      left: Int,
      barriers: List[ScalaPoint],
      target: ScalaPoint,
      curPos: ScalaPoint): Option[PathNode] = left match {
      case 0 => Some(PathNode(curPos, List()))
      case _ =>
        val directionalPositions = List(
          curPos.addVector(0, -1),
          curPos.addVector(0, 1),
          curPos.addVector(1, 0),
          curPos.addVector(-1, 0)
        ).filterNot(barriers.contains)

        val nextNodes = directionalPositions.sortBy(_.distance(target))
        Some(PathNode(curPos, nextNodes.flatMap(generatePathFromPoint(left - 1, barriers, target, _))))
    }
    generatePathFromPoint(3, barriers, target, curPos)
      .map(_.nextNodes.map(_.point))
      .get
      .filterNot(previousSteps.contains)
      .take(1)
  }


  private def updateMovables[T <: Movable](
    name: String,
    funcConstructor: ScalaPoint => T,
    localInfo: scala.collection.mutable.Set[T],
    deaths: List[Point],
    currents: List[Point]) = {

    updateDeaths(name, localInfo, deaths)
    removeElements(name, localInfo)
    updateMovables1(name, funcConstructor, localInfo, currents)
  }

  private def updateMovables1[T <: Movable](
    name: String,
    funcConstructor: ScalaPoint => T,
    localInfo: scala.collection.mutable.Set[T],
    currents: List[Point]) = {

    val handledMobs = scala.collection.mutable.Set[T]()
    currents
      .map(mc => mc -> localInfo.filter(_.isProbablySame(mc)).toList)
      .map({
        case (chopper, Nil) =>
          val newChopper = funcConstructor(chopper)
          localInfo.add(newChopper)
          chopper -> newChopper

        case (chopper, probableChoppers) if probableChoppers.size > 1 =>
          chopper -> {
            val movable = probableChoppers.filterNot(handledMobs).find(pc => {
              val direction = pc.point.calculateNewDirection(chopper)
              direction.equals(pc.direction)
            }) match {
              case None => probableChoppers.head
              case Some(pc) => pc
            }
            handledMobs.add(movable)
            movable
          }
        case (chopper, singleChopper :: Nil) =>
          handledMobs.add(singleChopper)
          chopper -> singleChopper
      })
      .foreach({
        case (chopper, theRealChoppah) =>
          theRealChoppah.setNewPoint(chopper)
      })
  }

  private def updateDeaths[T <: Movable](
    name: String,
    localInfo: scala.collection.mutable.Set[T],
    deaths: List[Point]) = deaths.map(dmc => {
    localInfo.find(mc => {
      mc.isProbablySame(dmc)
    }) match {
      case None =>
      case Some(realdmc) =>
        localInfo.remove(realdmc)
    }
  })

  private def removeElements[T <: Movable](name: String, localInfo: scala.collection.mutable.Set[T]) = {
    val removedElements = localInfo.filter(_.lastUpdate < (System.currentTimeMillis - 5000))
    localInfo.removeAll(removedElements)
    if (removedElements.contains(curTarget)) curTarget = None
  }

  private def findTargets(currentPos: ScalaPoint): List[Bomberman] =
    otherBombermans.toList.sortBy(_.lastMovementTime).take(3).sortBy(_.point.distance(currentPos))

}
