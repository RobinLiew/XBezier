package com.robinliew.bezier

import java.util

import com.robinliew.maths.{MyFit, MyVector}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.Set


class MyPoint(){
  var x: Double = _
  var y: Double = _
  def this(x: Double,y: Double){
    this
    this.x = x
    this.y = y
  }
  override def toString: String = x+","+y
}

object Bezier {

  /**
    * 贝塞尔基本函数
    * @param n 多项式的幂
    * @param t 控制参数
    * @return
    */
  def bezier(n: Int,t: Double): Double ={
    var sum: Double = 0
    for(k <- 0 to n){
      sum += BezierBinomial.binomial(n,k) * Math.pow(1-t,n-k) * Math.pow(t,k)
    }
    sum
  }

  /**
    * 贝塞尔二次曲线
    * @param t
    */
  def bezier2(t: Double): Double ={
    val t2 = t * t
    val mt = 1 - t
    val mt2 = mt * mt
    mt2 + 2*mt*t + t2
  }

  /**
    * 贝塞尔三次曲线
    * @param t
    * @return
    */
  def bezier3(t: Double): Double ={
    val t2 = t * t
    val t3 = t2 * t
    val mt = 1 -t
    val mt2 = mt * mt
    val mt3 = mt2 * mt
    mt3 + 3*mt2*t + 3*mt*t2 +t3
  }

  /**
    * 带权重的贝塞尔基本函数
    * @param n
    * @param t
    * @param w 权重
    * @return
    */
  def bezierW(n: Int,t: Double,w: Array[Double]): Double ={
    var sum: Double = 0
    for(k <- 0 until n){
      sum += w(k) * BezierBinomial.binomial(n,k) * Math.pow(1-t,n-k) * Math.pow(t,k)
    }
    sum
  }

  /**
    * 带权重的贝塞尔二次函数
    * @param t
    * @param w
    * @return
    */
  def bezier2W(t: Double,w: Array[Double]): Double ={
    val t2 = t * t
    val mt = 1 - t
    val mt2 = mt * mt
    w(0)*mt2 + w(1)*2*mt*t + w(2)*t2
  }

  def bezier2W(t: Double,w: mutable.Buffer[Double]): Double ={
    val t2 = t * t
    val mt = 1 - t
    val mt2 = mt * mt
    w(0)*mt2 + w(1)*2*mt*t + w(2)*t2
  }

  /**
    * 带权重的贝塞尔三次函数
    * @param t
    * @param w
    * @return
    */
  def bezier3W(t: Double,w: Array[Double]): Double ={
    val t2 = t * t
    val t3 = t2 * t
    val mt = 1 -t
    val mt2 = mt * mt
    val mt3 = mt2 * mt
    w(0)*mt3 + w(1)*3*mt2*t + w(2)*3*mt*t2 + w(3)*t3
  }


  /**
    * 通过起始点和控制点计算贝塞尔三次函数
    * @param t
    * @param w
    * @return
    */
  def bezier3W(t: Double,w: mutable.Buffer[Double]): Double ={
    val t2 = t * t
    val t3 = t2 * t
    val mt = 1 -t
    val mt2 = mt * mt
    val mt3 = mt2 * mt
    w(0)*mt3 + w(1)*3*mt2*t + w(2)*3*mt*t2 + w(3)*t3
  }


  /**
    * de Casteljau 算法的递归实现
    * @param points
    * @param t
    * @return
    */
  def deCasteljauCurve(points: Array[MyPoint],t: Double,ps: Set[MyPoint]): Set[MyPoint] ={
    if(ps == null){
      throw new Exception("please  init psBuffer!")
    }
    ps.add(points(0))
    ps.add(points(points.size - 1))

    if(points.length == 1){
      ps.add(points(0))
    }else{
      val newPoints = new Array[MyPoint](points.length - 1)
      for(i <- 0 until newPoints.length){
        val x = (1-t)*points(i).x+t*points(i+1).x
        val y = (1-t)*points(i).y+t*points(i+1).y
        newPoints(i) = new MyPoint(x,y)
        ps.add(newPoints(i))
      }
      deCasteljauCurve(newPoints,t,ps)
    }
    ps
  }


  /**
    * 计算三次贝塞尔控制点(两个控制点)方法的 重载
    * @param forcpoints
    * @return
    */
  def getControlPoints(forcpoints: java.util.List[(MyPoint, Array[Double])]): java.util.List[MyPoint] ={

    val cPoints = new util.ArrayList[MyPoint]()
    /**
      * 首先计算曲线起终点斜率
      */
    val headPoint = forcpoints.head._1
    val len = forcpoints.length
    val endPoint = forcpoints.last._1

    if((headPoint.x - endPoint.x).abs <  0.000000001){
      var diffTemp = 1.0
      /**
        * 请参照 http://blog.sklambert.com/finding-the-control-points-of-a-bezier-curve/
        * 中的计算方法
        */
      var m5: MyPoint = null

      /**
        * 计算中点
        */
      forcpoints.map(entry => {
        val p = entry._1
        val coeffArr = entry._2
        val diff = Math.abs(1/MyFit.getTangent(p.x, coeffArr))
        if (diff < diffTemp) {
          diffTemp = diff
          m5 = p
        }
      })
      /**
        * 先求控制点c0的x坐标
        */
      val c0x = (m5.x - headPoint.x)*2/3 + headPoint.x

      /**
        * 求控制点c0的y坐标
        */
      val c0y = (m5.y - headPoint.y)*4/3 + headPoint.y

      /**
        * 先求控制点c1的x坐标
        */
      val c1x = (m5.x - endPoint.x)*2/3 + endPoint.x

      /**
        * 求控制点c1的y坐标
        */
      val c1y = (m5.y - endPoint.y)*4/3 + endPoint.y


    }else {
      var diffTemp = Double.MaxValue
      val k = Math.abs((headPoint.y - endPoint.y) / (headPoint.x - endPoint.x))
      /**
        * 请参照http://blog.sklambert.com/finding-the-control-points-of-a-bezier-curve/
        * 中的计算方法
        */
      var m5: MyPoint = null

      /**
        * 计算中点
        */
      forcpoints.map(entry => {
        val p = entry._1
        val coeffArr = entry._2
        val diff = Math.abs(MyFit.getTangent(p.x, coeffArr) - k)
        if (diff < diffTemp) {
          diffTemp = diff
          m5 = p
        }
      })

      /**
        * 先求控制点c0的x坐标
        */
      val c0x = (m5.x - headPoint.x)*2/3 + headPoint.x

      /**
        * 求控制点c0的y坐标
        */
      val c0y = (m5.y - headPoint.y)*4/3 + headPoint.y

      /**
        * 先求控制点c1的x坐标
        */
      val c1x = (m5.x - endPoint.x)*2/3 + endPoint.x

      /**
        * 求控制点c1的y坐标
        */
      val c1y = (m5.y - endPoint.y)*4/3 + endPoint.y

      cPoints.add(headPoint)
      cPoints.add(new MyPoint(c0x,c0y))
      cPoints.add(new MyPoint(c1x,c1y))
      cPoints.add(endPoint)
    }

    cPoints
  }

  /**
    * 计算贝三次塞尔函数控制点(两个控制点)方法2 重载
    * @param forcpoints
    * @return
    */
  def getControlPoints(forcpoints: util.ArrayList[(Int, MyPoint)]): java.util.List[MyPoint] ={

    val cPoints = new util.ArrayList[MyPoint]()
    val indexPMap = forcpoints.toMap

    /**
      * 首先计算曲线起终点斜率
      */
    val headPoint = forcpoints.head._2
    //val len = forcpoints.length
    val endPoint = forcpoints.last._2

    if((headPoint.x - endPoint.x).abs <  0.000000001){
      var diffTemp = Double.MaxValue
      /**
        * 请参照 http://blog.sklambert.com/finding-the-control-points-of-a-bezier-curve/
        * 中的计算方法
        */
      var m5: MyPoint = null

      /**
        * 计算中点
        */
      forcpoints.foreach(entry => {
        val p = entry._2
        val index = entry._1
        val nextOpt = indexPMap.get(index+1)
        var nextP: MyPoint = null
        if(!nextOpt.isEmpty){
          nextP = nextOpt.get
        }
        if(nextP != null){
          val diff = Math.abs(nextP.x - p.x)
          if(diff < diffTemp){
            diffTemp = diff
            m5 = p
          }
        }
      })

      /**
        * 先求控制点c0的x坐标
        */
      val c0x = (m5.x - headPoint.x)*2/3 + headPoint.x

      /**
        * 求控制点c0的y坐标
        */
      val c0y = (m5.y - headPoint.y)*4/3 + headPoint.y

      /**
        * 先求控制点c1的x坐标
        */
      val c1x = (m5.x - endPoint.x)*2/3 + endPoint.x

      /**
        * 求控制点c1的y坐标
        */
      val c1y = (m5.y - endPoint.y)*4/3 + endPoint.y


    }else {
      var diffTemp = Double.MaxValue
      val k = (endPoint.y - headPoint.y) / (endPoint.x - headPoint.x)
      /**
        * 请参照http://blog.sklambert.com/finding-the-control-points-of-a-bezier-curve/
        * 中的计算方法
        */
      var m5: MyPoint = null

      /**
        * 计算中点
        */
      forcpoints.map(entry => {
        val index = entry._1
        val p = entry._2
        val nextOpt = indexPMap.get(index+1)
        var nextP: MyPoint = null
        if(!nextOpt.isEmpty){
          nextP = nextOpt.get
        }
        if(nextP != null){
          val diff = Math.abs((nextP.y - p.y)/(nextP.x - p.x) - k)
          if(diff < diffTemp){
            diffTemp = diff
            m5 = p
          }
        }
      })

      /**
        * 逆时针旋转过去
        */
      val zzV = Array[MyPoint](headPoint,endPoint)
      val tuple = MyVector.bezierRotate(zzV,m5)
      val newHead = tuple._1
      val newM5 = tuple._2
      val newEnd = tuple._3
      val theta = tuple._4

      /**
        * 先求控制点c0的x坐标
        */
      val c0x = (newM5.x - newHead.x)*2/3 + newHead.x

      /**
        * 求控制点c0的y坐标
        */
      val c0y = (newM5.y - newHead.y)*4/3 + newHead.y

      /**
        * 先求控制点c1的x坐标
        */
      val c1x = (newM5.x - newEnd.x)*2/3 + newEnd.x

      /**
        * 求控制点c1的y坐标
        */
      val c1y = (newM5.y - newEnd.y)*4/3 + newEnd.y

      val c0 = new MyPoint(c0x,c0y)
      val c1 = new MyPoint(c1x,c1y)

      /**
        * 顺时针旋转回来
        */
      val tuple1 = MyVector.bezierReturnRoate(headPoint,c0,c1,theta)
      val newc0 = tuple1._1
      val newc1 = tuple1._2

      cPoints.add(headPoint)
      cPoints.add(newc0)
      cPoints.add(newc1)
      cPoints.add(endPoint)
    }

    cPoints
  }

  /**
    * 二次贝塞尔曲线的控制点
    * @param forcpoints
    * @return
    */
  def getSecControlPoints(forcpoints: util.ArrayList[(Int, MyPoint)]): java.util.List[MyPoint] ={

    val cPoints = new util.ArrayList[MyPoint]()
    val indexPMap = forcpoints.toMap

    /**
      * 首先计算曲线起终点斜率
      */
    val headPoint = forcpoints.head._2
    //val len = forcpoints.length
    val endPoint = forcpoints.last._2

    if((headPoint.x - endPoint.x).abs <  0.000000001){
      var diffTemp = Double.MaxValue
      /**
        * 请参照 http://blog.sklambert.com/finding-the-control-points-of-a-bezier-curve/
        * 中的计算方法
        */
      var m5: MyPoint = null

      /**
        * 计算中点
        */
      forcpoints.foreach(entry => {
        val p = entry._2
        val index = entry._1
        val nextOpt = indexPMap.get(index+1)
        var nextP: MyPoint = null
        if(!nextOpt.isEmpty){
          nextP = nextOpt.get
        }
        if(nextP != null){
          val diff = Math.abs(nextP.x - p.x)
          if(diff < diffTemp){
            diffTemp = diff
            m5 = p
          }
        }
      })

      /**
        * 先求控制点c0的x坐标
        */
      val c0x = (m5.x - headPoint.x)*2/3 + headPoint.x

      /**
        * 求控制点c0的y坐标
        */
      val c0y = (m5.y - headPoint.y)*4/3 + headPoint.y

      /**
        * 先求控制点c1的x坐标
        */
      val c1x = (m5.x - endPoint.x)*2/3 + endPoint.x

      /**
        * 求控制点c1的y坐标
        */
      val c1y = (m5.y - endPoint.y)*4/3 + endPoint.y


    }else {
      var diffTemp = Double.MaxValue
      val k = (endPoint.y - headPoint.y) / (endPoint.x - headPoint.x)
      /**
        * 请参照http://blog.sklambert.com/finding-the-control-points-of-a-bezier-curve/
        * 中的计算方法
        */
      var m5: MyPoint = null

      /**
        * 计算中点
        */
      forcpoints.map(entry => {
        val index = entry._1
        val p = entry._2
        val nextOpt = indexPMap.get(index+1)
        var nextP: MyPoint = null
        if(!nextOpt.isEmpty){
          nextP = nextOpt.get
        }
        if(nextP != null){
          val diff = Math.abs((nextP.y - p.y)/(nextP.x - p.x) - k)
          if(diff < diffTemp){
            diffTemp = diff
            m5 = p
          }
        }
      })

      /**
        * 逆时针旋转过去
        */
      val zzV = Array[MyPoint](headPoint,endPoint)
      val tuple = MyVector.bezierRotate(zzV,m5)
      val newHead = tuple._1
      val newM5 = tuple._2
      val newEnd = tuple._3
      val theta = tuple._4

      /**
        * 先求控制点c的x坐标
        */
      val cx = newM5.x
      val cy = (newM5.y - newHead.y)*4/3 + newHead.y

      val cosTheta = Math.cos(theta)
      val sinTheta = Math.sin(theta)
      val cX = cx - newHead.x
      val cY = cy - newHead.y
      val cdeltaX = cX*cosTheta + cY*sinTheta
      val cdeltaY = -cX*sinTheta + cY*cosTheta
      val newc = new MyPoint(newHead.x+cdeltaX,newHead.y+cdeltaY)

      cPoints.add(headPoint)
      cPoints.add(newc)
      cPoints.add(endPoint)
    }

    cPoints
  }

  /**
    * 计算通过中点的控制点
    * 参考：https://xuhehuan.com/2608.html
    * @param forcpoints
    * @return
    */
  def getSecNewCPoints(forcpoints: util.ArrayList[(Int, MyPoint)]): java.util.List[MyPoint] = {
    val cPoints = new util.ArrayList[MyPoint]()
    val indexPMap = forcpoints.toMap

    /**
      * 首先计算曲线起终点斜率
      */
    val P0 = forcpoints.head._2
    val P2 = forcpoints.last._2

    if((P0.x - P2.x).abs <  0.000000001){
      var diffTemp = Double.MaxValue
      /**
        * 请参照 http://blog.sklambert.com/finding-the-control-points-of-a-bezier-curve/
        * 中的计算方法
        */
      var m5: MyPoint = null

      /**
        * 计算中点
        */
      forcpoints.foreach(entry => {
        val p = entry._2
        val index = entry._1
        val nextOpt = indexPMap.get(index+1)
        var nextP: MyPoint = null
        if(!nextOpt.isEmpty){
          nextP = nextOpt.get
        }
        if(nextP != null){
          val diff = Math.abs(nextP.x - p.x)
          if(diff < diffTemp){
            diffTemp = diff
            m5 = p
          }
        }
      })

    }else {
      var diffTemp = Double.MaxValue
      val k = (P2.y - P0.y) / (P2.x - P0.x)
      /**
        * 请参照http://blog.sklambert.com/finding-the-control-points-of-a-bezier-curve/
        * 中的计算方法
        */
      var P1: MyPoint = null

      /**
        * 计算中点
        */
      forcpoints.map(entry => {
        val index = entry._1
        val p = entry._2
        val nextOpt = indexPMap.get(index+1)
        var nextP: MyPoint = null
        if(!nextOpt.isEmpty){
          nextP = nextOpt.get
        }
        if(nextP != null){
          val diff = Math.abs((nextP.y - p.y)/(nextP.x - p.x) - k)
          if(diff < diffTemp){
            diffTemp = diff
            P1 = p
          }
        }
      })

      /**
        * 计算控制点Pc
        * 使用公式法
        */
      val tempXS = (P0.x - P1.x)/Math.abs(P0.x - P1.x) + (P2.x-P1.x)/Math.abs(P2.x-P1.x)
      val tempYS = (P0.y - P1.y)/Math.abs(P0.y - P1.y) + (P2.y-P1.y)/Math.abs(P2.y-P1.y)
      val PcX = P1.x - 0.5*Math.sqrt(Math.abs(P0.x-P1.x)*Math.abs(P2.x-P1.x))*tempXS
      val PcY = P1.y - 0.5*Math.sqrt(Math.abs(P0.y-P1.y)*Math.abs(P2.y-P1.y))*tempYS
      val Pc = new MyPoint(PcX,PcY)

      cPoints.add(P0)
      cPoints.add(Pc)
      cPoints.add(P2)
    }
    cPoints
  }


  /**
    * 根据起终点和贝塞尔函数两个控制点进行曲线等距插值
    * @param cpoints
    * @param pointsNum 插值点的数目
    * @return
    */
  def getFICurveTracing(cpoints: java.util.List[MyPoint],pointsNum: Int): java.util.List[MyPoint] ={
    val xList = cpoints.map(_.x)
    val yList = cpoints.map(_.y)

    val points = new java.util.ArrayList[MyPoint]()

    val intervalLen = 1.0/pointsNum.toDouble

    var i = 0.0
    while(i <= 1){
      val x = bezier3W(i,xList)
      val y = bezier3W(i,yList)
      points.append(new MyPoint(x,y))
      i += intervalLen
    }
    //println("points:"+points.map(_.toString).mkString(","))
    points
  }

  /**
    * 得到二次贝塞尔曲线的插值点
    * @param cpoints
    * @param pointsNum
    * @return
    */
  def getSecFICurveTracing(cpoints: java.util.List[MyPoint],pointsNum: Int): java.util.List[MyPoint] ={
    val xList = cpoints.map(_.x)
    val yList = cpoints.map(_.y)

    val points = new java.util.ArrayList[MyPoint]()

    val intervalLen = 1.0/pointsNum.toDouble

    var i = 0.0
    while(i <= 1){
      val x = bezier2W(i,xList)
      val y = bezier2W(i,yList)
      points.append(new MyPoint(x,y))
      i += intervalLen
    }
    //println("points:"+points.map(_.toString).mkString(","))
    points
  }


  def main(args: Array[String]): Unit = {

    val max = Double.MaxValue

    val points = Array[MyPoint](
      new MyPoint(1.2949606399142688E7,4836044.285391158),
      new MyPoint(1.2949609010226458E7,4836030.299937003),
      new MyPoint(1.2949615430766432E7,4836006.162217802),
      new MyPoint(1.2949625379121324E7,4835987.394081466),
      new MyPoint(1.2949633102759935E7,4835980.639543123),
      new MyPoint(1.2949633102759935E7,4835980.639543123),
      new MyPoint(1.2949659843488984E7,4835972.068763084),
      new MyPoint(1.2949686161896478E7,4835974.483982254),
      new MyPoint(1.2949713180634681E7,4835975.68698023))

    val res = Set[MyPoint]()
    deCasteljauCurve(points,0.5,res)
    val SEPARATOR = System.getProperty("line.separator")
    var wktRes = "P"+SEPARATOR
    res.map(p => {
      wktRes += "POINT ("+p.x+" "+p.y+")"+SEPARATOR
    })
    println(wktRes)
    println("==================================================================")
    println("points:"+points.map(_.toString).mkString(","))
    println("res:"+res.map(_.toString).mkString(","))


  }


}
