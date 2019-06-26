package com.robinliew.maths

import com.robinliew.bezier.MyPoint
import org.apache.commons.math3.fitting.{PolynomialCurveFitter, WeightedObservedPoints}

/**
  * @author robinliew  拟合计算工具类
  */
object MyFit {

  /**
    * 根据离散的x,y数组进行拟合
    * @param x
    * @param y
    * @param degree
    * @return
    */
  def getFittCoeff(x: Array[Double],y: Array[Double],degree: Int): Array[Double] = {
    val points = new WeightedObservedPoints()
    for(i <- 0 until x.length){
      points.add(x(i),y(i))
    }
    val fitter = PolynomialCurveFitter.create(degree)
    val result = fitter.fit(points.toList)
    result
  }


  def getFittCoeff(pointSeq: Iterable[MyPoint],degree: Int): Array[Double] = {
    val points = new WeightedObservedPoints()
    pointSeq.foreach(point => {points.add(point.x,point.y)})
    val fitter = PolynomialCurveFitter.create(degree)
    val result = fitter.fit(points.toList)
    result
  }

  /**
    * 获取三次多项式拟合系数
    * @param pointList
    * @return
    */
  def getFit3Coeff(pointList: Iterable[MyPoint]): Array[Double] = {
    val points = new WeightedObservedPoints()
    pointList.foreach(point => {points.add(point.x,point.y)})
    val fitter = PolynomialCurveFitter.create(3)
    val result = fitter.fit(points.toList)
    result
  }


  /**
    * 计算切线的方法
    * @param coeff
    * @param x
    * @return
    */
  def getTangent(x: Double,coeff: Array[Double]): Double ={
    var res: Double = 0
    for(i <- 1 until coeff.length){
      res = res + i*coeff(i)*Math.pow(x,i-1)
    }
    res
  }

  /**
    * 计算二阶导数
    * @param x
    * @param coeff
    * @return
    */
  def getSecondDerivative(x: Double,coeff: Array[Double]): Double ={
    var res: Double = 0
    for(i <- 2 until coeff.length){
      res = res + i*(i-1)*coeff(i)*Math.pow(x,i-2)
    }
    res
  }

  /**
    * 求一元n次方程的值
   */
  def getCurveY(x: Double,coeff: Array[Double]): Double ={
    var res = coeff(0)
    for(i <- 1 until coeff.length){
      res = res + coeff(i)*Math.pow(x,i)
    }
    res
  }

  def main(args: Array[String]): Unit = {
//    val str = "1.2949606696969697E7,4836197.924242424,1.294960609375E7,4836185.90625,1.2949607181818182E7,4836172.090909091,1.2949608125E7,4836159.145833333,1.29496076E7,4836146.016666667,1.29496075E7,4836133.676470588,1.2949607775E7,4836120.8375,1.2949610151515152E7,4836107.666666667,1.2949610018181818E7,4836094.054545455,1.294961026923077E7,4836084.442307692"
//    val points = str.split(",")
//    val pointsSeq = new ListBuffer[MyPoint]
//    var i = 0
//    while(i < points.length-1){
//      pointsSeq.append(MyPoint(points(i).toDouble,points(i+1).toDouble))
//      i = i + 2
//    }
//
//    val coeff1 = MyFit.getFittCoeff(pointsSeq.take(1),3)
//    val res = pointsSeq.map(point => {
//      val x = point.x
//      val y = getCurveY(x,coeff1)
//      MyPoint(x,y)
//    })
//    println("res:"+res.map(_.toString).mkString(","))
//
//    val y1 = getCurveY(1.2949600879611E7,coeff1)
//    val y2 = getCurveY(1.2949606770113125E7,coeff1)
//
//    val coeff = Array(1.4157727338522192E7,-0.1774647096188916,-4.1856152197517975E-8,-2.0134688766222074E-18)
//    val y = getCurveY(12949680.6640625,coeff)
//    println()


    val coeffArr = Array[Double](1.4662608338031044E7,-0.3859841571581266,-5.141262972640723E-8,1.7467980433037426E-15)
    val points5 = Array[MyPoint](new MyPoint(1.294961623076923E7,4836018.769230769),
      new MyPoint(1.2949645142857144E7,4835980.214285715),
      new MyPoint(1.2949683125E7,4835980.625),
      new MyPoint(1.29496233E7,4835999.775),
      new MyPoint(1.29496115E7,4836038.805555556),
      new MyPoint(1.2949635090909092E7,4835985.2272727275),
      new MyPoint(1.29496388125E7,4835983.21875),
      new MyPoint(1.2949615642857144E7,4836025.535714285),
      new MyPoint(1.2949614E7,4836032.384615385),
      new MyPoint(1.2949656933333334E7,4835979.166666667),
      new MyPoint(1.29496206E7,4836007.1),
      new MyPoint(1.2949631125E7,4835988.0625),
      new MyPoint(1.2949627333333334E7,4835992.833333333),
      new MyPoint(1.2949614E7,4836032.384615385),
      new MyPoint(1.29496305E7,4835987.416666667),
      new MyPoint(1.2949650357142856E7,4835978.678571428),
      new MyPoint(1.29496175E7,4836012.892857143))

    val myNewPoints = Array[MyPoint](
      new MyPoint(1.294961623076923E7,4836018.769230769),
      new MyPoint(1.2949645142857144E7,4835980.214285715),
      new MyPoint(1.2949683125E7,4835980.625),
      new MyPoint(1.29496115E7,4836038.805555556),
      new MyPoint(1.2949635090909092E7,4835985.2272727275),
      new MyPoint(1.2949656933333334E7,4835979.166666667),
      new MyPoint(1.29496206E7,4836007.1),
      new MyPoint(1.2949627333333334E7,4835992.833333333)
    )


    val cof = MyFit.getFittCoeff(myNewPoints,3)

    points5.map(p => {
      println(p.x+","+p.y)
    })

    println("====================================================================")
    val result = myNewPoints.map(p => {
      val y = MyFit.getCurveY(p.x,cof)
      println(p.x+","+y)
    })
    println("====================================================================")



  }

}
