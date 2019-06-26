package com.robinliew.maths

import com.robinliew.bezier.MyPoint

/**
  * @author robinliew  向量计算工具类
  */
object MyVector {

  /**
    * 计算两个向量的 cos(sita)
    * @param v1
    * @param v2
    * @return
    */
  def getCosSita(v1: Array[MyPoint],v2: Array[MyPoint]): Double ={
    val x1 = v1(1).x - v1(0).x
    val y1 = v1(1).y - v1(0).y
    val x2 = v2(1).x - v2(0).x
    val y2 = v2(1).y - v2(0).y

    val dotProduct = x1*x2 + y1*y2
    val v1Model = Math.sqrt(x1*x1 + y1*y1)
    val v2Model = Math.sqrt(x2*x2 + y2*y2)

    val cosSita = dotProduct/(v1Model*v2Model)
    cosSita
  }

  /**
    * 计算 (后一点-当前点) 构成的向量与 （当前点-前一点）构成的向量的夹角
    * @param v1 当前点-前一点）
    * @param v2 (后一点-当前点)
    * @return
    */
  def getTheta(v1: Array[MyPoint],v2: Array[MyPoint]): Double ={
    val v1X = v1(1).x - v1(0).x
    val v1Y = v1(1).y - v1(0).y
    /**
      * 向量v1和正北方向的夹角
      */
    val v1Theta = Math.atan2(v1X,v1Y)

    val v2X = v2(1).x - v2(0).x
    val v2Y = v2(1).y - v2(0).y

    val v2Theta = Math.atan2(v2X,v2Y)

    if(v1Theta * v2Theta > 0){
      v2Theta - v1Theta
    }else{
      if(Math.abs(v1Theta)+Math.abs(v2Theta) < Math.PI){
        Math.abs(v1Theta)+Math.abs(v2Theta)
      }else{
        2*Math.PI - Math.abs(v1Theta) - Math.abs(v2Theta)
      }
    }
  }


  /**
    * 点旋转操作
    *
    * 逆时针旋转矩阵：
    * cos   -sin
    * sin   cos
    * 顺时针旋转矩阵：
    * cos   sin
    * -sin  cos
    *
    * @param op
    * @param tv
    * @param p
    * @return
    */
  def rotate(op: MyPoint,tv: Array[MyPoint],p: MyPoint,ANGLE: Double): MyPoint ={
    /**
      * 构建旋转矩阵需要的元素
      */
    val x = p.x - op.x
    val y = p.y - op.y //y轴方向向量值

    /**
      * 计算切线向量的x,y坐标
      */
    val tvX = tv(1).x - tv(0).x
    val tvY = tv(1).y - tv(0).y
    /**
      * 计算和正北方向的夹角theta
      * 范围为 [-PI,PI]
      */
    val tvTheta = Math.atan2(tvX,tvY)
    val oTheta = Math.atan2(x,y)
    var theta = tvTheta - oTheta + ANGLE
    if(theta < 0){
      theta = 2*Math.PI + theta
    }

    val cosTheta = Math.cos(theta)
    val sinTheta = Math.sin(theta)

    val deltaX = x*cosTheta + y*sinTheta
    val deltaY = -x*sinTheta + y*cosTheta
    /**
      * 计算出点旋转后的点坐标
      */
    val px = op.x + deltaX
    val py = op.y + deltaY

    new MyPoint(px,py)
  }

  /**
    * 旋转曲线的端点及中间点
    * @param zzV 起点-终点
    * @param m5  中间点
    * @return  起点-中间点-终点-旋转角度
    */
  def bezierRotate(zzV: Array[MyPoint],m5: MyPoint): (MyPoint,MyPoint,MyPoint,Double) ={
    val head = zzV(0)
    var end = zzV(1)
    val x1 = end.x - head.x
    val y1 = end.y - head.y

    /**
      * 计算 起点->终点 向量与正北方向的夹角
      */
    val theta1 = Math.atan2(x1,y1)
    /**
      * 起点->终点 向量与正东方向的夹角
      */
    val theta = theta1 - Math.PI/2

    /**
      * 以起点为圆心逆时针旋转theta
      * cos   -sin
      * sin   cos
      */
    val cosTheta = Math.cos(theta)
    val sinTheta = Math.sin(theta)

    val deltaX = x1*cosTheta - y1*sinTheta
    val deltaY = x1*sinTheta + y1*cosTheta
    val endX = head.x + deltaX
    val endY = head.y + deltaY
    end = new MyPoint(endX,endY)

    /**
      * 中间点m5旋转操作
      */
    val m5x = m5.x - head.x
    val m5y = m5.y - head.y
    val m5deltaX = m5x*cosTheta - m5y*sinTheta
    val m5delatY = m5x*sinTheta + m5y*cosTheta
    val newm5 = new MyPoint(head.x+m5deltaX,head.y+m5delatY)

    (head,newm5,end,theta)
  }

  /**
    * 顺时针旋转，还原控制点c0,c1的位置
    * @param head  起点
    * @param c0  控制点
    * @param c1  控制点
    * @param theta  旋转角度
    * @return
    */
  def bezierReturnRoate(head:MyPoint,c0: MyPoint,c1: MyPoint,theta: Double): (MyPoint,MyPoint) ={
    val cosTheta = Math.cos(theta)
    val sinTheta = Math.sin(theta)

    val c0X = c0.x - head.x
    val c0Y = c0.y - head.y
    val c0deltaX = c0X*cosTheta + c0Y*sinTheta
    val c0deltaY = -c0X*sinTheta + c0Y*cosTheta
    val newc0 = new MyPoint(head.x+c0deltaX,head.y+c0deltaY)

    val c1X = c1.x - head.x
    val c1Y = c1.y - head.y
    val c1deltaX = c1X*cosTheta + c1Y*sinTheta
    val c1deltaY = -c1X*sinTheta + c1Y*cosTheta
    val newc1 = new MyPoint(head.x+c1deltaX,head.y+c1deltaY)

    (newc0,newc1)
  }


  /**
    * 通过三个连续的点构造的向量的象限来剔除不好的点
    * @param preTV
    * @param nextTV
    * @return  true是不好的点，false ok的点
    */
  def isErrorP(preTV: Array[MyPoint], nextTV: Array[MyPoint]): Boolean ={
    val preX = preTV(1).x - preTV(0).x
    val preY = preTV(1).y - preTV(0).y
    val preQLoc = calcQuadrantLoc(preX,preY)

    val nextX = nextTV(1).x - nextTV(0).x
    val nextY = nextTV(1).y - nextTV(0).y
    val nextQLoc = calcQuadrantLoc(nextX,nextY)

//    println("preTV:"+ preTV(0).toString+","+preTV(1).toString)
//    println("nextTV:"+ nextTV(0).toString+","+nextTV(1).toString)
//    println("preQLoc:"+preQLoc+"--"+"nextQLoc:"+nextQLoc)

    if(nextQLoc != preQLoc + 2 && nextQLoc != preQLoc - 2){//不是不好的点
      false
    }else{//是不好的点
      true
    }
  }

  /**
    * 计算向量所处的象限
    * @param x
    * @param y
    * @return
    */
  def calcQuadrantLoc(x: Double,y: Double): Int ={
    if(x == 0 && y == 0){//原点
      0
    }else{
      if(x >= 0 && y >= 0){ //第一象限
        1
      }else if(x <= 0 && y >= 0){ //第二象限
        2
      }else if(x <= 0 && y <= 0){ //第三象限
        3
      }else if(x >= 0 && y <= 0){ //第四象限
        4
      }else{
        0
      }
    }
  }


  def main(args: Array[String]): Unit = {
    val theta = Math.atan2(-1,-1)
    val a: Double = 0.1
    val b: Double = 0.1000000000000000000000000000001
    val e = a - b

    println()
  }

}
