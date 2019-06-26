package com.robinliew.bezier

import scala.collection.mutable.ArrayBuffer

/**
  * @author robinliew
  */
object BezierBinomial {

  val lut = ArrayBuffer(Array(1),                //n=0
                        Array(1,1),              //n=1
                        Array(1,2,1),            //n=2
                        Array(1,3,3,1),          //n=3
                        Array(1,4,6,4,1),        //n=4
                        Array(1,5,10,10,5,1),    //n=5
                        Array(1,6,15,20,15,6,1)) //n=6

  /**
    * 计算贝塞尔函数多项式的系数
    * 一个请求的n/k对不在LUT查找表中时，先将表扩大
    * @param n
    * @param k
    * @return
    */
  def binomial(n: Int,k: Int): Int ={
    while(n >= lut.length){
      val s = lut.length
      val nextRow = new Array[Int](s+1)
      nextRow(0) = 1
      val prev = s -1
      for(i <- 1 until s){
        nextRow(i) = lut(prev)(i-1) + lut(prev)(i)
      }
      nextRow(s) = 1
      lut.append(nextRow)
    }
    lut(n)(k)
  }

}
