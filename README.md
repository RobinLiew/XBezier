# XBezier
Bezier interpolation,computing control points and Quadratic Bezier Curve over three points and its order upgrading,de Casteljau,etc

我们通常用线条来绘制2D图形，大致分为两种线条：直线和曲线。不论我们动手还是用电脑，都能很容易地画出第一种线条。只要给电脑起点和终点，砰！直线就画出来了。没什么好疑问的。

然而，绘制曲线却是个大问题。虽然我们可以很容易地徒手画出曲线，但除非给出描述曲线的数学函数，不然计算机无法画出曲线。实际上，画直线时也需要数学函数，但画直线所需的方程式很简单，我们在这里不去考虑。在计算机看来，所有线条都是“函数”，不管它们是直线还是曲线。然而，这就表示我们需要找到能在计算机上表现良好的曲线方程。这样的曲线有很多种，在本文我们主要关注一类特殊的、备受关注的函数，基本上任何画曲线的地方都会用到它：贝塞尔曲线。

你怎么画它们？包围盒是怎么样的，怎么确定交点，怎么拉伸曲线，怎么进行bezier插值，怎么计算控制点？怎么计算过三点的二次贝塞尔曲线及其升阶（自适应的）？该项目对上述问题的计算逻辑使用scala进行了封装，你可以直接调用，也可以进行相关修改。

# 参考文献
- https://pomax.github.io/bezierinfo/zh-CN/
- http://blog.sklambert.com/finding-the-control-points-of-a-bezier-curve/
- https://xuhehuan.com/2608.html