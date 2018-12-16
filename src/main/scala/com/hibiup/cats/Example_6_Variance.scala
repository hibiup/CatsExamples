package com.hibiup.cats

/**
  * 协变：容器的继承顺序和容器内的类型的继承顺序一致
  **/
object Example_6_Covariance {
    sealed trait Shape
    case class Circle(radius: Double) extends Shape

    /** 声明一个协变容器 */
    trait Container[+A]

    /** 声明一个子类的容器 */
    val circles: Container[Circle] = new Container[Circle]{}

    /** 允许赋值给父类容器 */
    val shapes: Container[Shape] = circles

    assert(circles.isInstanceOf[Container[Shape]])
    var r = circles.isInstanceOf[shapes.type ]
}

/**
  * 逆变：容器的继承关系和容器内的类型的继承关系相反
  **/
object Example_6_Contravariance {
    sealed trait Shape
    case class Circle(radius: Double) extends Shape

    /** 声明一个逆变容器 */
    trait Container[-A]

    /** 声明一个父类的容器 */
    val shapes: Container[Shape] = new Container[Shape]{}

    /** 允许赋值给子类容器 */
    val circles: Container[Circle] = shapes

    assert(shapes.isInstanceOf[Container[Circle]])
    var r = shapes.isInstanceOf[circles.type ]
}

/**
  * 不变：容器之间没有继承关系
  **/
object Example_6_Invariance {
    sealed trait Shape
    case class Circle(radius: Double) extends Shape

    /** 声明一个逆变容器 */
    trait Container[A]

    /** 声明一个父类的容器 */
    val shapes: Container[Shape] = new Container[Shape]{}

    /** 声明一个子类的容器 */
    val circles: Container[Circle] = new Container[Circle]{}

    val r = !shapes.isInstanceOf[circles.type ]
}