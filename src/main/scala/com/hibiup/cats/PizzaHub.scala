package com.hibiup.cats

/**
  * 根据 README.md 的介绍,以 Cats 自带的 Cats.Show 为例讲解 typeclass, instance 和 instance 如何协同工作.
  */
object PizzaHub {
    /**
      * 准备一块 Pizza, Cats 将会将 Monad 方法 show 绑定到这块 Pizza 上.
      * */
    import PizzaHubModels._

    val pizza = Pizza(
        LargeCrustSize,
        ThinCrustType,
        Seq(Cheese, Pepperoni, Sausage)
    )


    /**
      * 1）typeclass：
      *
      * import cats.Show 引进了 Show 这个 typeclass，它定义了将要绑定的 show 方法：
      *
        trait Show[T] extends Show.ContravariantShow[T]

        // 继承自 ContravariantShow：
        trait ContravariantShow[-T] extends Serializable {
            def show(t: T): String
        }
      *
      *  可以看到它定义它只包含了一个 show 方法. 没有具体实现,只是一个 typeclass
      * */
    import cats.Show


    /**
      * 2）interface：
      *
      * interface 是暴露给用户的使用界面,也就是将 API 绑定到 Pizza 上,这里使用了 Syntax 方式. 我们看到
      * import cats.syntax.show._ 引进了 Show 的 syntax interface:
      *
        trait ShowSyntax extends Show.ToShowOps {
            implicit final def showInterpolator(sc: StringContext): Show.ShowInterpolator = Show.ShowInterpolator(sc)
        }

        // 继承自 ToShowOps：
        trait ToShowOps {
            implicit def toShow[A](target: A)(implicit tc: Show[A]): Ops[A] = new Ops[A] {
                val self = target
                val typeClassInstance = tc
            }
        }
      * ToShowOps.toShow 是一个隐式转换, 他为 Pizza 动态绑定隐式参数 Show instance
      *
      * */
    import cats.syntax.show._  //the interface syntax


    /** 3）instance：
      *
      * 第二步 ToShowOps.toShow 隐式绑定的对象是我们要实现的 typeclass 具体的工作 instance. 我们通过 object Show.show 来生成.
      *
      * show 方法接受一个函数参数，这个参数含有是对具体类型的实现，通过这个工厂方法生成 Show 的具体的 instance.
      *
        object Show {
            def show[A](f: A => String): Show[A] = new Show[A] {
                def show(a: A): String = f(a)
            }
        }
      *
      * Show.show 实际上等价于 apply，或 Monad.point, 它生成一个实现了算法的具体 Show 实例, 并隐式提供给 2) syntax interface 的
      * ToShowOps.toShow 作为参数. 而 ToShowOps.toShow 通过隐式转换为 Pizza 绑定了这个 show 方法。
      * */
    implicit val pizzaShow = Show.show[Pizza] { p =>
        s"""|Pizza(${p.crustSize}, ${p.crustType}),
            |      toppings = ${p.toppings}""".stripMargin
    }

    /**
      * 4) 于是我们可以直接(实际上是通过 syntax interface )对 Pizza 使用上面这个隐式 instance 了.　
      *
      * 注意：这个 show 不是工厂方法 Show.show，而是它传入的函数参数 p．
      * */
    val p = pizza.show
}



/** 辅助类，用于生成Pizza*/
object PizzaHubModels {
    sealed trait Topping
    case object Cheese extends Topping
    case object Pepperoni extends Topping
    case object Sausage extends Topping
    case object Mushrooms extends Topping
    case object Onions extends Topping

    sealed trait CrustSize
    case object SmallCrustSize extends CrustSize
    case object MediumCrustSize extends CrustSize
    case object LargeCrustSize extends CrustSize

    sealed trait CrustType
    case object RegularCrustType extends CrustType
    case object ThinCrustType extends CrustType
    case object ThickCrustType extends CrustType
    case class Pizza(
                            crustSize: CrustSize,
                            crustType: CrustType,
                            toppings: Seq[Topping]
                    )
}