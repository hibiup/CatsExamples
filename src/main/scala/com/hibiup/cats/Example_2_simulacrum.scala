package com.hibiup.cats

/**
  * 这个例子演示 Cats 的 @imulacrum.typeclass macro 的动态效果。
  *
  * typeclass trait CanTruthy[A] 的作用是判断一个数是否为 0，如果为 0 返回true，否则false
  */

object Example_2_simulacrum {
    /**
      * 1）定义一个 typeclass:
      */
    import simulacrum._
    /** @imulacrum.typeclass macro 的作用是告诉编译器这是一个 typeclass，它来自 "org.typelevel" %% "cats-mtl-core" % version。
      * 为此需要在 build.sbt 中加入以下两行编译选项：
      *
      * 　　scalacOptions ++= Seq("-Xplugin-require:macroparadise")
      * 　　addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
      *
      * 它在编译时动态生成以下 object CanTruthy 它宝航 interface 所需的各项方法，为我们节约了大量人工代码：
      *
        object CanTruthy {
            def apply[A](implicit instance: CanTruthy[A]): CanTruthy[A] = instance

            trait Ops[A] {
                def typeClassInstance: CanTruthy[A]
                def self: A
                def truthy: A = typeClassInstance.truthy(self)
            }

            // 数据结构到 typeclass 的隐式 syntax 绑定
            trait ToCanTruthyOps {
                implicit def toCanTruthyOps[A](target: A)(implicit tc: CanTruthy[A]): Ops[A] = new Ops[A] {
                    val self = target
                    val typeClassInstance = tc
                }
            }

            trait AllOps[A] extends Ops[A] {
                def typeClassInstance: CanTruthy[A]
            }

            object ops {
                implicit def toAllCanTruthyOps[A](target: A)(implicit tc: CanTruthy[A]): AllOps[A] = new AllOps[A] {
                    val self = target
                    val typeClassInstance = tc
                }
            }
        }
      *
      **/
    @typeclass trait CanTruthy[A] { self =>
        def truthy(a: A): Boolean
    }

    /**
      * 2) 定义工厂方法。期待获得应用时传入的具体实现函数，获得最终的 instance
      * */
    object CanTruthy {
        def fromTruthy[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
            def truthy(a: A): Boolean = f(a)
        }
    }
}

/**
  * 应用时
  */
object Client {
    /**
      * 3) import 进 @imulacrum.typeclass macro 自动生成的代码
      * */
    import com.hibiup.cats.Example_2_simulacrum.CanTruthy.ops._
    import com.hibiup.cats.Example_2_simulacrum.CanTruthy

    /**
      * 4）提供应用时所需的具体实现方法。
      *
      * fromTruthy 这个方法是 @imulacrum.typeclass 在编译时动态生成的工厂方法。
      * */
    implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.fromTruthy({
        case 0 => false
        case _ => true
    })

    /**
      * 5) 第三步 import 进来的自动生成的隐式转换将对象和方法绑定在一起．
      * */
    val r = 10.truthy
}