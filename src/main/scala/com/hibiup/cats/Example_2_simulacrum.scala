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
            // 很显然这是 interface 中的工厂方法。期待获得应用时传入的具体实现函数，获得最终的 instance
            def apply[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
                def truthy(a: A): Boolean = f(a)
            }

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
      * 1-2) 以下这一步不是必须的，只是因为 idea 对 macros 的不支持，导致在 IDE 中出现误判，可以通过显式声明一个空的 object 来欺骗
      * idea。这一步不是必须的，不定义除了影响误判外，不影响编译或调试。真正影响 macros 发挥作用的是 build.sbt 中的配置。（参见上面）
      * */
    //object CanTruthy {
        /*def apply[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
            def truthy(a: A): Boolean = f(a)
        }*/
    //}
}

/**
  * 应用时
  */
object Client {
    /**
      * 2) import 进 @imulacrum.typeclass macro 自动生成的 object CanTruthy 工厂方法 apply 来生成 instance，参数是
      * truthy(a: A): Boolean 函数实体。如果没有 1-2) 这里会出现误判，认为 CanTruthy 不存在，但是错误可以忽略.
      *
      * */
    import com.hibiup.cats.Example_2_simulacrum.CanTruthy
    implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.apply[Int]({
        case 0 => false
        case _ => true
    })

    /**
      * 3) import 进@imulacrum.typeclass macro 自动生成的隐式转换，将对象和方法绑定在一起．如果没有 1-2) 这里会出现误判，
      * 认为 package　和 truthy 方法不存在，但是错误可以忽略.
      * */
    import com.hibiup.cats.Example_2_simulacrum.CanTruthy.ops._
    val r = 10.truthy
}