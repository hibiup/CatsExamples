package com.hibiup.cats

/**
  * （p78） - Monad 与 Functor 的不同，或者说 flatMap 和 map 的不同是，map 在序列化运算过程中，它只处理数据本身的复杂性，
  * 它无法保证容器的复杂性也能够被处理，因此它也就不能保证序列化的持续执行．（Monadic 编程就是不断将数据从容器中取出 ->　处理
  * ->　打包 -> 再取出 -> 再处理 -> 再打包 -> ...的过程，因此必须考虑打包的复杂性．）
  * */
object Example_10_Monad {
    /**
      * 以 Option Monad 为例：
      * （Option 实现了 may or may not 运算．详细解释参考: https://github.com/hibiup/FreeMonad）
      *
      * 输入 string number，实现除法运算．
      * */
    def option_monad() = {
        def parseInt(str: String): Option[Int] = scala.util.Try(str.toInt).toOption
        def divide(a: Int, b: Int): Option[Int] = if(b == 0) None else Some(a / b)

        def stringDivideBy(aStr: String, bStr: String): Option[Int] =
            // 将除数和被除数 string => int
            parseInt(aStr).flatMap { aNum =>
                parseInt(bStr).flatMap { bNum =>
                    // 执行除法运算
                    divide(aNum, bNum)
                }
            }
        stringDivideBy("4", "2").foreach(result => println(result))

        def stringDivideByWithMap(aStr: String, bStr: String) =
        // 将除数和被除数 string => int
            parseInt(aStr).map { aNum =>
                parseInt(bStr).map { bNum =>
                    // 执行除法运算
                    divide(aNum, bNum)
                }
            }
        stringDivideByWithMap("4", "2").foreach(result => println(result))
    }
}
