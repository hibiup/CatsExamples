package com.hibiup.cats

/** *
  * State Monad 是用于保存状态的 Monad。详细说明参见：
  *
  * https://github.com/hibiup/FreeMonad/blob/master/src/main/scala/scalaz/examples/Example_2_State_Monad.scala
  */
object Example_17_Monad_State {
    def state_example(): Unit = {
        /**
          * State 的定义如下：
          *
             State[S, A]
          *
          * 第一个参数是状态值 A，第二个是返回值的第二个要素 S。完整的返回值是 (A, S)
          *
          * 和 Reader, Writer monad 类似，cats.data.State 是生成 State monad 的工厂类。传入一个满足类型参数的函数参数，返回一个
          * Monad 实例，然后这个 Monad 实例在应用中接受工厂类传入的函数参数的参数值来进行运算。
          * */

        /** 1）定义一个 State Monad，输入 Int 类型数据，返回 (Int, String) 。在这个例子中我们暂时不作状态转换，只介绍接口。*/
        import cats.data.State
        val stateMonad = State[Int, String] { state =>
            (state, s"The state is $state")
        }

        /** 2）生成状态为 10 的 state monad 实例(实际上是返回一个 Eval monad 实例。参见 Example_14_Monad_Eval ) */
        val state1 = stateMonad.run(10)

        /** 3）重新获取回状态值和返回值 */
        val (s1, r1) = state1.value
        import cats.syntax.eq._
        import cats.instances.int._
        assert(s1 === 10)
        import cats.instances.string._
        assert(r1 === "The state is 10")

        /** 3-1）或者只保留状态值 */
        val s2 = stateMonad.runS(20).value
        assert(s2 === 20)
        /** 3-2）或者只保留返回值 */
        val r2 = stateMonad.runA(30).value
        assert(r2 === "The state is 30")

        /*************************
          *  可以定义一个只关心 state，不关心 result 的 Monad
          */
        val stateMonad2 = State.get[Int]
        /** run monad 并得到结果 */
        val (s3, r3) = stateMonad2.run(10).value   // result  会被设置为和 state 相同。（result = state:cats.Id？）
        assert(s3 === r3)
        val _s3 = stateMonad2.runS(10).value
        val __s3 = stateMonad2.runA(10).value
        assert(_s3 === __s3)

        /*************************
          * pure 方法得到一个只有 result 没有 state 的 monad（state 为零值（identity））
          */
        val stateMonad3 = State.pure[Int, String]("Result")
        println(stateMonad3.map(result => assert("Result" === result)))
        /** 设置 state，实际上是执行与 identity 的 combine 运算 */
        import cats.instances.tuple._
        assert((10, "Result") === stateMonad3.run(10).value)

        /****************************
          * inspect 和 pure 很类似，但是它会将 state 作为参数传给 result.
          */
        val stateMonad4 = State.inspect[Int, String]{x => s"Result: $x"}
        assert((10, "Result: 10") === stateMonad4.run(10).value)   // 无论 pure 还是 inspect 返回的都是 Eval lazy 运算

        /*****************************
          * modify 可以在返回 state 之前插入运算。
          */
        var stateMonad5 = State.modify[Int]{x => x * 2}.inspect[String]{x => s"Result: $x"}
        assert((20, "Result: 20") === stateMonad5.run(10).value)  // 注意　modify 和 inspect 出现的顺序对结果的影响

        stateMonad5 = State.inspect[Int, String]{x => s"Result: $x"}.modify[Int]{x => x * 2}
        assert((20, "Result: 10") === stateMonad5.run(10).value)  // 注意　modify 和 inspect 出现的顺序对结果的影响
    }

    /** state monad 是设计用来做状态转换的，并且状态可以被传递。以下这个例子演示简单的状态转换和传递。*/
    def state_composing_and_transforming(): Unit = {
        import cats.data.State
        /** 1) 定义  state monad */
        val step1 = State[Int, String] { num =>
            /** 1-2) 实现状态转换运算 */
            val ans = num + 1
            /** 1-3) 返回新的状态(和返回值) */
            (ans, s"Result of step1: $ans")
        }

        /** 2) 定义一个新的 State monad */
        val step2 = State[Int, String] { num =>
            val ans = num * 2
            (ans, s"Result of step2: $ans")
        }

        val step3 = State[Int, String] { num =>
            val ans = num - 10
            (ans, s"Result of step3: $ans")
        }

        /** 3）定义传递状态的 Monad.
          *
          *   状态 s 将会被传递并累计运算，而每一次运算的结果 r1, r2. r3 会作为 tuple 赋值给最终的 state monad
          * */
        val steps = for {
            r1 <- step1        // flatMap 或 map 将会获得 result
            r2 <- step2        // 并且 state 值会被传递
            r3 <- step3        // state:Int = (20+1)*2-10
        } yield(r1, r2, r3)    // 将前几次的 result 全都返回。（当然也可以不返回任何结果）并和最终的 state 组装成新的 state monad: steps

        /** 4) 执行 steps monad 给出初始状态并得到结果。（结果类型将会是 (Int, Tuple2[String, String， String]) ）*/
        val (st1, re1) = steps.run(20).value
        import cats.syntax.eq._
        import cats.instances.int._
        assert(32 === st1)
        import cats.instances.string._
        assert("Result of step2: 42" === re1._2)  // 检查第二次运行的 result
    }
}
