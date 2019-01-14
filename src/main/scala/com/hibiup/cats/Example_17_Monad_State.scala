package com.hibiup.cats

/** *
  * State Monad 是用于保存状态的 Monad。详细说明参见：
  *
  * https://github.com/hibiup/FreeMonad/blob/master/src/main/scala/scalaz/examples/Example_2_State_Monad.scala
  */
object
 Example_17_Monad_State {
    def state_example(): Unit = {
        /**
          * State Monad 是一个用于保存或传递状态（结果）值的范式
          *
          * State 的定义如下：
          *
             State[S, A](f: S => (S, A))
          *
          *   + 参数是状态转移函数，这个函数接受一个初始状态 S，返回值是新的状态 S 和可能对下一次状态转换产生影响的要素 A. 比如
          * 随机数生成函数，每次生成的随机数是新的状态 S，而用于下次生成新随机数的种子就是 A。当然，如果每次转换都是独立的，
          * 那么可以没有A值返回。或者 A 值也可以用于记录附加信息，例如日志信息等。
          *
          *   + State Monad 是一个函数（monad）而不是 value（monoid），因此它本身并不存放状态，它只是用于状态计算，因此在使用
          * 的时候是先定义（传入状态转换函数），然后得到 monad 实例，在使用的时候调用 monad 实例的 run 函数传入状态值，最后
          * 通过 value 得到新的状态输出。
          * */

        /** 1）定义一个 State Monad，输入 Int 类型状态数据，返回 (Int, String) 。在这个例子中我们暂时不作状态转换，只介绍接口。*/
        import cats.data.State
        val stateMonad = State[Int, String] { state =>
            (state, s"The state is $state")
        }

        /** 2）生成状态为 10 的 state monad 实例(实际上是返回一个 Eval monad 实例。参见 Example_14_Monad_Eval ) */
        val state1 = stateMonad.run(10)

        /** 3）重新获取回状态值和返回值 */
        val (s1, a1) = state1.value
        import cats.syntax.eq._
        import cats.instances.int._
        assert(s1 === 10)
        import cats.instances.string._
        assert(a1 === "The state is 10")

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
        val (s3, a3) = stateMonad2.run(10).value   // result  会被设置为和 state 相同。（result = state:cats.Id？）
        assert(s3 === a3)
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

    /**
      * state monad 是设计用来做状态转换的，并且状态可以被传递。将多个 state monad 通过 flatMap 串联起来，前一个的输出 S 会
      * 自动成为下一个的输入值，由此可以将状态透明传递下去。以下这个例子演示简单的状态转换和传递。
      * */
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
            r1 <- step1        // flatMap 或 map 将会显示获得 A,而S会被隐式传递到下一个 state monad
            r2 <- step2        // step2 会隐式获得上一个输出的 S 值
            r3 <- step3        // 最终 S 值运算：state:Int = (20+1)*2-10
            /** 将前几次的 A值全都返回(以Tuple3的形式)。（当然如果不关心的话也可以不返，并不影响 state）并和最终
              * 的 state 组装成新的 state monad: steps */
        } yield(r1, r2, r3)

        /** 4) 执行 steps monad 给出初始状态并得到结果。（结果类型将会是 (Int, Tuple2[String, String， String]) ）*/
        val (st1, re1) = steps.run(20).value
        import cats.syntax.eq._
        import cats.instances.int._
        assert(32 === st1)
        import cats.instances.string._
        assert("Result of step2: 42" === re1._2)  // 检查第二次运行的 result

        /** 等价于以下，注意 S 值是被隐式传递的，因此我们只需考虑 A 值是否需要被记录。 */
        val (st2, re2) = step1.flatMap(r1 => step2.flatMap(r2 => step3.map{r3 =>
            (r1, r2, r3)
        })).run(20).value
        println(st2, re2)
    }
}
