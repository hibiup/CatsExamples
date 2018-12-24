package com.hibiup.cats

/**
  * Cats 的 Eval 允许我们抽象 eager， lazy 和 memorized 操作。
  *
  * 一般来讲，定义一个不变量：val，它被求值后就是 memorized的
  * 而一个 def 则不是 memorized 的，它每次被调用的时候都会被再次求值。
  *
  * 通过和 eager 和 lazy的组合，我们可以得到以下 evaluation 操作：
  *
  **/
object Example_14_Eval {
    import cats.Eval

    def cats_eval(): Unit = {
        println("========================")
        val now = Eval.now{        // Eval immediately
            println("Eval.now")
            "now." + math.random()}

        val later = Eval.later{    // Eval lazy but only once
            println("Eval.later")
            "later" + math.random()
        }

        val always = Eval.always{  // Eval lazy without memorized
            println("Eval.always")
            "always" + math.random()
        }

        println("========================")
        println(now.value, later.value, always.value)
        println(now.value, later.value, always.value)
    }

    def eval_greeting(): Unit = {
        var animal = "cat"
        var place = "mat"

        val saying = Eval.always {
            println("Step 1"); s"The $animal"
        }.map { str =>
            println("Step 2"); s"$str sat on"
        /** memoize 方法会缓存以上结果，导致 always 失效 */
        }.memoize.map {
            str => println("Step 3"); s"$str the $place"
        }
        println(saying.value)

        animal = "dog"
        place = "bed"
        println(saying.value)
    }


    /**
      * Eval 一个非常重要的特性是它的 defer 方法是 stack 安全的，因为它是基于 Trampoline 的。下面这个例子中的递归是不安全的，
      * 将它传递给 defer 就能够实现安全调用。
      *
      * Eval.defer 接受一个 Eval[_] 类型参数。
      * */
    import cats.syntax.functor._          // 引进 map 隐式方法
    def safe_factorial(n:BigInt):Eval[BigInt] = {
        if(n > 1) {
            Eval.defer(safe_factorial(n - 1).map(_ * n))
        } else {
            Eval.now(n)
        }
    }

    /**
      * Eval.defer 是非常重要的避免 stack overflow 的方法，它可以作用于其他函数，比如 folder_right 定义如下:
      *
      * def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B
      *
      * 1）实现一个 defer 版本的 folder_right. 将所有的右操作数都改为 Eval[_], 将处理函数的返回值交由 Eval.defer
      *    包裹起来，就实现了堆栈安全。
      * */
    def safe_folder_right[A, B](list: List[A], init_value: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
        list match {
            case head :: tail =>
                Eval.defer(fn(head, safe_folder_right(tail, init_value)(fn)))
            case Nil =>
                init_value
        }

    /** 2）然后在 safe_folder_right 的外面实现正常的 folder_right 函数界面，需要注意的是它的内部调用安全版的 safe_folder_right
      *    时需要将右边参数映射为 Eval[_], 函数方法也需要做一层 map 映射。 */
    def foldRight[A, B](list: List[A], init_value: B)(fn: (A, B) => B): B =
        safe_folder_right(list, Eval.now(init_value))    // init_value => Eval.now(init_value)
        {
            (a, b) =>  b.map(fn(a, _))     // 为函数方法包裹一层 map 调用： fn(a + b) => b.map(fn(a + _)) 来实现类型转换。
        }.value
}
