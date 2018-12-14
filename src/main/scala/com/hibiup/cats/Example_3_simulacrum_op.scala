package com.hibiup.cats

/** 这个例子演示如何加入一个运算符号　“|+|”　*/
object Example_3_simulacrum_op {
    /** 1) 定义 typeclass */

    import simulacrum._

    @typeclass trait CanAppend[A] {
        /** @op annotation 告诉编译器将这个方法映射为一个操作。 */
        /*@op("|+|")*/ def append(a1: A, a2: A): A
        def |+|(a1: A, a2: A): A = append(a1, a2)
    }
}

object Example_3_simulacrum_op_Client {
    import com.hibiup.cats.Example_3_simulacrum_op.CanAppend
    /** 2）应用时提供实现部分　*/
    implicit val intCanAppend: CanAppend[Int] = CanAppend.apply[Int]( { (a1:Int, a2:Int) =>
        { a1 + a2 }
    } )

    import Example_3_simulacrum_op.CanAppend.ops._
    val r = 1 |+| 2
}
