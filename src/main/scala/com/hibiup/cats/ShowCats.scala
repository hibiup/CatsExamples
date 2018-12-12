package com.hibiup.cats

import simulacrum.typeclass   // cats-mtl-core package

object Cats {

    /**
      * 1) 用 @typeclass annotation 定义一个 typeclass trait，这个 typeclass 中只包含 show 方法. @typeclass
      * 这个 macro annotation　将在编译期做以下几件事情：
      *
      *   1. 生成 Show 的伴生对象 object Show 和它的 apply 方法，这个 apply 用于返回 instance
      *   2. 在伴生对象中生成 Ops 和 AllOps traits ，实现所有转换运算符.（也就是生成 typeclass 的 instance）
      *   3. 添加一个名为　ToShowOps 的 trait 提供所有到 Ops 的隐式转换（也就是生成 Ops 的 interface）
      *   4. 生成一个名为 ops 的 object 提供所有到 AllOps　的隐式转换 .（也就是生成 AllOps 的 interface）
      *
      * 由此可以看到 Cats 通过 @typeclass 这个 annotation 动态地完成了所有 typeclass 对象的生成.　
      * */
    @typeclass trait Show[T] {
        def show(f: T): String
    }
}
