package com.hibiup.cats

/**
  * Functor 通常允许形成链式操作。这种 Functor 称为 Convariant functor。还有两类 Functor：
  *
  * Contravariant functor:  p62
  *    和 map 相反，Contravariant functor 需要实现 contramap 函数，参数是与 map 方法相反的操作。
  *
  * Invariant functor:
  *    Invariant functor 实现 imap 方法，这个函数，参数是 A => B 和 B => A 双向操作。
  */
object Example_9_Functor_Variant {
    import cats.Functor
    import simulacrum._

    /**
      * 一个 Printable Functor 通过 map 将数据转换成 String。 它的 contramap 则是一个逆过程, 它将某个类型[B] 逆变成 Printable[B]。
      * */
    def contravariant() {
        /**
          * 1）定义 Printable, 实现 contramap 方法.
          * */
        @typeclass trait Printable[A] { self =>
            def format(value: A): String
            /**
              * contramap 意味着根据给定的类型参数[B], 可以"还原"出新的 Printable[B].注意.返回Printable[B], 不是 Printable[A]
              * */
            def contramap[B](f: B => A): Printable[B] = new Printable[B] {
                override def format(value: B): String = self.format(f(value))
            }
        }

        /**
          * 2）两种方法获得 Printable:
          *
          * 2-1）显式定义 object Printable 实例获得
          * */
        object PrintableInt {
            def apply(f: Int => String) = new Printable[Int] {
                override def format(value: Int): String = f(value)
            }
        }
        val p1:Printable[Int] = PrintableInt(x => "Value is: \"" + x.toString + "\"!")

        /**
          * 2-2）（推荐）利用 Cats @typeclass 的 macro 获得.
          * */
        val p2 = Printable.apply[Int](x => "Value is: \"" + x.toString + "\"!")

        // 检验
        println(p2.format(100))                   // Value is: "100"!
        import cats.instances.string._
        import cats.syntax.eq._
        assert(p2.format(100) === p1.format(100))

        /**
          * 3）通过实例的 contramap 派生出 Boolean 的 Printable, 实现 B => A 的转换. 也有两种方式：
          *
          * 3-1）由 p1 直接"还原" 出 booleanPrintable1
          * */
        val booleanPrintable1 = p1.contramap[Boolean]{
            case true => 0
            case false => 1
        }

        // 检验
        println(booleanPrintable1.format(true))    // Value is: "0"!
        assert(booleanPrintable1.isInstanceOf[Printable[Boolean]])

        /**
          * 3-2）用 Cats.Contravariant "还原" 出 booleanPrintable1
          * */
        /*import cats.Contravariant
        import cats.syntax.contravariant._
        val booleanPrintable2 = Contravariant[Printable].contramap[Int, Boolean](p1){
            case true => 0
            case false => 1
        }
        println(booleanPrintable2.format(true))
        assert(booleanPrintable2.isInstanceOf[Printable[Boolean]])*/
    }

    /**
      * Invariant Factor 通过 imap 函数提供双向转变. 不仅从 Codec[A] => Codec[B], 还可以根据 B => Codec[B]:
      * */
    def invariant() {
        /**
          * 1）假设设计一个解码器(解码成 ASCII:Int 或还原成 A)，它可以实现正向与反向解码
          * */
        @typeclass trait Codec[A] { self =>
            def encode(c: A): Int
            def decode(a: Int): A
            /** 实现 imap 意味着可以根据给定的类型参数[B] 还原出新的双向解码器 Codec[B] */
            def imap[B](dec: A => B)(enc: B => A): Codec[B] = new Codec[B] {
                override def encode(c: B): Int = self.encode(enc(c))
                override def decode(a: Int): B = dec(self.decode(a))
            }
        }

        /** 2) 定义 object CharCodec 实例并获得隐式 */
        implicit object CharCodec extends Codec[Char] {
            override def encode(c: Char): Int = c.toByte
            override def decode(a: Int): Char = a.asInstanceOf[Char]
        }

        // 检验 CharCodec
        def encodechar(c:Char) = implicitly[Codec[Char]].encode(c)
        def decodechar(a:Int) = implicitly[Codec[Char]].decode(a)

        import cats.syntax.eq._
        import cats.instances.int._
        assert(97 === encodechar('a'))

        import cats.instances.char._
        assert('a' === decodechar(97))

        /** 3-1）从 CharCodec 的 imap 中 "还原" 出 BooleanCodec */
        val booleanCodec1:Codec[Boolean] = CharCodec.imap({
            case 't' => true
            case 'f' => false
        })({
            case true => 't'
            case false => 'f'
        })

        // 检验 BooleanCodec
        import cats.instances.boolean._
        def encodeboolean(b:Boolean) = booleanCodec1.encode(b)
        def decodeboolean(a:Int) = booleanCodec1.decode(a)
        assert(116 === encodeboolean(true))
        assert(false === decodeboolean(102))

        /** 3-2）利用 cats.Invariant "还原" 出 BooleanCodec */
        /*import cats.Invariant
        import cats.syntax.invariant._
        val booleanCodec2 = Invariant[Codec].imap(CharCodec)({
            case 't' => true
            case 'f' => false
        })({
            case true => 't'
            case false => 'f'
        })
        assert(116 === booleanCodec2.encode(true))
        assert(false === booleanCodec2.decode(102))*/
    }
}
