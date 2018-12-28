package com.hibiup.cats

object Example_16_Monad_Reader {

    /**
      * Reader Monad 用于从外部得到输入数据。
      * */
    def reader_example(): Unit = {
        import cats.data.Reader
        case class Cat(name: String, favoriteFood: String)

        /**
          * 和 Writer 一样，Reader 有两个类型参数，分别是输入，和输出。参数是一个输入到输出的函数，例如下面这个例子：
          *
          * catName是一个 Reader，输入是一个 Cat 类型，输出是 String，函数参数就是从 Cat 中提取出 Name
          **/

        /** 1) 定义 Reader，给出输入、输出参数类型；和输入到输出的算法。*/
        val catName: Reader[Cat, String] = Reader(cat => cat.name)

        /**
          * 2）使用 Reader 的时候通过 run 函数将参数传递给 Reader
          * */
        import cats.syntax.eq._
        import cats.instances.string._
        assert("Mimi" === catName.run(Cat("Mimi", "Fish")))
        // 可以省略 run
        assert("Mimi" === catName(Cat("Mimi", "Fish")))

        /** 和 Writer 一样，Reader 是 ReaderT 的 alias，ReaderT 有三个参数
          *
        type Reader[A, B] = cats.data.ReaderT[cats.Id, A, B]
          *
          * 第一个参数是 cats.Id，Reader 直接将输出转换成 cats.Id 作为 ReaderT 的 Id，可以通过 run 得到：
          *
          * */
        assert(catName.run(Cat("Mimi", "Fish")).isInstanceOf[cats.Id[String]] )
    }

    def reader_map_and_flatMap() = {
        import cats.data.Reader
        case class Cat(name: String, favoriteFood: String)
        val catName: Reader[Cat, String] = Reader(cat => cat.name)

        /**
          * Reader 的强大还在于它可以通过 map 和 flatMap 串联起来生成复合 Reader，例如我们可以将第一个 Reader map 后得到新的 Reader：
          * */
        val catGreets: Reader[Cat, String] = catName.map(name => s"Hello ${name}")

        import cats.syntax.eq._
        import cats.instances.string._
        assert(catGreets.run(Cat("Kitty", "Junk Food")) === "Hello Kitty" )

        /** flatMap 可以处理更多级联： */
        val catSays: Reader[Cat, String] = Reader(cat => s"likes ${cat.favoriteFood}")

        val feedCat = for {
            name <- catGreets
            food <- catSays
        } yield s"$name $food!"

        val mimi = Cat("Mimi", "Fish")
        assert("Hello Mimi likes Fish!" === feedCat(mimi))

        /** 等价于：*/
        val _feedCat = catGreets.flatMap{name =>
            catSays.map{food =>
                s"$name $food!"
            }
        }
        assert(feedCat(mimi) == _feedCat(mimi))
    }

    def read_form_database(): Unit = {
        /** 模拟一个数据库 */
        case class User( username: String, password: String )
        type DB = Map[Int, User]
        def UserTable:DB = Map[Int, User](
            1 -> User("UserA", "PasswordA"),
            2 -> User("UserB", "PasswordB")
        )

        /** 1) 定义一个 Reader，作用是从数据库中取出记录, Reader 的预定义参数是数据库,预期返回 A 类型记录 */
        import cats.data.Reader
        type DbReader[A] = Reader[DB, A]

        /** 2) 定义一个函数,函数传入记录 id,得到 Reader[_, Option[User]]
          *    注意：不是直接得到纪录, 而是预期这个 Reader 将在执行期传入数据库, 返回记录 */
        def findUsername(userId: Int): DbReader[Option[User]] = Reader(db => db.get(userId))

        import cats.Eq
        import cats.syntax.eq._
        import cats.instances.string._
        implicit val UserMonoid = new Eq[User]{
            def eqv(x: User, y: User): Boolean =
                (x.username === y.username) && (x.password === y.password)
        }  // 自定义类的 Eq 实例. 另一个例子参见 Example_4_Eq

        /** 3) 执行函数,得到 Reader. 然后给 Reader.run 传入数据库 => 得到记录 */
        findUsername(2).run(UserTable).map{user =>
            assert(user === User("UserB","PasswordB"))
        }
    }
}
