# 基本概念：

## typeclass

typeclass 模式有三部分组成:类型类（typeclass）本身、具体类型的实例（instance）、最终暴露给用户的使用接口（interface）。之所以要这样
设计是为了体现这样一个设计思想：数据与处理方法解耦。

* 类型类（typeclass）的作用是在设计时面向抽象类型提供抽象方法。
* interface 的作用是在设计时决定如何将方法与抽象类型绑定。

以上两类组件都在设计时定案。

* instance 是在应用时按预定的设计需求提供实现实体。
* 然后通过设计时提供的 interface 按预定方式执行绑定动作，完成调用。

这两个步骤在应用时发生。

通过以上结构我们就可以在应用时，才将关键函数过程通过 lambda 传入，这就实现了动态生成 instance 的能力。这个过程有点类似 Java 中在设计时
提供泛型 interface，然后在应用时通过匿名类提供具体类的实现方法一样。

具体过程例如：

### 类型类本身（typeclass）

在 Cats里，`类型类（typeclass）`本身是用至少有一个类型参数的 `trait` 实现。它相当于 Java 中的泛型Interface：
~~~
trait JsonWriter[A]{
  def write(value:A):Json
}
~~~

一般来讲这个类型类不包含实现代码。但是Cats的类型类中对 interface 这个单词有其它定义.

### 用户使用接口（interface）

在Java中，因为匿名类的绑定是 in-place 的，因此失去了灵活性。而 Scala 的隐式转换允许我们在设计时就实现对抽象类型的预先绑定。并且在设计时
就完成类型检查。因此 Cats 的 interface 指的是预先把类型类的实例（instance）作为隐式参数绑定的类。

interface 又分为 interface object 和 interface syntax 两类。

#### interface object

interface 既然是用户可以直接使用的，因此它们一般也是实例化的，因此一般定义成 object，然后在其内定义接口方法，这些接口方法 implicitly
地参数化使用 instance。例如：

~~~
object Json{
  def toJson[A](value:A)(implicit w:JsonWriter[A]):Json=w.write(value)
}
~~~

#### interface syntax

还有一种常见的定义 interface 的方式就是隐式转换，动态为目标加上新的方法，比如：
~~~
object JsonSyntax{
    implicit class JsonWriterOps[A](value:A){
        def toJson(implicit w:JsonWriter[A]):Json=w.write(value)
    }
}
~~~

### 类型的实例（instance）

进入应用时，我们才需要为 typeclass 提供面向具体数据类型的特定方法，它继承并实例化了抽象的 typeclass。类似在 Java 中在过程中实现的匿名类。但是
在 Scala 中一般会给它们贴上隐式 implicit 关键字，相比 Java 的匿名类，Scala 的隐式避免了匿名类必须 in-place 的缺点。

~~~
implicit val personJsonWriter:JsonWriter[Person]= new JsonWriter[Person] {
    override def write(value: Person) = JsObject(Map(
        "name"->JsString(value.name),
        "email"->JsString(value.email)
    ))
}
~~~

当然 `JsonWriter[Person]` 的函数体我们也可以通过 lambda 传入，参见 `PizzaHub` 例子。然后我们就可以这样使用它（object interface）：

~~~
import JsonWriterInstances.personJsonWriter        <-- 获得 object 的隐式参数
Json.toJson(Person("hang","hangscer@gmail.com"))   <-- 使用 instance
~~~

或（syntax interface）

~~~
import JsonWriterInstances._
import JsonSyntax.JsonWriterOps        <-- 获得隐式 syntax
val r1=Person("hang","email").toJson   <-- 使用 instance
~~~


# 参考：
https://nrinaudo.github.io/2015/12/13/tcgu-part-5.html
https://blog.csdn.net/hangscer/article/details/78375127
