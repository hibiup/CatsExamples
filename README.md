# 基本概念：

## typeclass

typeclass 模式有三部分组成:类型类（typeclass）本身、具体类型的实例（instance）、最终暴露给用户的使用接口（interface）。

### 类型类本身（typeclass）

在 Cats里，`类型类（typeclass）`本身是用至少有一个类型参数的 `trait` 实现。因此 typeclass 一定是 trait，并且带类型参数。比如：
~~~
trait JsonWriter[A]{
  def write(value:A):Json
}
~~~

一般来讲这个类型类不包含实现代码，更像 Java 中的interface。但是Cats的类型类中对 interface 这个单词有其它定义，它往往指的是
“暴露给用户去实现的方法".

### 类型的实例（instance）

typeclass 的具体实例提供了面向具体数据类型的特定方法，它继承并实例化了抽象的 typeclass。并且，一一般会给它们贴上隐式 implicit 关
键字：

例1）
~~~
implicit val stringJsonWriter:JsonWriter[String]= new JsonWriter[String] {
    override def write(value: String) = JsString(value)
}
~~~

例2）
~~~
implicit val personJsonWriter:JsonWriter[Person]= new JsonWriter[Person] {
    override def write(value: Person) = JsObject(Map(
        "name"->JsString(value.name),
        "email"->JsString(value.email)
    ))
}
~~~

### 用户使用接口（interface）

interface指的是把类型类的实例（instance）作为隐式参数的泛型方法，它们是用户最终直接使用的 API 接口。也就是说，interface 
是 instance 的用户。

interface 又分为 interface object 和 interface syntax 两类。

#### interface object

interface 既然是用户可以直接使用的，因此它们一般也是实例化的，因此一般定义成 object，然后在其内定义接口方法，这些接口方法 implicitly
地参数化使用 instance。例如：

~~~
object Json{
  def toJson[A](value:A)(implicit w:JsonWriter[A]):Json=w.write(value)
}
~~~

然后我们就可以这样使用它：

~~~
import JsonWriterInstances.personJsonWriter        <-- 获得隐式参数
Json.toJson(Person("hang","hangscer@gmail.com"))   <-- 使用 interface
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

然后转换对象就可以直接使用 toJson 方法：

~~~
import JsonWriterInstances._
import JsonSyntax.JsonWriterOps        <-- 获得隐式 syntax
val r1=Person("hang","email").toJson   <-- 使用 interface
~~~

Cats　就在这个基础上实现将类型类（typeclass）、具体类型实例(instance)以及接口(interface)模块化．

# 参考：
https://nrinaudo.github.io/2015/12/13/tcgu-part-5.html
https://blog.csdn.net/hangscer/article/details/78375127
