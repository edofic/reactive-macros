# reactive-mongo-macros

macros for serialization and deserialization of case classes into reactive mongo BSON

**WARNING** This is currently very experimental. You've been warned. Not let's have some fun.

The idea is to fully automate creation of BSONReader and BSONWriter for case classes. Since writing this code is tedious and repetitive the only logical conclusion is to have a computer write it. Also relevent: DRY principle.

Inspired by Play! Framework's JSON macros(see [JSON Macro Inception](http://www.playframework.com/documentation/2.1.0/ScalaJsonInception)) so the API tends to be similar. Well there's barely any API and that's the point. 'Nuff talking let's see some code

## Usage


    import com.edofic.reactivemacros.FormatBSON
    import reactivemongo.bson.handlers.{BSONWriter, BSONReader}

    case class Person(firstName: String, lastName: String)
    val formatter: BSONWriter[Person] with BSONReader[Person] = FormatBSON[Person]

And simply add dependency to your build settings(build.sbt or project/Build.scala)

    resolvers += "edofic snapshots" at "http://edofic.github.com/repository/snapshots"
    libraryDependencies += "com.edofic" %% "reactivemacros" % "0.1-SNAPSHOT"

And there you have your instances. Behind the scenes is some macro magic you're welcome to check out. Also see tests(src/test/scala/) for more samples. To recap: you can also use `ReadBSON[Foo]` and `WriteBSON[Foo]` if you only need one-way conversion.

Oh yeah, it plays along with existing instances - it picks them up from the implicit scope. And you can define readers and writers for raw values too using `ReadBSON` and `WriteBSON` type classes.


### Supported

- double
- string
- boolean
- int
- long
- nesting
- Seq
- Option
- case classes can also be defined inside objects, traits or classes
- You may overload apply method on the companion object and the macro will still pick the right one
- union types(see below)

### Unsupported for now

- recursive structures
- case classes cannot be defined inside functions

## Advanced usage

To just get a "magic" converter you use `FormatBSON[Foo]` which desugars to invocation of `apply` method on FormatBSON object. However this is equvalent to `FormatBSON.custom[Foo, Options.Default]` which also allows for some additional options. Options are specified as types since types are *the* thing you use when compiling code and it makes sense to encode your information into types. Suppoted options are declared in `com.edofic.reactivemacros.Options`. Note that each option is a trait that may extend some other options. This explicitly declares which option depends on what oter options so you don't need to specify everything when invoking the macros.

#### Options.Default
Default settings. Also the base trait for other options

#### Options.Verbose
When Verbose is given the generated code will be printed at compilation. It is *approximately* scala code. Automatically generated from the AST it reads quite well(it may be a bit verbose) but may not neccesasyily compile(missing quotes on strings and stuff like that)

#### Options.SaveClassName
Appends additional attribute to the generated BSON(only applies to write). Property name is className and it constains a BSONString with the class name

#### Options.UnionType
Creates a converter for an algebraic data type(but you can use it for other stuff too).
In general you want a top (preferably sealed) trait and a few implementing case classes. Then you can derive a FormatBSON for the trait that will now about the case classes and serialize them accordingly. It uses SaveClassName to store type information and deserialize correctly. You can use this to store different(hopefully similar) documents inside a single collection simply.
Quick usage sample(names are arbitrary):

    trait Parrent
    case class FooChild(s: String) extends Parrent
    case class BarChild(n: Int) extends Parrent

    import com.edofic.reactivemacros.FormatBSON
    import com.edofic.reactivemacros.Options._
    val formatter = FormatBSON.custom[Parrent, UnionType[FooChild \/ BarChild]]

## BSON literals
Writing queries for mongo can be a bit boiler-platey so I've included a macro that parses a string literal ar compile time and transforms it into code that constructs the BSONValue-based query. No runtime parsing whatsoever! This means it's as fast as writing the queries by hand and completley safe - any errors will be caught at compile time. And it also does something similar to scala's string interpolations. Just the syntax is a bit different because some of the limitations with interpolators.
See some examples below

    import com.edofic.reactivemacros.LiteralBSON
    LiteralBSON("""{hello: "world", "$get": 1.13, doc: {f: 2}, float: -1}""")

The latter line compiles exactly the same as hand writing it out

    BSONDocument(
      "hello" -> BSONString("world"),
      "$get" -> BSONDouble(1.13),
      "doc" -> BSONDocument("f" -> BSONInteger(2)),
      "float" -> BSONInteger(-1)
    )

You can also reference BSONValues in scope

    val v = BSONString("hai")
    val lite = LiteralBSON("""{value:$v}""")
    val hand = BSONDocument("value" -> v)

For conveniance I've included an implicit conversions that boxes any value(given a writer-WriteBSON for its type is available in implicit scope) into appropriate BSONValue

    import WriteBSON.any2BSONValue
    val v = "hai"
    val lite = LiteralBSON("""{value:$v}""")
    val hand = BSONDocument("value" -> BSONString(v))

And you can also include arbitrary scala expressions(alsmost - your code may not contain $$) and this is the point that differs from standard scala string interpolation. Code is delimited by $$ on both sides.

    LiteralBSON("""{value: $$ 1+1 $$}""")
    //"desugars" into
    BSONDocument("value" -> BSONInteger(1+1))