# reactive-mongo-macros

macros for serialization and deserialization of case classes into reactive mongo BSON

**WARNING** This is currently very experimental. You've been warned. Not let's have some fun.

The idea is to fully automate creation of BSONReader and BSONWriter for case classes. Since writing this code is tedious and repetitive the only logical conclusion is to have a computer write it. Also relevent: DRY principle. 

Inspired by Play! Framework's JSON macros(see [JSON Macro Inception](http://www.playframework.com/documentation/2.1.0/ScalaJsonInception)) so the API tends to be similar. Well there's barely any API and that's the point. 'Nuff talking let's see some code

    import com.edofic.reactivemacros.FormatBSON
    import reactivemongo.bson.handlers.{BSONWriter, BSONReader}
    
    case class Person(firstName: String, lastName: String)
    val formatter: BSONWriter[Person] with BSONReader[Person] = FormatBSON[Person]

And simply add dependency to your build settings(build.sbt or project/Build.scala)

    resolvers += "edofic snapshots" at "http://edofic.github.com/repository/snapshots"
    libraryDependencies += "com.edofic" %% "reactivemacros" % "0.1-SNAPSHOT"

And there you have your instances. Behind the scenes is some macro magic you're welcome to check out. Also see tests(src/test/scala/) for more samples. 

Oh yeah, it plays along with existing instances - it picks them up from the implicit scope. And you can define readers and writers for raw values too using `ReadBSON` and `WriteBSON` type classes. 

There are some rough edges still:

### Supported 

- double
- string
- boolean
- int
- long
- nesting
- Seq
- Option
- case classes can be defined inside objects

### Unsupported

- recursive structures
- case classes cannot be defined inside other (case) classes or functions(something to do with the pointer to the parrent)
