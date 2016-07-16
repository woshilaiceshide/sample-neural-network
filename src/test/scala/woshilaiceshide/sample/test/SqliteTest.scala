package woshilaiceshide.sample.test

import woshilaiceshide.sample._

import anorm._
import anorm.SqlParser._

//https://github.com/brettwooldridge/HikariCP
//https://github.com/xerial/sqlite-jdbc
object SqliteTest extends App {

  final case class Person(id: Int, name: String)

  val db = DB.newHikari("org.sqlite.JDBC", "jdbc:sqlite:test.db")

  object Person {
    val model = int("id") ~ str("name") map {
      case id ~ name => Person(id, name)
    }
  }

  db.withConnection { implicit c =>
    SQL("""drop table if exists person""").executeUpdate()
    SQL("""create table person (id integer, name string)""").executeUpdate()
    SQL("""insert into person values(1, 'leo')""").executeUpdate()
    SQL("""insert into person values(2, 'yui')""").executeUpdate()
    val persons = SQL("""select * from person""").as(Person.model.*)

    println(s"persons: ${persons.mkString(", ")}")

  }
  db.shutdown()
}