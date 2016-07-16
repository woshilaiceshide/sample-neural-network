package woshilaiceshide.sample

import java.sql.Connection

import com.zaxxer.hikari._
import com.zaxxer.hikari.pool._

import scala.util.control.ControlThrowable

class DB(pool: HikariPool) {

  def withConnection[A](autocommit: Boolean = true)(f: Connection => A): A = {
    val c = pool.getConnection()
    c.setAutoCommit(autocommit)
    try {
      f(c)
    } finally {
      c.close()
    }
  }

  def withConnection[A](f: Connection => A): A = withConnection(true)(f)

  def withTransaction[A](f: Connection => A) = {
    val c = pool.getConnection()
    c.setAutoCommit(false)
    try {
      val a = f(c)
      c.commit()
      a
    } catch {
      case e: ControlThrowable =>
        c.commit()
        throw e
      case e: Throwable =>
        c.rollback()
        throw e

    } finally {
      c.close()
    }
  }

  def shutdown() = pool.shutdown()

}
object DB {

  def apply(driver: String, jdbc_url: String, max_pool_size: Int = 1) = {
    Class.forName(driver)

    val config = new HikariConfig()
    config.setJdbcUrl(jdbc_url)
    config.setMaximumPoolSize(max_pool_size)
    val pool = new HikariPool(config)

    new DB(pool)
  }

}