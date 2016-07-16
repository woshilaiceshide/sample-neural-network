package woshilaiceshide.sample

import java.sql.Connection

import com.zaxxer.hikari._
import com.zaxxer.hikari.pool._

import com.jolbox.bonecp._

import scala.util.control.ControlThrowable

class DB(pool: { def getConnection(): Connection; def shutdown(): Unit }) {

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

  object Classifier extends scala.Enumeration {
    val BONECP = Value
    val HIKARI = Value

  }

  def newHikari(driver: String, jdbc_url: String, max_pool_size: Int = 1) = {
    Class.forName(driver)

    val config = new HikariConfig()
    config.setJdbcUrl(jdbc_url)
    config.setMaximumPoolSize(max_pool_size)
    val pool = new HikariPool(config)

    new DB(pool)
  }

  def newBonecp(driver: String, jdbc_url: String, max_pool_size: Int = 1) = {
    Class.forName(driver)

    val config = new BoneCPConfig()
    config.setJdbcUrl(jdbc_url)
    config.setPartitionCount(1)
    config.setMaxConnectionsPerPartition(1)
    config.setLogStatementsEnabled(true)
    val pool = new com.jolbox.bonecp.BoneCP(config)

    new DB(pool)
  }

}