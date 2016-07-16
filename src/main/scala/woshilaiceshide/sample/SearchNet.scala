package woshilaiceshide.sample

import anorm._
import anorm.SqlParser._

import java.sql.Connection

object SearchNet {
  final case class Link(fromid: Int, toid: Int, strength: Double)
  object Link {
    val model = int("fromid") ~ int("toid") ~ double("strength") map {
      case fromid ~ toid ~ strength => Link(fromid, toid, strength)
    }
  }
  def dtanh(x: Double) = 1.0d - x * x
  implicit class Range(val x: Int) extends AnyVal { def range = 0 until x }
}

class SearchNet(db_name: String) {
  import SearchNet._
  val db = DB.newHikari("org.sqlite.JDBC", s"jdbc:sqlite:${db_name}")
  def shutdown() = db.shutdown()
  private def show: RowParser[Unit] = null

  def get_all_wordhidden() = db.withConnection { implicit c =>
    SQL("""select * from wordhidden""").as(Link.model.*)
  }

  def get_all_hiddenurl() = db.withConnection { implicit c =>
    SQL("""select * from hiddenurl""").as(Link.model.*)
  }

  def get_all_hiddennode() = db.withConnection { implicit c =>
    SQL("""select * from hiddennode""").as(str("create_key").*)
  }

  def make_tables() = {
    db.withConnection { implicit c =>
      SQL("""create table if not exists hiddennode(create_key)""").executeUpdate()
      SQL("""create table if not exists wordhidden(fromid,toid,strength)""").executeUpdate()
      SQL("""create table if not exists hiddenurl(fromid,toid,strength)""").executeUpdate()
    }
  }

  //layer: wordhidden or hiddenurl
  def get_strength(fromid: Int, toid: Int, layer: String) = {
    val existed = db.withConnection { implicit c =>
      SQL(s"""select strength from ${layer} where fromid = {fromid} and toid = {toid}""")
        .on('fromid -> fromid, 'toid -> toid)
        .as(double("strength").singleOpt)
    }
    existed.getOrElse(if (layer == "wordhidden") -0.2 else /*hiddenurl*/ 0)
  }

  def set_strength(fromid: Int, toid: Int, layer: String, strength: Double)(implicit c: Connection) = {
    val existed = SQL(s"""select rowid from ${layer} where fromid = {fromid} and toid = {toid}""")
      .on('fromid -> fromid, 'toid -> toid)
      .as(int("rowid").singleOpt)

    existed match {
      case None =>
        SQL(s"""insert into ${layer}(fromid, toid, strength)values({fromid}, {toid}, {strength})""")
          .on('fromid -> fromid, 'toid -> toid, 'strength -> strength)
          .executeInsert()
      case Some(rowid) =>
        SQL(s"""update ${layer} set strength = {strength} where rowid = {rowid}""")
          .on('strength -> strength, 'rowid -> rowid)
          .executeUpdate()
    }
  }

  def generate_hiddennode(wordids: Seq[Int], urls: Seq[Int]) = {
    require(wordids.size <= 3)
    val create_key = wordids.sorted.mkString("_")
    db.withTransaction { implicit c =>
      val existed = SQL(s"""select rowid from hiddennode where create_key = {create_key}""")
        .on('create_key -> create_key)
        .as(int("rowid").singleOpt)

      if (existed.isEmpty) {
        val hiddenid = SQL(s"""insert into hiddennode(create_key)values({create_key})""")
          .on('create_key -> create_key)
          .executeInsert(SqlParser.scalar[Int].singleOpt)

        hiddenid.map { id =>
          for (wordid <- wordids) set_strength(wordid, id, "wordhidden", 1.0d / wordids.size)
          for (urlid <- urls) set_strength(id, urlid, "hiddenurl", 0.1d)
        }
      }
    }
  }

  def get_all_hiddenids(wordids: Seq[Int], urlids: Seq[Int]) = {
    val l0 = wordids.flatMap { wordid =>
      db.withConnection { implicit c =>
        SQL(s"""select toid from wordhidden where fromid = {fromid}""").on('fromid -> wordid).as(int("toid").*)
      }
    }

    val l1 = urlids.flatMap { urlid =>
      db.withConnection { implicit c =>
        SQL(s"""select fromid from hiddenurl where toid = {toid}""").on('toid -> urlid).as(int("fromid").*)
      }
    }

    (l0 ++ l1).distinct
  }

  final class Trained(val wordids: Seq[Int], hiddenids: Seq[Int], val urlids: Seq[Int], wo: Seq[Seq[Double]], wi: Seq[Seq[Double]]) {
    def update_database() = db.withTransaction { implicit c =>
      wordids.size.range.map { i =>
        hiddenids.size.range.map { j => set_strength(wordids(i), hiddenids(j), "wordhidden", wi(i)(j)) }
      }
      hiddenids.size.range.map { i =>
        urlids.size.range.map { j => set_strength(hiddenids(i), urlids(j), "hiddenurl", wo(i)(j)) }
      }
    }
  }

  final class Relevant(val wordids: Seq[Int], val urlids: Seq[Int]) {

    val hiddenids = get_all_hiddenids(wordids, urlids)

    val wi = wordids.map { wordid =>
      hiddenids.map { hiddenid =>
        get_strength(wordid, hiddenid, "wordhidden")
      }
    }

    val wo = hiddenids.map { hiddenid =>
      urlids.map { urlid =>
        get_strength(hiddenid, urlid, "hiddenurl")
      }
    }

    val ai = Seq.fill(wordids.size)(1.0)
    val ah = hiddenids.size.range.map { i =>
      val tmp = wordids.size.range.map { j =>
        ai(j) * wi(j)(i)
      }.sum
      java.lang.Math.tanh(tmp)
    }
    val ao = urlids.size.range.map { i =>
      val tmp = hiddenids.size.range.map { j =>
        ah(j) * wo(j)(i)
      }.sum
      java.lang.Math.tanh(tmp)
    }

    def feed_forward() = ao

    def backpropagation(targets: Seq[Double], N: Double = 0.5d) = {
      val output_deltas = urlids.size.range.map { i =>
        val error = targets(i) - ao(i)
        dtanh(ao(i)) * error
      }

      val hidden_deltas = hiddenids.size.range.map { i =>
        val error = urlids.size.range.map { j =>
          output_deltas(j) * wo(i)(j)
        }.sum
        dtanh(ah(i)) * error
      }

      val wo1 = hiddenids.size.range.map { i =>
        urlids.size.range.map { j =>
          val change = output_deltas(j) * ah(i)
          wo(i)(j) + N * change
        }
      }

      val wi1 = wordids.size.range.map { i =>
        hiddenids.size.range.map { j =>
          val change = hidden_deltas(j) * ai(i)
          wi(i)(j) + N * change
        }
      }

      new Trained(wordids, hiddenids, urlids, wo1, wi1)
    }
  }

  private def setup_network(wordids: Seq[Int], urlids: Seq[Int]) = new Relevant(wordids, urlids)

  def get_result(wordids: Seq[Int], urlids: Seq[Int]) = setup_network(wordids, urlids).feed_forward()

  def train_query(wordids: Seq[Int], urlids: Seq[Int], selected_url: Int) = {
    generate_hiddennode(wordids, urlids)
    val relevant = setup_network(wordids, urlids)
    relevant.feed_forward()
    val targets = urlids.map {
      case x if x == selected_url => 1.0d
      case _ => 0.0d
    }
    val trained = relevant.backpropagation(targets)
    trained.update_database()
  }

}
