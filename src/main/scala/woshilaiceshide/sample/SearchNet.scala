package woshilaiceshide.sample

import anorm._
import anorm.SqlParser._
import anorm.SqlQuery._

object SearchNet {
  final case class Link(fromid: Int, toid: Int, strength: Double)
  object Link {
    val model = int("fromid") ~ int("toid") ~ double("strength") map {
      case fromid ~ toid ~ strength => Link(fromid, toid, strength)
    }
  }
}

class SearchNet(db_name: String) {

  import SearchNet._

  val db = DB.newHikari("org.sqlite.JDBC", s"jdbc:sqlite:${db_name}")

  def shutdown() = db.shutdown()

  private def show: RowParser[Unit] = null

  def show_wordhidden() = {
    val links = db.withConnection { implicit c =>
      SQL("""select * from wordhidden""").as(Link.model.*)
    }
    links.map { println(_) }
  }

  def show_hiddenurl() = {
    val links = db.withConnection { implicit c =>
      SQL("""select * from hiddenurl""").as(Link.model.*)
    }
    links.map { println(_) }
  }

  def show_hiddennode() = {
    val nodes = db.withConnection { implicit c =>
      SQL("""select * from hiddennode""").as(str("create_key").*)
    }
    nodes.map { println(_) }
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
    existed match {
      case None => if (layer == "wordhidden") -0.2 else /*hiddenurl*/ 0
      case Some(x) => x
    }

  }

  def set_strength(fromid: Int, toid: Int, layer: String, strength: Double) = {
    val existed = db.withConnection { implicit c =>
      SQL(s"""select rowid from ${layer} where fromid = {fromid} and toid = {toid}""")
        .on('fromid -> fromid, 'toid -> toid)
        .as(str("rowid").singleOpt)
    }
    existed match {
      case None =>
        db.withConnection { implicit c =>
          SQL(s"""insert into ${layer}(fromid, toid, strength)values({fromid}, {toid}, {strength})""")
            .on('fromid -> fromid, 'toid -> toid, 'strength -> strength)
            .executeInsert()
        }
      case Some(rowid) =>
        db.withConnection { implicit c =>
          SQL(s"""update ${layer} set strength = {strength} where rowid = {rowid}""")
            .on('strength -> strength, 'rowid -> rowid)
            .executeUpdate()
        }
    }
  }

  def generate_hiddennode(wordids: Seq[Int], urls: Seq[Int]) = {
    require(wordids.size <= 3)
    val create_key = wordids.sorted.mkString("_")
    val existed = db.withConnection { implicit c =>
      SQL(s"""select rowid from hiddennode where create_key = {create_key}""")
        .on('create_key -> create_key)
        .as(int("rowid").singleOpt)
    }
    if (existed.isEmpty) {
      val hiddenid = db.withConnection { implicit c =>
        SQL(s"""insert into hiddennode(create_key)values({create_key})""")
          .on('create_key -> create_key)
          .executeInsert(SqlParser.scalar[Int].singleOpt)
      }

      hiddenid.map { id =>
        for (wordid <- wordids) {
          set_strength(wordid, id, "wordhidden", 1.0 / wordids.size)
        }
        for (urlid <- urls) {
          set_strength(id, urlid, "hiddenurl", 0.1)
        }
      }
    }
  }

  def get_all_hiddenids(wordids: Seq[Int], urlids: Seq[Int]) = {
    val l0 = wordids.map { wordid =>
      db.withConnection { implicit c =>
        SQL(s"""select toid from wordhidden where fromid = {fromid}""").on('fromid -> wordid).as(int("toid").singleOpt)
      }
    }

    val l1 = urlids.map { urlid =>
      db.withConnection { implicit c =>
        SQL(s"""select fromid from hiddenurl where toid = {toid}""").on('toid -> urlid).as(int("fromid").singleOpt)
      }
    }

    (l0 ++ l1).filter { _ != None }.map { _.get }.distinct
  }

  class Relavant(val wordids: Seq[Int], val urlids: Seq[Int]) {

    val hiddenids = get_all_hiddenids(wordids, urlids)

    //val ai = Seq.fill(wordids.size)(1.0)
    //val ah = Seq.fill(hiddenids.size)(1.0)
    //val ao = Seq.fill(urlids.size)(1.0)

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

    def feed_forward() = {
      val ai = Seq.fill(wordids.size)(1.0)

      val ah = (0 until hiddenids.size).map { i =>
        val tmp = (0 until wordids.size).map { j =>
          ai(j) * wi(j)(i)
        }.sum
        java.lang.Math.tanh(tmp)
      }

      val ao = (0 until urlids.size).map { i =>
        val tmp = (0 until hiddenids.size).map { j =>
          ah(j) * wo(j)(i)
        }.sum
        java.lang.Math.tanh(tmp)
      }

      ao
    }

  }

  private def setup_network(wordids: Seq[Int], urlids: Seq[Int]) = {
    new Relavant(wordids, urlids)
  }

  def get_result(wordids: Seq[Int], urlids: Seq[Int]) = {
    setup_network(wordids, urlids).feed_forward()
  }

}
