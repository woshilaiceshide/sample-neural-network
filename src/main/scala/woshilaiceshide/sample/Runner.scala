package woshilaiceshide.sample

import SearchNet._

object Runner extends App {

  val mynet = new SearchNet("nn.db")

  mynet.make_tables()

  val (wWorld, wRiver, wBank) = (101, 102, 103)
  val (uWorldBank, uRiver, uEarth) = (201, 202, 203)
  mynet.generate_hiddennode(Seq(wWorld, wBank), Seq(uWorldBank, uRiver, uEarth))

  println(s"wordhidden: ")
  println(mynet.get_all_wordhidden().map { _.toString }.mkString(System.lineSeparator()))

  println(s"hiddennode: ")
  println(mynet.get_all_hiddennode().map { _.toString }.mkString(System.lineSeparator()))

  println(s"hiddenurl: ")
  println(mynet.get_all_hiddenurl().mkString(System.lineSeparator()))

  println(s"before train #1...")
  println(mynet.get_result(Seq(wWorld, wBank), Seq(uWorldBank, uRiver, uEarth)))

  mynet.train_query(Seq(wWorld, wBank), Seq(uWorldBank, uRiver, uEarth), uWorldBank)

  println(s"after train #1...")
  println(mynet.get_result(Seq(wWorld, wBank), Seq(uWorldBank, uRiver, uEarth)))

  println(s"train using some data 30 times...")
  val allurls = Seq(uWorldBank, uRiver, uEarth)
  30.range.map { i =>
    mynet.train_query(Seq(wWorld, wBank), allurls, uWorldBank)
    mynet.train_query(Seq(wRiver, wBank), allurls, uRiver)
    mynet.train_query(Seq(wWorld), allurls, uEarth)
  }

  println(s"""after train 30 times""")
  println(mynet.get_result(Seq(wWorld, wBank), allurls))
  //(0.861, 0.011, 0.016)

  println(mynet.get_result(Seq(wRiver, wBank), allurls))
  //(-0.030, 0.883, 0.006)

  println(mynet.get_result(Seq(wBank), allurls))
  //(0.865, 0.001, -0.85)

  mynet.shutdown()

}