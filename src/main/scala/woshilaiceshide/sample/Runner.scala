package woshilaiceshide.sample

object Runner extends App {

  val mynet = new SearchNet("nn.db")

  mynet.make_tables()

  val (wWorld, wRiver, wBank) = (101, 102, 103)
  val (uWorldBank, uRiver, uEarth) = (201, 202, 203)
  mynet.generate_hiddennode(Seq(wWorld, wBank), Seq(uWorldBank, uRiver, uEarth))

  println(s"hiddennode: ")
  mynet.show_hiddennode()

  println(s"wordhidden: ")
  mynet.show_wordhidden()

  println(s"hiddenurl: ")
  mynet.show_hiddenurl()

  mynet.shutdown()

}