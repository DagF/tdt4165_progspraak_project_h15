


object Main extends App {

  def thread(body: =>Unit): Thread = {
      val t = new Thread {
        override def run() = body
      }
      t.start
      t
    }
  
  val account1 = new Account(300)
  val account2 = new Account(500)
  val account3 = new Account(800)





  Main thread( Bank transaction(account1,account2,100) )
  Main thread(  Bank transaction(account2,account3,200) )
  Main thread( Bank transaction(account3,account1,400) )

  println( account1 getBalanceAmount )
  println( account2 getBalanceAmount )
  println( account3 getBalanceAmount )

}