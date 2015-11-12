import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

  val accountCounter = new AtomicInteger(1000)

  def createAccount(initialBalance: Double): ActorRef = {
    // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
    var id = ""
    accountCounter.synchronized{
      id = "" + accountCounter.incrementAndGet()

    }
    val account = BankManager.createAccount(id,this.bankId,initialBalance)

    account
  }

  def findAccount(accountId: String): Option[ActorRef] = {
    val ref = BankManager.findAccount(this.bankId, accountId)
    Option(ref)
  }

  def findOtherBank(bankId: String): Option[ActorRef] = {
    // Use BankManager to look up a different bank with ID bankId
    val ref = BankManager.findBank(bankId)
    Option(ref)
  }

  override def receive = {
    case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance)
    case GetAccountRequest(id) => ??? // Return account
    case IdentifyActor => sender ! this
    case t: Transaction => processTransaction(t)

    case t: TransactionRequestReceipt => {
      println("I got IT!!!")
      val isInternal = t.toAccountNumber.length <= 4
      val toBankId = if (isInternal) bankId else t.toAccountNumber.substring(0, 4)
      val toAccountId = if (isInternal) t.toAccountNumber else t.toAccountNumber.substring(4)

      if( isInternal ){
        val account = findAccount( toAccountId)
        account match {
          case Some(ref) => ref.forward(t)
          case None => println("Dit not find it")
        }
      }
      else{
        val bank = findOtherBank( toBankId )
        bank match {
          case Some(ref) => ref.forward(t)
          case None => println("Dit not find it")
        }
      }
    }

    case msg => {}
  }

  def processTransaction(t: Transaction): Unit = {
    implicit val timeout = new Timeout(5 seconds)
    val is8Bits= t.to.length <= 4
    val toBankId = if (is8Bits) bankId else t.to.substring(0, 4)
    val toAccountId = if (is8Bits) t.to else t.to.substring(4)
    val isInternal = bankId.equals(toBankId)
    //val transactionStatus = t.status
    //val toBankId = t.to.substring(0, 4)
    //val toAccountId = t.to.substring(4)
    //val isInternal = bankId.equals(toBankId)
    //val transactionStatus = t.status
    // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
    // HINT: Make use of the variables that have been defined above.

    if( isInternal ){
      val account = findAccount( toAccountId)
      account match {
        case Some(ref) => {
          ref.forward(t)
          println(ref)
        }
        case None => println("Dit not find it")
      }
    }
    else{
      val bank = findOtherBank( toBankId )

      bank match {
        case Some(ref) => {
          println("another bank")
          println(ref)
          ref.forward(t)
        }
        case None => println("Dit not find it")
      }
    }

  }
}