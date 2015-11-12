import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

  private var transactions = HashMap[String, Transaction]()

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)

  def getFullAddress: String = {
    bankId + accountId
  }

  def getTransactions: List[Transaction] = {
    // Should return a list of all Transaction-objects stored in transactions
    transactions.values.toList
  }

  def allTransactionsCompleted: Boolean = {
    transactions.synchronized {
      transactions.foreach {
        case(key, value) => if(value.status == TransactionStatus.PENDING){
          return false
        }
      }
      true
    }
    // Should return whether all Transaction-objects in transactions are completed
  }

  def withdraw(amount: Double): Unit = {
    balance.synchronized {
      if (balance.amount - amount < 0) throw new NoSufficientFundsException
      if (amount <= 0) throw new IllegalAmountException
      balance.amount -= amount
    }
  }

  def deposit(amount: Double): Unit = {
    balance.synchronized {
      if (amount <= 0) throw new IllegalAmountException()
      balance.amount += amount
    }
  }

  def sendTransactionToBank(t: Transaction): Unit = {
    // Should send a message containing t to the bank of this account
    BankManager.findBank(bankId).forward(t)
  }

  def transferTo(accountNumber: String, amount: Double): Transaction = {

    val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

    if (reserveTransaction(t)) {
      try {
        withdraw(amount)
        sendTransactionToBank(t)

      } catch {
        case _: NoSufficientFundsException | _: IllegalAmountException =>
          t.status = TransactionStatus.FAILED
      }
    }

    t

  }

  def reserveTransaction(t: Transaction): Boolean = {
    if (!transactions.contains(t.id)) {
      transactions += (t.id -> t)
      return true
    }
    false
  }

  override def receive = {
    case IdentifyActor => sender ! this

    case TransactionRequestReceipt(to, transactionId, transaction) => {
      transactions.synchronized{
        val t = transactions.get(transactionId)
        t match {
          case Some(tra) => tra.status = transaction.status
          case None => ???
        }
        transaction
      }
    }

    case BalanceRequest => ??? // Should return current balance

    case t: Transaction => {
      // Handle incoming transaction

      try{
        deposit(t.amount)
        t.status = TransactionStatus.SUCCESS
      }
      catch{
        case e: Exception => t.status = TransactionStatus.FAILED
      }

      val transactionReqestReceipt = new TransactionRequestReceipt(t.to, t.id, t)
      sender ! transactionReqestReceipt
    }

    case msg => ???
  }

  def getBalanceAmount: Double = balance.amount

}
