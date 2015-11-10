import scala.concurrent.ExecutionContext
import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

  class AccountID(var uid: Int) {}

  private val uid = new AccountID(0)
   val transactionsQueue: TransactionQueue = new TransactionQueue()
   val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executorContext = ExecutionContext.fromExecutor(new ForkJoinPool(6))

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    transactionsQueue push new Transaction(
      transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
  }

  def generateAccountId: Int = {
    var id = 0

    uid synchronized{
      id = uid.uid
      uid.uid += 1
    }
    id
  }

  val transactionThread = Main.thread(
    processTransactions
  )


  private def processTransactions: Unit = {
    while( !transactionsQueue.isEmpty ){
        executorContext.execute(transactionsQueue pop)
    }
    processTransactions
  }

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {

    processedTransactions.iterator.toList
  }

  def getTransactionsInTransactionQueueAsList: List[Transaction] = {
    transactionsQueue.iterator.toList
  }
}
