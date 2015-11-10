
import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  private val queue = collection.mutable.Queue[Transaction]()
  class Counter(var count :Int){}
  val counter = new Counter(0)

  def incrementCounter: Unit ={
    counter synchronized{
      counter.count += 1
    }
  }

  def decrementCounter: Unit = {
    counter synchronized {
      counter.count -= 1
    }
  }

  def getCount: Int = {
    var c = 0
    counter synchronized{
      c = counter.count
    }
    c
  }

  // Remove and return the first element from the queue
  def pop: Transaction = {
    queue synchronized{
      queue.dequeue
    }
  }

  // Return whether the queue is empty
  def isEmpty: Boolean = {
    var empty = false
    queue synchronized {
      empty = queue isEmpty
    }
    empty
  }

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = {
    queue synchronized {
      queue.enqueue (t)
    }
  }

  // Return the first element from the queue without removing it
  def peek: Transaction = queue last

  // Return an iterator to allow you to iterate over the queue
  def iterator = {
    queue synchronized {
      queue.iterator
    }
  }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Integer) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING
  class Attempts(var int: Int) {}
  var attempts = new Attempts(1)

  def incrementAttemts(): Unit ={
    attempts synchronized {
      attempts.int += 1
    }
  }

  def getAttemts(): Int ={
    var int = 0
    attempts synchronized {
      int = attempts.int
    }
    int
  }

  def run: Unit = {

    def doTransaction() = {
      from withdraw amount
      to deposit amount
    }

    try {
      if (from.uid < to.uid)
        from synchronized {
          to synchronized {
            doTransaction
          }
        }
      else
        to synchronized {
          from synchronized {
            doTransaction
          }
        }
      status = TransactionStatus.SUCCESS
    }
    catch {
      case e :Exception => {
        status = TransactionStatus.FAILED
      }
    }


    if( status == TransactionStatus.SUCCESS ){
      processedTransactions.push(this)
    }
    else if(status == TransactionStatus.FAILED ){
      if( getAttemts < allowedAttemps ){
        incrementAttemts
        transactionsQueue.push(this);
      }else{
        processedTransactions.push(this)
      }
    }
    else{
      println("You missed me!")
    }
  }



}
