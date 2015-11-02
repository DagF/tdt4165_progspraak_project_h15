import exceptions.{IllegalAmountException, NoSufficientFundsException}

class Account(val initialBalance: Double, val uid: Int = Bank getUniqueId) {

  private var balance: Double = initialBalance

  def withdraw(amount: Double): Unit =  {
    this.synchronized{
      if( amount > balance) throw new NoSufficientFundsException
      else if ( amount < 0) throw new IllegalAmountException
      else balance -= amount
    }
  }

  def deposit(amount: Double): Unit = {
    this.synchronized{
      if( amount < 0 ) throw new IllegalAmountException
      else balance += amount
    }
  }

  def getBalanceAmount: Double = balance

}