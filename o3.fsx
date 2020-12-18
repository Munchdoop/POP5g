type Account(nameInit:string, accountNumberInit:int, transactionsInit:(string*float) list) =
    //constructor
    static let mutable lastAccountNumber = 0
    do lastAccountNumber <- lastAccountNumber + 1
    let mutable transactionsList = transactionsInit
    let mutable accountNumber = accountNumberInit
    //properties
    member this.AccountNumber = accountNumber
    member this.TransactionsList = transactionsList
    //methords
    member this.DisplayTransactionsList() = printfn "%A" this.TransactionsList
    member this.LastAccount() = lastAccountNumber
    member this.Add(description:string, money:float) =
        let newAction = (description, money) in transactionsList <- newAction :: transactionsList
    member this.Balance() =
        let mutable balance = 0.0
        for elm in transactionsList do
            balance <- balance + snd elm
        printfn "Balance: %f" balance 
        balance

let acc1 = Account("John", 10, [])
printfn "Last account: %A" (acc1.LastAccount())
let acc2 = Account("Mark", 20, [("Inheritance", 69420.00)])
printfn "Last account: %A" (acc2.LastAccount())

acc1.Balance()
acc1.Add("Deposit", 10.00)
acc1.Balance()
acc1.Add("Withdraw", -20.00)
acc1.Balance()
acc1.DisplayTransactionsList()

acc2.Balance()
acc2.DisplayTransactionsList()