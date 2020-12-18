type Employee(nameInit:string, numInit : int) =
    member val Name = nameInit with get, set
    member val Number = numInit with get, set

type ProductionWorker(nameInit, numInit, shiftNumInit:int, hourlyPayInit:int) =
    inherit Employee(nameInit, numInit)
    member this.ShiftNum = shiftNumInit
    member this.HourlyPay = hourlyPayInit

type ShiftSupervisor(nameInit, numInit, annualSalaryInit:int) =
    inherit Employee(nameInit, numInit)
    let mutable annualSalary = annualSalaryInit
    let mutable bonusAmount : int = 1000
    let mutable bonus:int = 0
    member this.AnnualSalary = annualSalary
    member this.Bonus = bonus
    member this.SetBonus(amount:int) = bonusAmount <- amount
    member this.AddBonus() = bonus <- bonus + bonusAmount
    member this.GetTotalIncome() = this.AnnualSalary + this.Bonus

printfn "Enter name: "
let myName = System.Console.ReadLine()
printfn "Enter number: "
let myNum = System.Console.ReadLine() |> int
printfn "Enter shift number (1 or 2): "
let myShift = System.Console.ReadLine() |> int
printfn "Enter hourly pay: "
let myPay = System.Console.ReadLine() |> int

let myWorker = ProductionWorker(myName, myNum, myShift, myPay)
printfn "%A %A %A %A" (myWorker.Name) (myWorker.Number) (myWorker.ShiftNum) (myWorker.HourlyPay)