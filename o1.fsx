type Person(nameInit:string, addressInit:string, phoneInit:string)=
    let mutable name = nameInit
    let mutable address = addressInit
    let mutable phone = phoneInit
    member this.Name = name
    member this.Address = address
    member this.Phone = phone

type Customer(cusNumInit:int, isOnListInit:bool)=
    inherit Person("John", "Red House", "123-4567")
    member this.ID = cusNumInit
    member this.IsOnList = isOnListInit

let myCustomer = Customer(1, true)
printfn "Customer name: %A" myCustomer.Name
printfn "Customer address: %A" myCustomer.Address
printfn "Customer phone number: %A" myCustomer.Phone
printfn "Customer number: %A" myCustomer.ID
printfn "Customer is on mailing list: %b" myCustomer.IsOnList

type MyClass2(dataIn) as self =
    let data = dataIn
    do
        self.PrintMessage()
    member this.PrintMessage() =
        printf "Creating MyClass2 with Data %d" data

let myClass = MyClass2(5)