open System.Net
open System
open System.IO
open System.Text.RegularExpressions

let fetchUrl(url:string) : string =
    let req= WebRequest.Create(Uri(url))
    use resp= req.GetResponse()
    use stream = resp.GetResponseStream()
    use reader = new StreamReader(stream)
    reader.ReadToEnd()

///<summary> Counts the hyperlinks found in the HTML of the given URL. </summary>
///<param name="url"> The url which HTML is analyzed. </param>
///<returns> count of hyperlinks of given URL, if URL is invalid the count of the google homepage is given. </returns>
let countLinks (url:string) : int =
    try 
        let tags = "(?s)<a [^>]*?>(?<text>.*?)</a>"
        let link = fetchUrl url
        let regex = Regex tags 
        let count = regex.Matches(link).Count
        count
    with
        |_ ->   printfn "error: not valid URL - used 'http://google.com/' instead:"
                let tags = "(?s)<a [^>]*?>(?<text>.*?)</a>"
                let link = fetchUrl "http://google.com/"
                let regex = Regex tags 
                let count = regex.Matches(link).Count
                count

printfn"write url"
let urls = Console.ReadLine()
printfn "%A" (countLinks urls)