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

printfn "input google homepage: %A" (countLinks "http://www.google.com/")
printfn "input google result for 'cat': %A" (countLinks "http://www.google.com/search?sxsrf=ALeKk03czmkWp5nxj7SY4BGytSSAe4MYfg%3A1606398018183&source=hp&ei=QrC_X4DzCKiYlwTk64LADw&q=cat&oq=cat&gs_lcp=CgZwc3ktYWIQAzIECCMQJzIKCCMQJxCoAxCeAzIECCMQJzIFCAAQsQMyCwgAELEDEIMBEIsDMg4ILhCLAxCoAxCbAxCaAzICCAAyCAgAELEDEIsDMgUILhCxAzIFCAAQiwM6BwgjEOoCECc6BggjECcQE1CLCFijE2C5FGgBcAB4AYAB6wOIAeIGkgEHMS4yLjQtMZgBAKABAaoBB2d3cy13aXqwAQq4AQM&sclient=psy-ab&ved=0ahUKEwiAnPX5qqDtAhUozIUKHeS1APgQ4dUDCAc&uact=5")
printfn "fail input 'hej' : %A" (countLinks "hej")