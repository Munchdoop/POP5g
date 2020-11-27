Oversæt og kør koden
- Åben din Kommando-linje eller terminal
- Bevæg dig ind i den sti hvori filerne ligger
- Skriv "fsharpc -a readNWrite.fs " i terminalen
- Skriv "fsharpc -r readNWrite.dll cat.fsx" i terminalen
- Skriv "fsharpc -r readNWrite.dll tac.fsx" i terminalen
- Skriv "fsharpc countLinks.fsx" i terminalen
For at køre cat: 
- Skriv "mono cat.exe test.txt" i terminalen hvor test-txt henviser til en txt i samme filplacering
For at køre tac: 
- Skriv "mono tac.exe test.txt" i terminalen hvor test-txt henviser til en txt i samme filplacering
For at køre countLinks:
- Skriv "mono countLinks.exe" i terminalen 
- Skriv en webadresse i terminalen (NB: kun http:// virker, ikke https://)

Example cat:
>mono cat.exe a.txt b.txt
- apple is a fruit
- ape is an animal
- avocado is expensive
- banana is my favorite food
- bento is a lunch box
- bend is what my body does 

>mono cat.exe hej
- System.Exception: Error: file does not exist

Example tac:
mono tac.exe a.txt b.txt 
- seod ydob ym tahw si dneb
- xob hcnul a si otneb
- doof etirovaf ym si ananab 
- evisnepxe si odacova
- lamina na si epa
- tiurf a si elppa

>mono tac.exe hej
- System.Exception: Error: file does not exist

Example 9g3:
>mono countLinks.exe 
>http://www.google.com/
- 28
>mono countLinks.exe 
>http://www.google.com/search?sxsrf=ALeKk03czmkWp5nxj7SY4BGytSSAe4MYfg%3A1606398018183&source=hp&ei=QrC_X4DzCKiYlwTk64LADw&q=cat&oq=cat&gs_lcp=CgZwc3ktYWIQAzIECCMQJzIKCCMQJxCoAxCeAzIECCMQJzIFCAAQsQMyCwgAELEDEIMBEIsDMg4ILhCLAxCoAxCbAxCaAzICCAAyCAgAELEDEIsDMgUILhCxAzIFCAAQiwM6BwgjEOoCECc6BggjECcQE1CLCFijE2C5FGgBcAB4AYAB6wOIAeIGkgEHMS4yLjQtMZgBAKABAaoBB2d3cy13aXqwAQq4AQM&sclient=psy-ab&ved=0ahUKEwiAnPX5qqDtAhUozIUKHeS1APgQ4dUDCAc&uact=5
- 73
>mono countLinks.exe 
>hej
- error: not valid URL - used 'http://google.com/' instead: 28  