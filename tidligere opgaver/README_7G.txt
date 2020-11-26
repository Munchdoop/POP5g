INSTRUKTIONER TIL KOMPILERING OG KØRING AF KODEN
- Pakke .zip-filen ud til en mappe og bevæg dig ind i den sti, hvori filerne ligger
- Åben din Kommando-linje eller terminal
- Skriv "fsharpc -a wargame-shuffle.fs wargame-game.fs" i terminalen **(ordren er vigtig!)**
--- Filen "wargame-game.dll" skal genereres i mappen
- Skriv "fsharpc -r wargame-game.dll wargame-game-stats.fs" i terminalen
--- Distruerbare "wargame-game-stats.exe" skal genereres i mappen
- Skriv "mono wargame-game-stats.exe" i terminalen for at køre koden