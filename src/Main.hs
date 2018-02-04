--Copyright (c) 2018 Paolo Belluti. All rights reserved.
--
--This file is part of Cinemaffinity.
--
--Cinemaffinity is free software: you can redistribute it and/or modify
--it under the terms of the GNU General Public License as published by
--the Free Software Foundation, either version 3 of the License, or
--(at your option) any later version.
--
--Cinemaffinity is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--GNU General Public License for more details.
--
--You should have received a copy of the GNU General Public License
--along with Cinemaffinity.  If not, see <http://www.gnu.org/licenses/>.


module Main where

import Database.HDBC
import Database.HDBC.Sqlite3
--import qualified Data.HashMap as HashMap
import Data.List
import Control.Monad

import DbEsempio

-- percorso relativo: da dove inizia l'esecuzione di ghci
-- se doppio click su Main.hs: nella stessa cartella
-- se avviato da menu start: C:\Program Files\Haskell Platform\8.2.1
-- :! cd per verificare
fileDb = "C:\\Users\\Paolo\\Desktop\\test1.db" :: FilePath

--conn <- connectSqlite3 fileDb
--conn <- connettiPFK

connettiPFK = do conn <- connectSqlite3 fileDb
                 runRaw conn "COMMIT; PRAGMA foreign_keys = ON; BEGIN TRANSACTION"
                 return conn

creaDbEsempio' :: IO ()
creaDbEsempio' = creaDbEsempio fileDb -- creaDbEsempio è in DbEsempio

main :: IO ()
main =
  do putStrLn "questa è la funzione main :: IO Integer"

-- nuovi tipi

type Username  = String
type Email     = String
type Imdb      = Integer
type Titolo    = String
type Anno      = Integer
type Regista   = String
type Film      = (Imdb, Titolo, Anno, Regista)
type Gradito   = Bool
type Commento  = String
type Etichetta = String

-- esistenza record date chiavi primarie

esisteUtente :: Connection -> Username -> IO Bool
esisteUtente conn username =
  do tuple <- quickQuery' conn "SELECT * FROM Utenti WHERE username = ?" [toSql username]
     if length tuple == 0
       then return False
       else return True

esisteFilm :: Connection -> Imdb -> IO Bool
esisteFilm conn imdb =
  do tuple <- quickQuery' conn "SELECT * FROM Film WHERE imdb = ?" [SqlInteger imdb]
     if length tuple == 0
       then return False
       else return True

esisteDescrizione :: Connection -> Imdb -> Etichetta -> IO Bool
esisteDescrizione conn imdb etichetta =
  do tuple <- quickQuery' conn "SELECT * FROM Etichette WHERE imdb = ? AND etichetta = ?" [SqlInteger imdb, toSql etichetta]
     if length tuple == 0
       then return False
       else return True

esisteRecensione :: Connection -> Username -> Imdb -> IO Bool
esisteRecensione conn username imdb =
  do tuple <- quickQuery' conn "SELECT * FROM Recensioni WHERE username = ? AND imdb = ?" [toSql username, SqlInteger imdb]
     if length tuple == 0
       then return False
       else return True

-- getter

getListaImdb :: Connection -> IO [Integer]
getListaImdb conn =
  do tuple <- quickQuery' conn "SELECT imdb FROM Film" []
     return $ map fromSql (concat tuple)

getEtichette :: Connection -> IO [String]
getEtichette conn =
  do tuple <- quickQuery' conn "SELECT DISTINCT etichetta FROM Etichette" []
     return $ map fromSql (concat tuple)

getEtichetteAZ :: Connection -> IO [String]
getEtichetteAZ conn = sort <$> getEtichette conn
     
-- inserimento nuovi record

insUtente :: Connection -> Username -> Email -> IO Bool
insUtente conn username email =
  do esiste <- esisteUtente conn username
     if not esiste
       then do run conn "INSERT INTO Utenti (username, email) VALUES (?, ?)" [toSql username, toSql email]
               return True
       else return False

insFilm :: Connection -> Imdb -> Titolo -> Anno -> Regista -> IO Bool
insFilm conn imdb titolo anno regista =
  do esiste <- esisteFilm conn imdb
     if not esiste
       then do run conn "INSERT INTO Film (imdb, titolo, anno, regista) VALUES (?, ?, ?, ?)" [SqlInteger imdb, toSql titolo, SqlInteger anno, toSql regista]
               return True
       else return False

insDescrizione :: Connection -> Imdb -> Etichetta -> IO Bool
insDescrizione conn imdb etichetta =
  do esisteF <- esisteFilm conn imdb
     esisteE <- esisteDescrizione conn imdb etichetta
     if (esisteF && (not esisteE))
       then do run conn "INSERT INTO Etichette (imdb, etichetta) VALUES (?, ?)" [SqlInteger imdb, toSql etichetta]
               return True
       else return False

insRecensione :: Connection -> Username -> Imdb -> Gradito -> Commento -> IO Bool
insRecensione conn username imdb gradito commento =
  do esisteU <- esisteUtente conn username
     esisteF <- esisteFilm conn imdb
     esisteR <- esisteRecensione conn username imdb
     if (esisteU && esisteF && (not esisteR))
       then do run conn "INSERT INTO Recensioni (username, imdb, gradito, commento) VALUES (?, ?, ?, ?)" [toSql username, SqlInteger imdb, toSql gradito, toSql commento]
               return True
       else return False

-- eliminazione record

-- propagazione gestita da FOREIGN KEY ON DELETE
elimUtente :: Connection -> Username -> IO Bool
elimUtente conn username =
  do esiste <- esisteUtente conn username
     if esiste
        then do run conn "DELETE FROM Recensioni WHERE username = ?" [toSql username]
                run conn "DELETE FROM Utenti WHERE username = ?" [toSql username]
                return True
        else return False

-- propagazione gestita da FOREIGN KEY ON DELETE
elimFilm :: Connection -> Imdb -> IO Bool
elimFilm conn imdb =
  do esiste <- esisteFilm conn imdb
     if esiste
        then do run conn "DELETE FROM Recensioni WHERE imdb = ?" [SqlInteger imdb]
                run conn "DELETE FROM Etichette WHERE imdb = ?" [SqlInteger imdb]
                run conn "DELETE FROM Film WHERE imdb = ?" [SqlInteger imdb]
                return True
        else return False

elimFilm2 :: Connection -> Film -> IO Bool
elimFilm2 conn film =
  do let imdb = imdbFilm film
     tuple <- quickQuery' conn "SELECT * FROM Film WHERE imdb = ?" [SqlInteger imdb]
     let filmTrovato = filmDaTupla (head tuple)
     if ((length tuple == 1) && (filmTrovato == film))
        then do elimFilm conn imdb
                return True
        else return False

elimDescrizione :: Connection -> Integer -> Etichetta -> IO Bool
elimDescrizione conn imdb etichetta =
  do esiste <- esisteDescrizione conn imdb etichetta
     if esiste
        then do run conn "DELETE FROM Etichette WHERE imdb = ? AND etichetta = ?" [SqlInteger imdb, toSql etichetta]
                return True
        else return False

elimRecensione :: Connection -> Username -> Imdb -> IO Bool
elimRecensione conn username imdb =
  do esiste <- esisteRecensione conn username imdb
     if esiste
        then do run conn "DELETE FROM Recensioni WHERE username = ? AND imdb = ?" [toSql username, SqlInteger imdb]
                return True
        else return False

-- funzioni di confronto etc.

filmToString :: Film -> String
filmToString (i, t, a, r) = t ++ " (" ++ show a ++ ") di " ++ r ++ " [" ++ show i ++ "]"

imdbFilm :: Film -> Integer
imdbFilm (imdb, _, _, _) = imdb

filmDaImdb :: Connection -> Integer -> IO Film
filmDaImdb conn imdb =
  do tuple <- quickQuery' conn "SELECT * FROM Film WHERE imdb = ?" [SqlInteger imdb]
     let [i, t, a, r] = head tuple
     let imdb    = fromSql i :: Integer
     let titolo  = fromSql t :: String
     let anno    = fromSql a :: Integer
     let regista = fromSql r :: String
     return (imdb, titolo, anno, regista)
     
filmDaTupla :: [SqlValue] -> Film
filmDaTupla [i, t, a, r] =
  (imdb, titolo, anno, regista) where
    imdb    = fromSql i :: Integer
    titolo  = fromSql t :: String
    anno    = fromSql a :: Integer
    regista = fromSql r :: String

etichetteDaImdb :: Connection -> Imdb -> IO [Etichetta]
etichetteDaImdb conn imdb =
  do tuple <- quickQuery' conn "SELECT etichetta FROM Etichette WHERE imdb = ?" [SqlInteger imdb]
     let sqlEtichette = concat tuple
     return (map fromSql sqlEtichette :: [Etichetta])

-- filmGraditi conn utente :: IO [Film]
-- filmToString :: Film -> String
-- (map filmToString) :: [Film] -> [String]
-- fmap (map filmToString) :: Functor f => f [Film] -> f [String]
-- (map filmToString) <$> (filmGraditi conn username) :: IO String
filmGraditi :: Connection -> Username -> IO [Film]
filmGraditi conn username =
  do tuple <- quickQuery' conn "SELECT imdb, titolo, anno, regista FROM Utenti NATURAL JOIN Recensioni NATURAL JOIN Film WHERE username = ? AND gradito = \"True\"" [toSql username]
     return $ map filmDaTupla tuple

filmSgraditi :: Connection -> Username -> IO [Film]
filmSgraditi conn username =
  do tuple <- quickQuery' conn "SELECT imdb, titolo, anno, regista FROM Utenti NATURAL JOIN Recensioni NATURAL JOIN Film WHERE username = ? AND gradito = \"False\"" [toSql username]
     return $ map filmDaTupla tuple

-- non usato, banale. map imdbFilm <$> filmGraditi "noise" conn
imdbGraditi :: String -> Connection -> IO [Integer]
imdbGraditi username conn = map imdbFilm <$> filmGraditi conn username

etichetteFilm :: Connection -> Film -> IO [String]
etichetteFilm conn film = (etichetteDaImdb conn . imdbFilm) film

conta :: Eq a => a -> [a] -> Int
conta elem = length . filter (== elem)

affinitàE :: Connection -> Username -> Etichetta -> IO Int
affinitàE conn username etichetta =
  do imdbGraditi <- map imdbFilm <$> filmGraditi conn username
     etiGradite  <- concat <$> mapM (etichetteDaImdb conn) imdbGraditi
     imdbSgraditi <- map imdbFilm <$> filmSgraditi conn username
     etiSgradite  <- concat <$> mapM (etichetteDaImdb conn) imdbSgraditi
     let pos = conta etichetta etiGradite
     let neg = conta etichetta etiSgradite
     return $ pos - neg

-- affinità in termini di affinitàE (somma algebrica)
affinitàF :: Connection -> Username -> Imdb -> IO Int
affinitàF conn username imdb =
  do etichette <- etichetteDaImdb conn imdb
     listaAffinitàE <- mapM (affinitàE conn username) etichette
     return $ sum listaAffinitàE

mappaAffinitàFilm :: Connection -> Username -> IO [(Film, Int)]
mappaAffinitàFilm conn username =
  do tuple <- quickQuery' conn "SELECT * FROM Film" [] -- imdb, titolo, anno, regista -- astraibile, usato anche in mappaGradimentiMedi
     let listaFilm = map filmDaTupla tuple
     listaAffinità <- sequence $ map (affinitàF conn username) (map imdbFilm listaFilm) -- sequence $ map == mapM ?
     return $ reverse . sortBy ordineAffinità $ zip listaFilm listaAffinità

-- come mappaAffinitàFilm, ma contiene solo i film non recensiti
mappaAffinitàFilmS :: Connection -> Username -> IO [(Film, Int)]
mappaAffinitàFilmS conn username =
  do mappa <- mappaAffinitàFilm conn username
     filterM (\(film, _) -> not <$> recensito conn film username) mappa

-- come mappaAffinitàFilmS, ma filtra con l'SQL senza passare per mappaAffinitàFilm e recensito
mappaAffinitàFilmS2 :: Connection -> Username -> IO [(Film, Int)]
mappaAffinitàFilmS2 conn username =
  do tuple <- quickQuery' conn "SELECT * FROM Film WHERE imdb NOT IN (SELECT imdb FROM Recensioni NATURAL JOIN Utenti WHERE username = ?)" [toSql username]
     let listaFilm = map filmDaTupla tuple
     --               sequence trasforma [IO Int] -> IO [Int]
     listaAffinità <- sequence $ map (affinitàF conn username) (map imdbFilm listaFilm)
     return $ reverse . sortBy ordineAffinità $ zip listaFilm listaAffinità

recensito :: Connection -> Film -> Username -> IO Bool
recensito conn film username = esisteRecensione conn username (imdbFilm film)

ordineAffinità :: (Num a, Ord a) => (x, a) -> (x, a) -> Ordering
ordineAffinità (_, a1) (_, a2)
  | a1 < a2   = LT
  | a1 > a2   = GT
  | otherwise = EQ
     
mappaAffinitàEtichetteX :: Connection -> Username -> IO [(String, Int)]
mappaAffinitàEtichetteX conn username =
  do listaEtichette <- getEtichette conn
     listaAffinità  <- sequence $ map (affinitàE conn username) listaEtichette
     return $ reverse . sortBy ordineAffinità $ zip listaEtichette listaAffinità

mappaAffinitàEtichette :: Connection -> Username -> [Etichetta] -> IO [(Etichetta, Int)]
mappaAffinitàEtichette conn username etichette =
  do listaAffinità <- mapM (affinitàE conn username) etichette -- era sequence $ map (aff...
     return $ reverse . sortBy ordineAffinità $ zip etichette listaAffinità

gradimentoMedio :: Connection -> Imdb -> IO Double
gradimentoMedio conn imdb =
  do tuple <- quickQuery' conn "SELECT gradito FROM Recensioni WHERE imdb = ?" [SqlInteger imdb]
     let gradimentiBool = map fromSql $ concat tuple
     let gradimentiNum  = map (\b -> if b then 1 else -1) gradimentiBool
     return $ media gradimentiNum

mappaGradimentiMedi :: Connection -> IO [(Film, Double)]
mappaGradimentiMedi conn =
  do tuple <- quickQuery' conn "SELECT * FROM Film" []
     let listaFilm = map filmDaTupla tuple
     listaGrad <- sequence $ map (gradimentoMedio conn) (map imdbFilm listaFilm)
     return $ reverse . sortBy ordineAffinità $ zip listaFilm listaGrad

media :: Integral a => [a] -> Double
media [] = 0
media numeri = fromIntegral (sum numeri) / fromIntegral (length numeri)

elencoOrdEtichette :: Connection -> Username -> [Etichetta] -> IO [(Int, [Etichetta])]
elencoOrdEtichette conn username etichette =
  do mappa <- mappaAffinitàEtichette conn username etichette
     return $ elencoDaMappa mappa
  where elencoDaMappa [] = []
        elencoDaMappa [(e,a)] = [(a,[e])]
        elencoDaMappa ((e,a):mr) = (a,(map (\(e', a') -> e')
                                           (filter (\(_, a') -> a' == a)
                                                   ((e,a):mr)           ))) : (elencoDaMappa (filter (\(_, a') -> a' /= a)
                                                                                                     ((e,a):mr)           ))


--estremo inferiore = somma+1 del numero di tag che si trovano nelle righe superiori (nSup)
--estremo superiore = estremo inferiore + (numero tag in stessa riga -1) (nStessaRiga)
--per testarla: (:: necessario su elenco altrimenti ghci si aspetta Int e riceve Integer)
--map ($ elenco) (map intervalloPosizione etichette)
--p19: elenco = [(100,["a4","a6"]),(75,["a3"]),(66,["a5"]),(0,["a1","a2"])] :: [(Int,[String])]
--p19: etichette = ["a4","a6","a3","a5","a1","a2"]
--p19: intervalli == [(1,2),(1,2),(3,3),(4,4),(5,6),(5,6)]
--p40: elenco = [(66,["a8","a9"]),(50,["a5"]),(33,["a7"]),(0,["a1","a2","a3","a4","a6"])] :: [(Int,[String])]
--p40: etichette = ["a8","a9","a5","a7","a1","a2","a3","a4","a6"]
--p40: intervalli == [(1,2),(1,2),(3,3),(4,4),(5,9),(5,9),(5,9),(5,9),(5,9)]
intervalloPosizione :: String -> [(Int, [String])] -> (Int, Int)
intervalloPosizione etichetta elenco = (inf, sup)
  where inf = 1 + nSup etichetta elenco
        sup = inf + nStessaRiga etichetta elenco - 1
        nSup eti [] = 0
        nSup eti ((_, etichette):es) = if elem eti etichette then 0 else length etichette + nSup eti es
        nStessaRiga eti elenco = length $ concat (map (\(_,etichette) -> etichette)
                                                      (filter (\(_,etichette) -> elem eti etichette)
                                                              elenco                                ))

-- let in if
distanzaEtichetta :: [(Int, [Etichetta])] -> [(Int, [Etichetta])] -> Etichetta -> Integer
distanzaEtichetta elenco1 elenco2 etichetta =
     let intervallo1 = intervalloPosizione etichetta elenco1
         intervallo2 = intervalloPosizione etichetta elenco2
         [(inf1,sup1),(inf2,sup2)] = sort [intervallo1,intervallo2]
     in if sup1 >= inf2
           then 0
           else fromIntegral (inf2 - sup1)

---- guardie where
--distanzaEtichetta' :: [(Int, [String])] -> [(Int, [String])] -> String -> Integer
--distanzaEtichetta' elenco1 elenco2 etichetta
--  | sup1 >= inf2 = 0
--  | otherwise    = fromIntegral (inf2 - sup1)
--  where [(inf1,sup1),(inf2,sup2)] = sort [intervallo1,intervallo2]
--        intervallo1 = intervalloPosizione etichetta elenco1
--        intervallo2 = intervalloPosizione etichetta elenco2

---- distanzaEtichetta conn etichetta utente1 utente2
--distanzaEtichetta2 :: Connection -> Username -> Username -> Etichetta -> IO Integer
--distanzaEtichetta2 conn utente1 utente2 etichetta =
--  do elenco1 <- elencoOrdEtichette conn utente1
--     elenco2 <- elencoOrdEtichette conn utente2
--     return $ distanzaEtichetta elenco1 elenco2 etichetta

--distanzaTotaleX :: Connection -> Username -> Username -> IO Integer
--distanzaTotaleX conn utente1 utente2 =
--  do etichette <- getEtichette conn
--     sum <$> mapM (distanzaEtichetta2 conn utente1 utente2) etichette

-- solo etichette condivise
distanzaTotale :: Connection -> Username -> Username -> IO Integer
distanzaTotale conn utente1 utente2 =
  do etichette <- etichetteCondivise conn utente1 utente2
     elenco1   <- elencoOrdEtichette conn utente1 etichette
     elenco2   <- elencoOrdEtichette conn utente2 etichette
     return $ sum $ map (distanzaEtichetta elenco1 elenco2) etichette

etichetteCondivise :: Connection -> Username -> Username -> IO [Etichetta]
etichetteCondivise conn utente1 utente2 = --non si può leggere più volte con lo stesso statement: fetchAllRows non supporta parametri --bastava definire una stringa...
  do let query = "SELECT DISTINCT etichetta FROM Etichette NATURAL JOIN Recensioni WHERE username = ?"
     tuple1 <- quickQuery' conn query [toSql utente1]
     tuple2 <- quickQuery' conn query [toSql utente2]
     let etichette1 = map fromSql (concat tuple1)
     let etichette2 = map fromSql (concat tuple2)
     return $ intersezione etichette1 etichette2

intersezione :: Eq a => [a] -> [a] -> [a]
intersezione [] _     = []
intersezione (x:xs) y = if elem x y then (x:(intersezione xs y)) else intersezione xs y

distanzaMedia :: Connection -> Username -> Username -> IO Double
distanzaMedia conn utente1 utente2 =
  do distTot <- distanzaTotale conn utente1 utente2
     numEti  <- length <$> etichetteCondivise conn utente1 utente2
     if numEti == 0 then return 0
                    else return $ (fromIntegral distTot) / (fromIntegral numEti)

-- funzioni di presentazione

-- filmGraditi username conn >>= elencaFilm
elencaFilm :: [Film] -> IO ()
elencaFilm listaFilm = mapM_ putStrLn . map filmToString $ listaFilm

mostraAffinitàFilm :: [(Film, Int)] -> IO ()
mostraAffinitàFilm mappa =
  do let stringhe = map (\(f, a) -> show a ++ "\t\t" ++ filmToString f) mappa
     putStrLn $ "Affinità" ++ "\t" ++ "Film"
     putStrLn $ "--------" ++ "\t" ++ "----"
     mapM_ putStrLn stringhe

-- solo affinità positive
mostraFilmConsigliati :: Connection -> String -> Int -> IO ()
mostraFilmConsigliati conn username n =
  do mappa <- mappaAffinitàFilmS2 conn username
     let mappaPositiva = filter (\(_, a) -> a > 0) mappa
     mostraAffinitàFilm $ take n mappaPositiva

mostraAffinitàEtichette :: Connection -> String -> IO ()
mostraAffinitàEtichette conn username =
  do tuple <- quickQuery' conn "SELECT DISTINCT etichetta FROM Etichette NATURAL JOIN Recensioni WHERE username = ?" [toSql username]
     let etiRecensite = map fromSql (concat tuple)
     mappa <- mappaAffinitàEtichette conn username etiRecensite
     let stringhe = map (\(etichetta, a) -> show a ++ "\t\t" ++ etichetta) mappa
     putStrLn $ "Affinità" ++ "\t" ++ "Etichetta"
     putStrLn $ "--------" ++ "\t" ++ "---------"
     mapM_ putStrLn stringhe

mostraGradimentiMedi :: Connection -> IO ()
mostraGradimentiMedi conn =
  do mappa <- mappaGradimentiMedi conn
     let stringhe = map (\(f, g) -> take 4 (show g) ++ "\t\t" ++ filmToString f) mappa
     putStrLn $ "Gradim. medio" ++ "\t" ++ "Film"
     putStrLn $ "-------------" ++ "\t" ++ "----"
     mapM_ putStrLn stringhe

mostraGradimentiPercento :: Connection -> IO ()
mostraGradimentiPercento conn =
  do mappa <- mappaGradimentiMedi conn
     let stringhe = map (\(f, g) -> take 5 (show (g/2*100+50)) ++ "\t\t" ++ filmToString f) mappa
     putStrLn $ "% gradimento" ++ "\t" ++ "Film"
     putStrLn $ "------------" ++ "\t" ++ "----"
     mapM_ putStrLn stringhe

confrontaAffinitàEtichette :: Connection -> Username -> Username -> IO ()
confrontaAffinitàEtichette conn utente1 utente2 =
  do etichette <- sort <$> etichetteCondivise conn utente1 utente2
     elenco1   <- elencoOrdEtichette conn utente1 etichette
     elenco2   <- elencoOrdEtichette conn utente2 etichette
     distTot   <- distanzaTotale conn utente1 utente2
     distMedia <- distanzaMedia conn utente1 utente2
     putStrLn $ "Etichetta" ++ "\t" ++ "Pos. " ++ utente1 ++ "\t" ++ "Pos. " ++ utente2 ++ "\t" ++ "Distanza"
     putStrLn $ "---------" ++ "\t" ++ replicate (5 + length utente1) '-' ++ "\t" ++ replicate (5 + length utente2) '-' ++ "\t" ++ replicate (length "Distanza") '-'
     mapM_ putStrLn (map (riga elenco1 elenco2) etichette)
     putStrLn $ "\n" ++ "Etichette condivise: " ++ show (length etichette)
     putStrLn $ "\n" ++ "Distanza totale: " ++ show distTot
     putStrLn $ "\n" ++ "Distanza media: " ++ take 4 (show distMedia)
  where riga el1 el2 eti = padR 16 eti ++ padR 16 (interv el1 eti) ++ padR 16 (interv el2 eti) ++ padR 16 (show $ distanzaEtichetta el1 el2 eti)
        interv el eti = show $ intervalloPosizione eti el

padR :: Int -> String -> String
padR n str
  | length str < n = str ++ replicate (n - length str) ' '
  | otherwise      = str