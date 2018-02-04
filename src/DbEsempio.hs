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

module DbEsempio
( creaDbEsempio
) 
where

import Database.HDBC
import Database.HDBC.Sqlite3

creaDbEsempio :: String -> IO ()
creaDbEsempio fileDb = do
  conn <- connectSqlite3 fileDb
-- Utenti (username -> Recensioni.username)
  run conn "DROP TABLE IF EXISTS Utenti" []
  --run conn "CREATE TABLE Utenti (username VARCHAR(20) PRIMARY KEY, email VARCHAR(254) NOT NULL)" []
  run conn "CREATE TABLE Utenti (username VARCHAR(20), email VARCHAR(254) NOT NULL, PRIMARY KEY (username))" []
  aggiungiUtente <- prepare conn "INSERT INTO Utenti VALUES (?, ?)"
  executeMany aggiungiUtente utentiEsempio
-- Film (imdb -> Etichette.imdb, Recensioni.imdb)
  run conn "DROP TABLE IF EXISTS Film" []
  --run conn "CREATE TABLE Film (imdb NUMBER(8,0) PRIMARY KEY, titolo VARCHAR(100), anno NUMBER(4,0), regista VARCHAR(100))" []
  run conn "CREATE TABLE Film (imdb NUMBER(8,0), titolo VARCHAR(100), anno NUMBER(4,0), regista VARCHAR(100), PRIMARY KEY (imdb))" []
  aggiungiFilm <- prepare conn "INSERT INTO Film VALUES (?, ?, ?, ?)"
  executeMany aggiungiFilm filmEsempio
-- Etichette (chiavi esterne: Film.imdb)
  run conn "DROP TABLE IF EXISTS Etichette" []
  --run conn "CREATE TABLE Etichette (imdb NUMBER(8,0) REFERENCES Film (imdb), etichetta VARCHAR(100), PRIMARY KEY (imdb, etichetta))" []
  run conn "CREATE TABLE Etichette (imdb NUMBER(8,0), etichetta VARCHAR(100), PRIMARY KEY (imdb, etichetta), FOREIGN KEY (imdb) REFERENCES Film (imdb) ON DELETE CASCADE)" []
  aggiungiEtichetta <- prepare conn "INSERT INTO Etichette VALUES (?, ?)"
  executeMany aggiungiEtichetta etichetteEsempio
-- Recensioni (chiavi esterne: Utenti.username, Film.imdb)
  run conn "DROP TABLE IF EXISTS Recensioni" []
  run conn "CREATE TABLE Recensioni (username VARCHAR(20), imdb NUMBER(8,0), gradito BOOLEAN, commento TEXT, PRIMARY KEY (username, imdb), FOREIGN KEY (username) REFERENCES Utenti (username) ON DELETE CASCADE, FOREIGN KEY (imdb) REFERENCES Film (imdb) ON DELETE CASCADE)" []
  aggiungiRecensione <- prepare conn "INSERT INTO Recensioni VALUES (?, ?, ?, ?)"
  executeMany aggiungiRecensione recensioniEsempio
  -- disconnetti
  commit conn
  disconnect conn

utentiEsempio = [[toSql "noise", toSql "go_noise@msn.com"]
                ,[toSql "dragend", toSql "bremen@hotmail.it"]
                ,[toSql "kyubi", toSql "tyrrhenoi@hotmail.it"]
                ,[toSql "orca", toSql "simatz91@polimi.it"]
                ,[toSql "chopper", toSql "bari@spari.net"]
                ]

filmEsempio = [[SqlInteger 0307479, toSql "Solaris", SqlInteger 2002, toSql "Steven Soderbergh"] -- https://movielens.org/movies/5881
              ,[SqlInteger 2278871, toSql "Blue Is The Warmest Color", SqlInteger 2013, toSql "Abdellatif Kechiche"] -- https://movielens.org/movies/105355
              ,[SqlInteger 0117951, toSql "Trainspotting", SqlInteger 1996, toSql "Danny Boyle"] -- https://movielens.org/movies/778
              ,[SqlInteger 0070608, toSql "Robin Hood", SqlInteger 1973, toSql "Wolfgang Reitherman"] -- https://movielens.org/movies/3034
              ,[SqlInteger 0107290, toSql "Jurassic Park", SqlInteger 1993, toSql "Steven Spielberg"] -- https://movielens.org/movies/480
              ,[SqlInteger 0110912, toSql "Pulp Fiction", SqlInteger 1994, toSql "Quentin Tarantino"] -- https://movielens.org/movies/296
              ,[SqlInteger 0060827, toSql "Persona", SqlInteger 1966, toSql "Ingmar Bergman"] -- https://movielens.org/movies/7327
              ,[SqlInteger 0301727, toSql "Il Popolo Migratore", SqlInteger 2001, toSql "Jacques Perrin"] -- https://movielens.org/movies/6299
              ,[SqlInteger 0109830, toSql "Forrest Gump", SqlInteger 1994, toSql "Robert Zemeckis"] -- https://movielens.org/movies/356
              ,[SqlInteger 0180093, toSql "Requiem For A Dream", SqlInteger 2000, toSql "Darren Aronofsky"] -- https://movielens.org/movies/3949
              ,[SqlInteger 0947798, toSql "Il Cigno Nero", SqlInteger 2010, toSql "Darren Aronofsky"] -- https://movielens.org/movies/81591
              ,[SqlInteger 0133093, toSql "Matrix", SqlInteger 1999, toSql "The Wachowski Brothers"] -- https://movielens.org/movies/2571
              ,[SqlInteger 0167261, toSql "Il Signore Degli Anelli: Le Due Torri", SqlInteger 2002, toSql "Peter Jackson"] -- https://movielens.org/movies/5952
              ,[SqlInteger 0108052, toSql "Schindler's List", SqlInteger 1993, toSql "Steven Spielberg"] -- https://movielens.org/movies/52885
              ,[SqlInteger 0156887, toSql "Perfect Blue", SqlInteger 1997, toSql "Satoshi Kon"] -- https://movielens.org/movies/52885
              ,[SqlInteger 0851578, toSql "Paprika", SqlInteger 2006, toSql "Satoshi Kon"] -- https://movielens.org/movies/52885
              ,[SqlInteger 0062622, toSql "2001: Odissea Nello Spazio", SqlInteger 1968, toSql "Stanley Kubrick"] -- https://movielens.org/movies/924
              ,[SqlInteger 0454841, toSql "Le Colline Hanno Gli Occhi", SqlInteger 2006, toSql "Alexandre Aja"] -- https://movielens.org/movies/52885
              ,[SqlInteger 0428803, toSql "La Marcia Dei Pinguini", SqlInteger 2005, toSql "Luc Jacquet"] -- https://movielens.org/movies/34072
              ,[SqlInteger 0241527, toSql "Harry Potter E La Pietra Filosofale", SqlInteger 2001, toSql "Chris Columbus"] -- https://movielens.org/movies/4896
              ,[SqlInteger 0304141, toSql "Harry Potter E Il Prigioniero Di Azkaban", SqlInteger 2004, toSql "Alfonso Cuaròn"] -- https://movielens.org/movies/8368
              ,[SqlInteger 0137523, toSql "Fight Club", SqlInteger 1999, toSql "David Fincher"] -- https://movielens.org/movies/2959
              ,[SqlInteger 0460791, toSql "The Fall", SqlInteger 2006, toSql "Tarsem Singh"] -- https://movielens.org/movies/59387
              ]

etichetteEsempio = [[SqlInteger 0307479, toSql "Fantascienza"] -- Solaris
                   ,[SqlInteger 0307479, toSql "Drammatico"]
                   ,[SqlInteger 0307479, toSql "Psicologico"]
                   ,[SqlInteger 0307479, toSql "Sentimentale"]
                   ,[SqlInteger 2278871, toSql "Drammatico"] -- Blue Is the Warmest Color
                   ,[SqlInteger 2278871, toSql "Sentimentale"]
                   ,[SqlInteger 0117951, toSql "Drammatico"] -- Trainspotting
                   ,[SqlInteger 0117951, toSql "Thriller"]
                   ,[SqlInteger 0117951, toSql "Droga"]
                   ,[SqlInteger 0117951, toSql "Crimine"]
                   ,[SqlInteger 0070608, toSql "Animazione"] -- Robin Hood
                   ,[SqlInteger 0070608, toSql "Comico"]
                   ,[SqlInteger 0070608, toSql "Fiabesco"]
                   ,[SqlInteger 0107290, toSql "Avventura"] -- Jurassic Park
                   ,[SqlInteger 0107290, toSql "Azione"]
                   ,[SqlInteger 0107290, toSql "Fantascienza"]
                   ,[SqlInteger 0107290, toSql "Orrore"]
                   ,[SqlInteger 0110912, toSql "Commedia"] -- Pulp Fiction
                   ,[SqlInteger 0110912, toSql "Thriller"]
                   ,[SqlInteger 0110912, toSql "Crimine"]
                   ,[SqlInteger 0110912, toSql "Droga"]
                   ,[SqlInteger 0060827, toSql "Drammatico"] -- Persona
                   ,[SqlInteger 0060827, toSql "Psicologico"]
                   ,[SqlInteger 0301727, toSql "Documentario"] -- Il Popolo Migratore
                   ,[SqlInteger 0301727, toSql "Natura"]
                   ,[SqlInteger 0109830, toSql "Drammatico"] -- Forrest Gump
                   ,[SqlInteger 0109830, toSql "Guerra"]
                   ,[SqlInteger 0109830, toSql "Sentimentale"]
                   ,[SqlInteger 0109830, toSql "Storico"]
                   ,[SqlInteger 0180093, toSql "Drammatico"] -- Requiem For A Dream
                   ,[SqlInteger 0180093, toSql "Droga"]
                   ,[SqlInteger 0180093, toSql "Sentimentale"]
                   ,[SqlInteger 0947798, toSql "Drammatico"] -- il Cigno Nero
                   ,[SqlInteger 0947798, toSql "Orrore"]
                   ,[SqlInteger 0947798, toSql "Psicologico"]
                   ,[SqlInteger 0947798, toSql "Thriller"]
                   ,[SqlInteger 0133093, toSql "Azione"] -- Matrix
                   ,[SqlInteger 0133093, toSql "Fantascienza"]
                   ,[SqlInteger 0167261, toSql "Azione"] -- Il Signore Degli Anelli: Le Due Torri
                   ,[SqlInteger 0167261, toSql "Avventura"]
                   ,[SqlInteger 0167261, toSql "Guerra"]
                   ,[SqlInteger 0167261, toSql "Fantastico"]
                   ,[SqlInteger 0108052, toSql "Drammatico"] -- Schindler's List
                   ,[SqlInteger 0108052, toSql "Guerra"]
                   ,[SqlInteger 0108052, toSql "Storico"]
                   ,[SqlInteger 0156887, toSql "Animazione"] -- Perfect Blue
                   ,[SqlInteger 0156887, toSql "Drammatico"]
                   ,[SqlInteger 0156887, toSql "Psicologico"]
                   ,[SqlInteger 0156887, toSql "Thriller"]
                   ,[SqlInteger 0851578, toSql "Animazione"] -- Paprika
                   ,[SqlInteger 0851578, toSql "Thriller"]
                   ,[SqlInteger 0062622, toSql "Fantascienza"] -- 2001: Odissea Nello Spazio
                   ,[SqlInteger 0062622, toSql "Thriller"]
                   ,[SqlInteger 0454841, toSql "Orrore"] -- Le Colline Hanno Gli Occhi
                   ,[SqlInteger 0428803, toSql "Documentario"] -- La Marcia Dei Pinguini
                   ,[SqlInteger 0428803, toSql "Natura"]
                   ,[SqlInteger 0241527, toSql "Fantastico"] -- Harry Potter E La Pietra Filosofale
                   ,[SqlInteger 0241527, toSql "Avventura"]
                   ,[SqlInteger 0304141, toSql "Fantastico"] -- Harry Potter E Il Prigioniero Di Azkaban
                   ,[SqlInteger 0304141, toSql "Avventura"]
                   ,[SqlInteger 0137523, toSql "Drammatico"] -- Fight Club
                   ,[SqlInteger 0137523, toSql "Psicologico"]
                   ,[SqlInteger 0137523, toSql "Thriller"]
                   ,[SqlInteger 0460791, toSql "Fantastico"] -- The Fall
                   ,[SqlInteger 0460791, toSql "Fiabesco"]
                   ]

recensioniEsempio = [[toSql "noise", SqlInteger 0060827, toSql True, toSql ""] -- Persona
                    ,[toSql "noise", SqlInteger 0156887, toSql True, toSql ""] -- Perfect Blue
                    ,[toSql "noise", SqlInteger 0947798, toSql True, toSql ""] -- il Cigno Nero
                    ,[toSql "noise", SqlInteger 0062622, toSql True, toSql ""] -- 2001: Odissea Nello Spazio
                    ,[toSql "noise", SqlInteger 0460791, toSql True, toSql ""] -- The Fall
                    ,[toSql "noise", SqlInteger 0167261, toSql False, toSql ""] -- Il Signore Degli Anelli: Le Due Torri
                    ,[toSql "noise", SqlInteger 0241527, toSql False, toSql ""] -- Harry Potter E La Pietra Filosofale
                    ,[toSql "noise", SqlInteger 0304141, toSql False, toSql ""] -- Harry Potter E Il Prigioniero Di Azkaban
                    ,[toSql "noise", SqlInteger 0301727, toSql False, toSql ""] -- Il Popolo Migratore
                    ,[toSql "dragend", SqlInteger 0133093, toSql True, toSql ""] -- Matrix
                    ,[toSql "dragend", SqlInteger 0107290, toSql True, toSql ""] -- Jurassic Park
                    ,[toSql "dragend", SqlInteger 0167261, toSql True, toSql ""] -- Il Signore Degli Anelli: Le Due Torri
                    ,[toSql "dragend", SqlInteger 2278871, toSql False, toSql ""] -- Blue Is the Warmest Color
                    ,[toSql "dragend", SqlInteger 0180093, toSql False, toSql ""] -- Requiem For A Dream
                    ,[toSql "kyubi", SqlInteger 0117951, toSql True, toSql ""] -- Trainspotting
                    ,[toSql "kyubi", SqlInteger 0947798, toSql True, toSql ""] -- il Cigno Nero
                    ,[toSql "kyubi", SqlInteger 0062622, toSql True, toSql ""] -- 2001: Odissea Nello Spazio
                    ,[toSql "kyubi", SqlInteger 0109830, toSql True, toSql ""] -- Forrest Gump
                    ,[toSql "kyubi", SqlInteger 0070608, toSql True, toSql ""] -- Robin Hood
                    ,[toSql "kyubi", SqlInteger 2278871, toSql True, toSql ""] -- Blue Is the Warmest Color
                    ,[toSql "kyubi", SqlInteger 0307479, toSql False, toSql ""] -- Il Signore Degli Anelli: Le Due Torri
                    ,[toSql "kyubi", SqlInteger 0241527, toSql False, toSql ""] -- Harry Potter E La Pietra Filosofale
                    ,[toSql "kyubi", SqlInteger 0428803, toSql True, toSql ""] -- La Marcia Dei Pinguini
                    ,[toSql "kyubi", SqlInteger 0301727, toSql True, toSql ""] -- Il Popolo Migratore
                    ,[toSql "orca", SqlInteger 0060827, toSql True, toSql ""] -- Persona
                    ,[toSql "chopper", SqlInteger 0060827, toSql False, toSql ""] -- Persona
                    ]