Cinemαffinity
=============

Applicazione dimostrativa sviluppata come parte del progetto di tesi **"Studio e sperimentazione di metodi per l'interfacciamento tra il linguaggio funzionale Haskell e i sistemi di gestione di basi di dati"** presso l'Università degli studi di Brescia - Dipartimento di ingegneria dell'informazione (AA 2016-2017).

Il RDBMS utilizzato è SQLite 3, ma il programma può essere adattato ad altri sistemi con modifiche minime.

Requisiti: Haskell Platform con pacchetti base, HDBC, HDBC-sqlite3.

Inizializzazione
----------------
Da REPL aperta su Main.hs, impostare il percorso del file di database nella variabile `fileDb`:

`fileDb = "C:\\Users\\Nomeutente\\Desktop\\cinema.db"`

Acquisire la connessione con pragma `foreign_keys`, ad esempio in un dato `conn`:

`conn <- connettiPFK`

Creare il database e popolarlo con le tuple preimpostate in `DbEsempio.hs`:

`creaDbEsempio fileDb`

Inserimento dati
----------------

Inserimento di un nuovo utente:

`insUtente conn "nomeutente" "e@mail.com"`

Inserimento di un nuovo film con numero IMDb:

`insFilm conn 1234567 "Titolo" 2018 "Regista"`

Assegnamento di un'etichetta ad un film:

`insDescrizione conn 1234567 "Etichetta"`

Inserimento di una recensione positiva:

`insRecensione conn "nomeutente" 1234567 True "Bel film."`

Gradimento medio
----------------

Per un singolo film:

`gradimentoMedio conn 1234567`

Per tutti i film del catalogo, in formato percentuale:

`mostraGradimentiPercento conn`

Affinità utente-etichetta
-------------------------

Per una singola etichetta:

`affinitàE conn "nomeutente" "Etichetta"`

Per tutte le etichette dei film in catalogo:

`mostraAffinitàEtichette conn "nomeutente"`

Affinità utente-film
--------------------

`affinitàF conn "nomeutente" 1234567`

Raccomandazioni personalizzate
------------------------------

Raccomandazione di 5 film:

`mostraFilmConsigliati conn "nomeutente" 5`

Confronto tra preferenze utenti
-------------------------------

`confrontaAffinitàEtichette conn "utente1" "utente2"`
