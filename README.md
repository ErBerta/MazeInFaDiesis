<html><body class="c35"><h1 class="c9" id="h.6ehpltnwddmi"><span class="c19">Requisiti</span></h1><p class="c2 c5"><span class="c0"></span></p><p class="c2"><span class="c0">Il progetto consiste nella realizzazione di un semplice videogioco in stile ASCII ART. In particolare, viene richiesto di sviluppare un programma che permetta la creazione di labirinti (maze) dove poter giocare in modalit&agrave; interattiva o in modalit&agrave; automatica.</span></p><p class="c2 c5"><span class="c0"></span></p><p class="c2"><span class="c0">I requisiti minimi per questo progetto sono:</span></p><ul class="c20 lst-kix_oyn3t7z52q0x-0 start"><li class="c2 c4"><span class="c0">implementazione dell&rsquo;algoritmo di generazione di un labirinto casuale w x h; &egrave; inoltre richiesta la sua corretta visualizzazione su schermo, mediante l&rsquo;utilizzo del motore gra&#64257;co messo a disposizione, oppure attraverso altri metodi concordati con il docente;</span></li></ul><ul class="c20 lst-kix_kf5rgq3ocrwk-0 start"><li class="c2 c4"><span class="c0">sviluppo della modalit&agrave; di gioco interattiva: un giocatore pu&ograve; dare comandi interattivi mediante l&rsquo;utilizzo di un dispositivo di input data (es: tastiera) al &#64257;ne di poter risolvere il labirinto generato. L&rsquo;interazione deve essere facilmente visualizzabile per l&rsquo;utente &nbsp;finale tramite la console;</span></li></ul><ul class="c20 lst-kix_xi4ypvcqf0x5-0 start"><li class="c2 c4"><span class="c0">sviluppo della modalit&agrave; automatica di risoluzione: il programma deve poter risolvere automaticamente il labirinto generato e mostrare gra&#64257;camente la soluzione al giocatore. L&rsquo;utente utilizzatore del programma deve poter scegliere tra le diverse modalit&agrave; di gioco, possibilmente tramite un men&ugrave; oppure una qualche altra forma interattiva di interfaccia gra&#64257;ca.</span></li></ul><p class="c2 c5"><span class="c0"></span></p><p class="c2 c5"><span class="c0"></span></p><p class="c2 c5"><span class="c0"></span></p><hr style="page-break-before:always;display:none;"><p class="c2 c5"><span class="c0"></span></p><h1 class="c9" id="h.cjy9yx6wk3ip"><span class="c19">Linguaggi utilizzati</span></h1><p class="c2"><span class="c0">Come da richiesta, il codice del nostro programma &egrave; scritto in f#. Le variabili, i tipi e le firme delle funzioni sono in genere autoesplicative e in inglese. Abbiamo deciso per&ograve; di scrivere i commenti in italiano, perch&eacute; abbiamo considerato chi effettivamente sarebbe andato a lavorare sul progetto e, avendo entrambi come lingua madre l&rsquo;italiano, abbiamo deciso che scrivere i commenti in inglese avrebbe potuto portarci a perdite di tempo e incomprensioni in modo particolare nel caso dovessimo mettere mano al codice in futuro.</span></p><h1 class="c9" id="h.jj70jct60za"><span class="c19">Descrizione file di progetto</span></h1><h2 class="c14" id="h.paz5pmj7loce"><span class="c22">Motore grafico</span></h2><p class="c2"><span>Il motore grafico su cui il nostro programma &egrave; basato ci &egrave; stato fornito come base di partenza su cui strutturare tutto il nostro progetto.<br>Su di esso, abbiamo effettuato alcune modifiche perch&egrave; potesse risolvere alcune nostre necessit&agrave;. In particolare abbiamo aggiunto nel file Engine.fs una funzione </span><span class="c1">refresh</span><span class="c0">&nbsp;che quando richiamata esegue un aggiornamento della schermata. Questo metodo verr&agrave; utilizzato durante la risoluzione automatica, in quanto abbiamo la necessit&agrave; di eseguire il refresh dello schermo ad ogni spostamento del nostro risolutore e non solo alla pressione di un tasto.</span></p><p class="c2"><span class="c0">I sorgenti e la documentazione del motore si possono trovare ai seguenti link:</span></p><ul class="c20 lst-kix_esttz82kjrq2-0 start"><li class="c2 c4"><span class="c18"><a class="c3" href="https://www.google.com/url?q=https://moodle.unive.it/pluginfile.php/408461/mod_assign/introattachment/0/ASCII_Maze.zip?forcedownload%3D1&amp;sa=D&amp;ust=1580066038516000">Sorgenti</a></span></li><li class="c2 c4"><span class="c18"><a class="c3" href="https://www.google.com/url?q=https://moodle.unive.it/pluginfile.php/408461/mod_assign/introattachment/0/ASCII%2520Maze1.2.pdf?forcedownload%3D1&amp;sa=D&amp;ust=1580066038516000">Documentazione e consegna del progetto</a></span></li></ul><h2 class="c14" id="h.cuugys853dfy"><span class="c22">Main.fs</span></h2><p class="c2"><span class="c0">Questo file ci &egrave; stato assegnato quasi pronto, al suo interno conteneva gi&agrave; alcune delle funzioni principali del programma, come la creazione della finestra di log. A questo abbiamo aggiunto le chiamate alle nostre funzioni, prima il main di Menu.fs. Poi a seconda delle scelte effettuate nel menu, viene richiamata la funzione che avvia la corrispondente modalit&agrave; di gioco. Al main di Maze.fs passo modalit&agrave; di gioco e risoluzione scelta.</span></p><h2 class="c14" id="h.tf3j8hufq1x1"><span class="c22">Menu.fs</span></h2><p class="c2"><span class="c0">Questo file contiene la gestione del men&ugrave; del gioco. Questo men&ugrave; consente di scegliere prima di tutto il gioco a cui si vuole giocare e poi, la grandezza del labirinto.</span></p><p class="c2"><span class="c0">Per memorizzare questa scelta ci siamo affidati a due variabili mutable &ldquo;gameMod&rdquo; e &ldquo;gameRes&rdquo; che contengono rispettivamente la modalit&agrave; di gioco (definite nel file di configurazione) e le 5 risoluzioni che abbiamo scelto per il nostro labirinto (15*15, 25*25, 51*51, 99*99, 99*51).</span></p><p class="c2"><span class="c0">Le varie scelte potranno essere effettuate spostando il cursore rosso verso l&rsquo;alto o verso il basso premendo &lsquo;w&rsquo; e &lsquo;s&rsquo;. In qualsiasi momento premendo il tasto &lsquo;q&rsquo; il giocatore potr&agrave; tornare al men&ugrave; precedente o uscire dal gioco a seconda di dove si trova.</span></p><h2 class="c14" id="h.5hi39slrl2o4"><span class="c22">Maze.fs</span></h2><h3 class="c12" id="h.53zs8vryj44j"><span class="c16">Modalit&agrave; di gioco</span></h3><p class="c27"><span>Le diverse modalit&agrave; di gioco vengono avviate in base al valore passato come parametro al </span><span class="c1">main </span><span class="c0">e sono le seguenti:</span></p><h4 class="c31" id="h.dx289pviq0ui"><span class="c29">Interattiva ~ single player</span></h4><h5 class="c23" id="h.1ajk5ikt4u8s"><span class="c7">Descrizione</span></h5><p class="c2"><span class="c0">In questa modalit&agrave; di gioco pu&ograve; giocare solo un utente per volta. Lo scopo del gioco &egrave; quello di raggiungere l&rsquo;angolo in basso a destra partendo da quello opposto.</span></p><h5 class="c23" id="h.u4b7n03spfsk"><span class="c7">Istruzioni di gioco</span></h5><p class="c2"><span class="c0">L&rsquo;utente potr&agrave; muoversi utilizzando 4 tasti:</span></p><ul class="c20 lst-kix_h5euicqe2m0c-0 start"><li class="c2 c4"><span class="c0">&lsquo;w&rsquo; spostamento di una posizione verso l&rsquo;alto</span></li><li class="c2 c4"><span class="c0">&lsquo;a&rsquo; spostamento di una posizione verso sinistra</span></li><li class="c2 c4"><span class="c0">&lsquo;s&rsquo; spostamento di una posizione verso il basso</span></li><li class="c2 c4"><span class="c0">&lsquo;d&rsquo; spostamento di una posizione verso il destra</span></li></ul><p class="c2"><span class="c0">La pressione del tasto &lsquo;q&rsquo; effettuer&agrave; l&#39;uscita dal gioco e il ritorno al men&ugrave;</span></p><h4 class="c31" id="h.aojr4rwtikny"><span class="c29">Interattiva ~ multi player</span></h4><h5 class="c23" id="h.vp7hxwvdkn94"><span class="c7">Descrizione</span></h5><p class="c2"><span class="c0">Possono giocare due giocatori in contemporanea. I giocatori inizieranno in alto a lati opposti e dovranno raggiungere l&rsquo;angolo in basso opposto a loro.</span></p><h5 class="c23" id="h.192fhpt161p7"><span class="c7">Istruzioni di gioco</span></h5><p class="c2"><span class="c0">L&rsquo;utente 1 potr&agrave; muoversi utilizzando 4 tasti:</span></p><ul class="c20 lst-kix_h5euicqe2m0c-0"><li class="c2 c4"><span class="c0">&lsquo;w&rsquo; spostamento di una posizione verso l&rsquo;alto</span></li><li class="c2 c4"><span class="c0">&lsquo;a&rsquo; spostamento di una posizione verso sinistra</span></li><li class="c2 c4"><span class="c0">&lsquo;s&rsquo; spostamento di una posizione verso il basso</span></li><li class="c2 c4"><span class="c0">&lsquo;d&rsquo; spostamento verso destra di una posizione</span></li></ul><p class="c2 c26 c5"><span class="c0"></span></p><p class="c2"><span class="c0">L&rsquo;utente 2 potr&agrave; muoversi utilizzando 4 tasti:</span></p><ul class="c20 lst-kix_h5euicqe2m0c-0"><li class="c2 c4"><span class="c0">&lsquo;i&rsquo; spostamento di una posizione verso l&rsquo;alto</span></li><li class="c2 c4"><span class="c0">&lsquo;j&rsquo; spostamento di una posizione verso sinistra</span></li><li class="c2 c4"><span class="c0">&lsquo;k&rsquo; spostamento di una posizione verso il basso</span></li><li class="c2 c4"><span class="c0">&lsquo;l&rsquo; spostamento verso destra di una posizione</span></li></ul><p class="c2 c5 c26"><span class="c0"></span></p><p class="c2"><span class="c0">La pressione del tasto &lsquo;q&rsquo; effettuer&agrave; l&#39;uscita dal gioco e il ritorno al men&ugrave;</span></p><h4 class="c31" id="h.mo0rj3v1hph1"><span class="c29">Automatica</span></h4><h5 class="c23" id="h.5a24xk7zlf69"><span class="c7">Descrizione</span></h5><p class="c2"><span class="c0">In seguito alla pressione del tasto &lsquo;s&rsquo; il labirinto verr&agrave; risolto automaticamente dal computer mostrando il percorso fatto.</span></p><p class="c2"><span class="c0">Sfrutta la variabile &ldquo;stop&rdquo; per fermare la ricerca e ignorare i risultati della ricorsione.</span></p><h5 class="c23" id="h.10628n13vcom"><span class="c7">Istruzioni di gioco</span></h5><ul class="c20 lst-kix_axnml2vqwvq4-0 start"><li class="c2 c4"><span class="c0">&lsquo;s&rsquo; inizio ricerca automatica del percorso corretto</span></li><li class="c2 c4"><span class="c0">&rsquo;q&rsquo; uscita dal programma (non effettuabile durante l&rsquo;esecuzione del risolutore)</span></li></ul><h4 class="c31" id="h.93jc18g6kdqs"><span class="c29">Interattiva + ~ single player</span></h4><h5 class="c23" id="h.mk6j0ya0xf0n"><span class="c7">Descrizione</span></h5><p class="c2"><span class="c0">Questa modalit&agrave; di gioco rispetto alla modalit&agrave; interattiva single player aggiunge solamente la valorizzazione del killer point che far&agrave; terminare precocemente il programma.</span></p><p class="c2 c5"><span class="c0"></span></p><h3 class="c12" id="h.z6fxk4z37ro5"><span class="c16">Implementazione</span></h3><p class="c2"><span class="c0">Abbiamo cercato il pi&ugrave; possibile di evitare di riscrivere codice, motivo per cui ci sono molte funzioni in comune tra tutte le varie modalit&agrave;.</span></p><h4 class="c31" id="h.nrbpwgjgg2n7"><span class="c29">Generazione del labirinto</span></h4><p class="c2"><span class="c0">Per la generazione del labirinto, ci siamo affidati ad un algoritmo trovato online che abbiamo implementato e modificato per ottenere una matrice valorizzata con i valori &ldquo;wall&rdquo; e &ldquo;path&rdquo; che indicano rispettivamente il muro e il percorso.</span></p><p class="c2"><span class="c0">Useremo questa matrice in diverse occasioni:</span></p><ul class="c20 lst-kix_oustk2fg7itl-0 start"><li class="c2 c4"><span class="c0">Generazione di un array di pixel per la visualizzazione, tramite il motore grafico, del labirinto</span></li><li class="c2 c4"><span class="c0">Controllo durante il gioco della presenza dei muri per permettere o negare lo spostamento</span></li></ul><h5 class="c23" id="h.cz34qiw1jdhr"><span class="c7">Algoritmo</span></h5><p class="c2"><span class="c0">L&rsquo;algoritmo di generazione funziona al seguente modo:</span></p><ul class="c20 lst-kix_rlgb24b481n3-0 start"><li class="c2 c4"><span>Inizializzazione della matrice a &ldquo;w</span><span class="c0">all&rdquo;</span></li><li class="c2 c4"><span>Scelta randomica di un punto da cui partire, settandolo a &ldquo;p</span><span class="c0">ath&rdquo;</span></li><li class="c2 c4"><span>Estensione ricorsiva del &ldquo;p</span><span>ath&rdquo; </span><span class="c0">a partire dal punto ottenuto, collegandolo di volta in volta a uno a caso dei suoi vicini (celle con distanza di 2)</span></li></ul><p class="c2 c5"><span class="c0"></span></p><hr style="page-break-before:always;display:none;"><h3 class="c12 c36" id="h.up2lxo184nlf"><span class="c16"></span></h3><h3 class="c12" id="h.6lyas2qi71pk"><span class="c16">My_update</span></h3><p class="c2"><span>Questa funzione &egrave; quella che gestisce i movimenti dello sprite effettuando anche i vari controlli del caso. Viene passata al motore grafico nella modalit&agrave; interattiva, in quella automatica viene richiamata direttamente da una nostra funzione, mentre in quella </span><span>multigiocatore</span><span class="c0">&nbsp;viene utilizzata da una funzione supplementare, passata al motore grafico, &nbsp;in modo da gestire i due diversi giocatori. </span></p><h3 class="c12" id="h.l74471yo7bxy"><span class="c16">Ricerca del percorso</span></h3><p class="c2"><span>Per effettuare la ricerca del percorso, abbiamo pensato di utilizzare un algoritmo di ricerca in profondit&agrave;, provando</span><span>&nbsp;prima il percorso di destra e in basso per ottimizzarne la</span><span class="c0">&nbsp;ricerca.<br>Le istruzioni eseguite sono:</span></p><ol class="c20 lst-kix_aqawjdz0299v-0 start" start="1"><li class="c2 c4"><span>Controllo della possibilit&agrave; di spostamento tramite una funzione </span><span class="c1">trymove</span><span class="c0">&nbsp;che esegue un controllo sulla struttura dati in cui &egrave; memorizzato il labirinto</span></li><li class="c2 c4"><span>Controllo che la posizione di destinazione non sia gi&agrave; stata visitata accedendo alla matrice </span><span class="c1">Visited </span><span>di tipo </span><span class="c1 c28">Visit</span></li><li class="c2 c4"><span>Se entrambi i controlli vanno a buon fine eseguo lo spostamento attraverso la funzione </span><span class="c28 c1">my_update</span></li><li class="c2 c4"><span class="c0">Quando nessuna delle quattro vie &egrave; percorribile torno indietro colorando di rosso la strada.</span></li></ol><h3 class="c21" id="h.stsl3ucwsz5q"><span class="c16">Avvio del programma</span></h3><p class="c2"><span class="c0">Il main accetta come parametro la modalit&agrave; di gioco e la risoluzione, una volta ottenute inizializza le variabili necessarie al funzionamento del programma e istanzia l&rsquo;engine. In base al GameMod ottenuto lancia l&rsquo;esecuzione della porzione di programma desiderata.</span></p><h3 class="c12" id="h.8c9c3g1dubu3"><span>KillerPoint</span></h3><p class="c2"><span class="c0">&Egrave; un punto invisibile che viene inizializzato all&rsquo;avvio del programma e che solo nella modalit&agrave; interattiva+ prende un valore all&rsquo;interno del labirinto. Questo punto quando attraversato blocca il programma e ritorna &ldquo;Game Over&rdquo; al player.</span></p><h3 class="c12" id="h.wl2ott1r8602"><span class="c16">Visualizzazione</span></h3><p class="c2"><span class="c0">Per garantire una visualizzazione quadrata del labirinto, abbiamo deciso di raddoppiare tutte le misure in larghezza senza intaccare per&ograve; le modalit&agrave; di memorizzazione.</span></p><p class="c2"><span class="c0">La strada percorsa dagli utenti sar&agrave; visualizzabile durante il gioco attraverso la creazione di uno sprite colorato per ogni pixel percorso dall&rsquo;utente. Nella modalit&agrave; automatica, segnaliamo di due colori diversi la strada corretta e quella errata.</span></p><hr style="page-break-before:always;display:none;"><h1 class="c9 c11" id="h.6llz0jm3f5vu"><span class="c19"></span></h1><h1 class="c9" id="h.xxipxyezbi7w"><span class="c19">Conclusioni</span></h1><p class="c2"><span class="c0">Il software finale &egrave; funzionante, collaudato e testato. Risponde a tutti i requisiti, con l&rsquo;aggiunta di alcune funzionalit&agrave; extra come la modalit&agrave; multigiocatore e la modalit&agrave; giocatore interattiva+. </span></p><p class="c2"><span class="c0">La struttura del programma inoltre, &egrave; predisposta per una facile implementazione futura di nuovi giochi e modalit&agrave; di gioco.</span></p><div><p class="c5 c38"><span class="c0"></span></p></div></body></html>
