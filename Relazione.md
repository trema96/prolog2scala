
# Prolog2Scala

Il progetto consiste di due moduli principali: uno si occupa di tradurre i programmi prolog in equivalenti programmi scala, l'altro invece definisce elementi che saranno necessari per l'esecuzione del programma tradotto.
## Libreria di esecuzione 
La libreria di esecuzione ha lo scopo rendere più evidente il parallelismo tra il programma prolog e la versione tradotta. Inoltre questa introduce elementi che permettono di scrivere i programmi in modo più conciso, di conseguenza il suo utilizzo semplifica anche l'operazione di traduzione.  La libreria è contenuta nel package `prolog2scala.lib`
Di seguito viene rappresentato un esempio di possibile traduzione di predicato prolog che si vorrebbe ottenere utilizzando questa libreria di supporto.
```prolog
#merge: merge(+list1, +list2, -outList)
merge(Xs,[],Xs):-!.  
merge([],Ys,Ys).  
merge([X|Xs],[Y|Ys],[X|Zs]):-X<Y,!,merge(Xs,[Y|Ys],Zs).  
merge([X|Xs],[X|Ys],[X,X|Zs]):-!,merge(Xs,Ys,Zs).  
merge([X|Xs],[Y|Ys],[Y|Zs]):-merge([X|Xs],Ys,Zs).  
```
```scala
def merge(list1: List[Int], list2: List[Int]): Stream[List[Int]] = Predicate[(List[Int],List[Int]),List[Int]](  
  Rule.withCut{case (xs, Nil) => ! Stream(xs)},  
  Fact{case (Nil, ys) => ys},  
  Rule.withCut{case (x :: xs, y :: ys) if x < y => ! (for (zs <- merge(xs, y +: ys)) yield x +: zs)},  
  Rule.withCut{case (x :: xs, x_1 :: ys) if x == x_1 => ! (for (zs <- merge(xs, ys)) yield x +: x +: zs)},  
  Rule{case (x :: xs, y :: ys) => for (zs <- merge(x +: xs, ys)) yield y +: zs }  
)(list1,list2)
```
La libreria fornisce supporto anche all'utilizzo del cut come operatore (prefisso, infisso o postfisso). Nei test sono presenti altri esempi di utilizzo.

## Modulo di traduzione
La traduzione di un programma prolog è eseguita in 3 fasi:
1. Parsing del programma e costruzione dell'AST
2. Decisione dei tipi del programma
3. Costruzione dell'albero del programma scala e generazione del corrispondente codice
### Parsing
Per effettuare il parsing del sorgente prolog si è fatto uso della libreria [fastparse](https://github.com/lihaoyi/fastparse). 
Questa permette di costruire un parser come combinazione di stringhe e altri parser; a ciascun parser si può poi associare una funzione che prendendo in input i componenti restituisce un output che nel caso specifico di questo progetto è un nodo dell'ast.  Di seguito sono illustrati degli esempi di regole di parsing:
```scala
def termList[_: P]: P[Seq[Term]] = P(term.rep(min = 1, sep = ","))
def struct[_: P]: P[Struct] = P(name ~~ ("(" ~ termList ~ ")").?) map {  
  case (name, args) => Struct(name, args.getOrElse(Seq.empty))  
}
```

Infine il parser richiede che i programmi prolog siano preceduti da una o più direttive, della forma 
```
#scalaName: predicateName([+|-]argumentName, ...)
```
Queste specificano quali predicati del programma prolog devono essere tradotti, con quale combinazione di input/output e con quali nomi.
### Decisione dei tipi
Una parte fondamentale del processo di traduzione è quella di assegnare dei tipi agli argomenti dei predicati e delle strutture che compaiono nel programma prolog. 
L'algoritmo esamina l'intero programma, tenendo traccia degli argomenti con cui viene usato ciascun predicato; queste informazioni vengono successivamente usate per determinare il tipo più adatto a ciascun argomento:
- Se l'argomento viene utilizzato esclusivamente con variabili "libere" gli sarà assegnato il tipo di un type parameter
- Se l'argomento è utilizzato solo con liste allora il tipo sarà una lista di elementi il cui tipo è determinato seguendo queste regole
- Se l'argomento è utilizzato con un singolo tipo di struttura allora il suo tipo sarà quello della struttura (che sarà tradotta come case class o case object, in base all'arità) 
- Se l'argomento è utilizzato con più strutture viene creato un trait che sarà esteso da ciascuna di esse, e il tipo dell'argomento sarà questo trait
- Se non è soddisfatta nessuna delle altre condizioni il tipo dell'argomento sarà `Any`
### Costruzione albero
Per questa fase si è utilizzata la libreria [treehugger](https://github.com/eed3si9n/treehugger), che permette di costruire l'albero di un programma scala e successivamente generarne il codice.

La traduzione del programma avviene a partire dalle direttive, che indicano quali predicati tradurre e come. Per tradurre un predicato si traducono le clauses corrispondenti. In base alla direzione (In/Out) degli argomenti si può determinare quali variabili sono conosciute all'inizio dell'esecuzione della clause, e queste permettono di determinare quali altri predicati sono necessari alla traduzione del predicato corrente.

Si consideri ad esempio
```prolog
#pred1: pred1(+arg1, +arg2, -arg3)
pred1(A, B, C) :- pred2(A,X,s1(B),s2(A, Y)), pred3(X,Y,C).
```
Si vuole quindi tradurre `pred1` con `arg1` e `arg2` come input, e `arg3` come output.
Inizialmente le variabili conosciute saranno `A B`, e la clause potrà essere tradotta solo se al termine della valutazione dei termini nel corpo anche la variabile `C` è conosciuta. Non tutti gli argomenti di `pred2` sono completamente conosciuti, quindi `pred2` dovrà essere tradotto con combinazione di input/output `+-+-`, e in seguito alla sua valutazione saranno conosciute le variabili `A B X Y`. Infine pred3 dovrà essere tradotto con input/output `++-`, aggiungendo anche `C` al set di variabili conosciute.
Se sia `pred2` che `pred3` possono essere tradotti con le combinazioni di i/o specificate allora questa clause di `pred1` potrà essere tradotta.

Man mano che si verifica la traducibilità di ciascun predicato se ne costruisce il relativo albero. Di seguito è rappresentato un esempio di traduzione e generazione completamente automatica di un programma.
```prolog
#lookup: lookup(+list, -elem, -position, -listNoElem)  
lookup([H|T],H,zero,T).  
lookup([H|T],E,s(N),[H|T2]):- lookup(T,E,N,T2).
```
```scala
object TranslatedProgram {
  trait Lookup_pos
  case class S(arg0: Lookup_pos) extends Lookup_pos
  case object Zero extends Lookup_pos
  def lookup[A1](list: List[A1]): Stream[(A1, Lookup_pos, List[A1])] = Predicate[List[A1], (A1, Lookup_pos, List[A1])](Fact({
    case h :: t => (h, Zero, t)
  }), Rule({
    case h :: t => for ((e, n, t2) <- lookup(t))
      yield (e, S(n), List(h) ++ t2)
  }))(list)
}
```

#### Treehugger
Questa libreria risulta particolarmente comoda in quanto per costruire l'albero del programma che si vuole generare si può utilizzare un DSL che rende l'operazione simile alla realizzazione di un normale programma scala.

In realtà non si è utilizzata la libreria treehugger, ma si è dovuto realizzare un  [fork](https://github.com/trema96/treehugger).
Questo è stato necessario perché trehugger non consente di utilizzare pattern alla sinistra di assegnamenti o di `<-` nei `for`, feature che risulta estremamente utile nei casi in cui si hanno per esempio delle collezioni di tuple (`(e, n, t2)  <- lookup(t)` nell'esempio precedente). Nel fork quindi si sono applicate le modifiche necessarie a consentire questa operazione
### Dettagli implementativi
Di seguito si evidenziano i dettagli relativi all'implementazione che si ritengono più interessanti
#### TranslationResult
L'operazione di traduzione potrebbe fallire per vari motivi (per esempio a causa di un errore nel programma prolog, oppure potrebbe non essere possibile tradurre il programma secondo le direttive fornite), e in vari step dell'operazione. Ciascuno step da risultati di tipi diversi, ma al primo step che fallisce si vuole interrompere l'intero processo di traduzione e restituire un messaggio contenente il motivo dell'errore.
Per fare questo si sono realizzati i `TranslationResult[+A]`. Un translation result può indicare un successo (`TranslationResult.Success`), o un fallimento (`TranslationResult.Failure`). Nel primo caso il translation result contiene un elemento di tipo A, nel secondo caso, invece, contiene un messaggio con i dettagli dell'errore.
Per consentire di utilizzare lo stesso `TranslationResult.Failure` al posto di un qualsiasi `TranslationResult[A]` questo estende `TranslationResult[Nothing]`.
Per permettere di manipolare il contenuto di un risultato si sono realizzati dei metodi `map` e `flatMap`. Infine si è realizzata una classe implicita che definisce metodi per semplificare la traduzione di sequenze di elementi.
#### Type aliases
Si è fatto spesso uso di type alias per specificare i tipi di collezioni ricorrenti.
Il loro uso ha portato vari vantaggi:
-Evidenziano il contenuto e lo scopo della collezione, ad esempio anziché avere una `Map[StructId, Seq[ArgumentTypeGroup]]`. si ha una `StructTypeMap`.
-Evidenziano il collegamento che c'è tra queste collezioni quando sono usate in punti diversi del codice.
-Semplificano operazioni di refactoring, quando si vuole ad esempio cambiare tipo degli elementi
Una possibile alternativa sarebbe stata quella di realizzare classi contenitori della collezione desiderata, ma gli alias in questa situazioni risultano più vantaggiosi, in quanto:
-Permettono di continuare a utilizzare normalmente i metodi della collezione
-Associando a ciascuno di essi una classe implicita si sono potuti "aggiungere" dei metodi
-Non si vuole controllare l'accesso ai dati

## Testing
Per testare il corretto funzionamento del sistema si è utilizzato scala test.
Si sono realizzati test per verificare il funzionamento sia del modulo di esecuzione che quello di traduzione.
Per verificare il corretto funzionamento dei programmi tradotti si è utilizzato scala-compiler per compilarli a runtime ed eseguirli con determinati input.

## Utilizzo
Per tradurre un programma prolog si utilizza innanzitutto il metodo `parse` dell'oggetto `Program`, passando in input il programma prolog da tradurre. Se il parsing ha successo in output si riceverà l'albero del programma prolog e, invocando su di esso il metodo `translate` si può ottenere il codice del corrispondente programma scala, o se l'operazione non ha successo la causa dell'errore.
```scala
val Parsed.Success(programTree, _) = Program.parse(programString)  
val TranslationResult.Success(scalaCode) = programTree.translate()
```

## Conclusioni
Il sistema realizzato è ancora limitato, ma è in grado di tradurre in maniera automatica alcuni dei programmi traducibili, chiedendo all'utente soltanto quali elementi del programma vuole tradurre.
Molti elementi del prolog non sono ancora supportati, come ad esempio la variabile `_`, numeri, operatori e `!`, ma non dovrebbe risultare complicato aggiungerne il supporto in futuro.

L'utilizzo di scala per la realizzazione di questo progetto è stato vantaggioso per vari aspetti. Innanzitutto la realizzazione del parser è stata molto semplice e veloce. La libreria fastparse permette permette di scrivere parser in maniera rapida e il codice risultate risulta semplice da capire. Inoltre le case classes di scala sono risultate particolarmente utili nell'implementazione dei nodi dell'albero del programma prolog.
Un'altro vantaggio di scala sono le feature che il linguaggio offre. Nella traduzione di programmi prolog si fa largo uso di feature come pattern matching e for comprehension, che semplificano la struttura del programma risultante. Se si fosse dovuto realizzare un analogo traduttore, ma da prolog a java le operazioni di traduzione non sarebbero state altrettanto semplici.
