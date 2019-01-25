


let pesi1 =[(2,1,1);(2,1,3);(1,1,5);(5,1,2);(5,1,6);(3,1,6);(6,1,7);(4,1,3)];;

let f = function
1 -> [5]
| 2 -> [1;3]
| 3 -> [6]
| 4 -> [3]
| 5 -> [2;6]
| 6 -> [7]
| _ -> [];;


type 'a graph = Graph of ('a -> 'a list);;
let g = Graph f;;

let pesi2 =[(1,1,2);(2,1,3);(3,1,4);(4,1,3);(4,1,5);(6,1,5)];;

let f1 = function
1 -> [2]
| 2 -> [3]
| 3 -> [4]
| 4 -> [5;3]
| 6 -> [5]
| _ -> [];;

let g1 = Graph f1;; 
(*i grafi vengono definiti mediante la funzione dei successori, e le lunghezze vengono definite mediante una lista di 
triple, dove la generica tripla ha il seguente significato (ingresso,lunghezza,arrivo)*)



(*l'assunzione è che i pesi devono essere non negativi e diversi da 0*)
(*nel caso di link bidirezionali, nella lista vanno specificati sia costo di andata e costo di ritorno*)

exception NotFound;;
exception Errore;;
(*la funzione rimuovitesta mi serve per rimuovere il duplicato nel cammino finale, data la chiamata alla seconda funzione*)
let rec rimuovitesta=function
    []->raise Errore
    |testa::coda->coda
(*la funzione cerca, prende due coppie di interi, e una lista di lunghezze, e restituisce la lunghezza associata all'arco che ha come estremi proprio i due nodi*)
let rec cerca x y= function
    []->0
    |(a,b,c)::resto -> if((a=x && c=y))then b else cerca x y resto;;

(*la funzione costocamm si avvale della funzione costocamm_ottimo che grazie alla ricorsione di coda permette di calcolare in modo efficiente
il costo del cammino passato come parametro basandosi sui pesi che sono passati come secondo parametro.*)
let costocamm cammino pesi= 
let rec costocamm_ottimo costo=function
    []->raise Errore
    |x::y::rest -> costocamm_ottimo ( costo + cerca x y pesi ) (y::rest)
    |_::[]->costo 
in costocamm_ottimo 0 cammino;;

(*questa funzione stampa semplicemente la lista del cammino ogni volta che questa viene espansa, usata e lasciata per debug*)
let rec stampalista = function [] -> print_newline()
    | x::rest -> print_int(x); print_string("; "); stampalista rest;;

let arrivo_fine inizio fine (Graph succ) =
        let estendia cammino = (*stampalista cammino;*)
             List.map (function x -> x::cammino)
            (List.filter (function x -> not (List.mem x cammino)) (succ (List.hd cammino)))
        in let rec search_auxa fine = function
            [] -> raise NotFound
            | cammino::rest -> if ((fine = List.hd cammino) ) then List.rev cammino         
                    else search_auxa fine ((estendia cammino) @ rest)
in search_auxa fine [[inizio]];;

(*la funzione searchp esegue una ricerca in profondità considerando anche i cicli, non appena soddisfa il k, 
controlla se il cammino è esattamente la soluzione, altrimenti, ritorna alla fine chiamando la funzione arrivo_fine*)
let searchp inizio fine k pesi (Graph succ)=
        let estendi cammino = (*stampalista cammino;*)
            List.map (function x -> x::cammino)
             (succ (List.hd cammino))            
        in let rec search_aux fine = function
            [] -> raise NotFound
            | cammino::rest -> if ((costocamm (List.rev cammino) pesi >=k) && (List.hd cammino == fine) )then List.rev cammino 
                                else if((costocamm (List.rev cammino) pesi) >=k)  
                                        then (List.rev (rimuovitesta cammino)) @ arrivo_fine (List.hd cammino) (fine) (Graph succ) (*devo rimuovere la coda dalla prima lista a sinistra*)      
                    else search_aux fine ((estendi cammino) @ rest)
in search_aux fine [[inizio]];;



(*la funzione cerca_cammino serve per richiamare l'esecuzione della funzione searchp*)
let cerca_cammino inizio fine k pesi (Graph succ)=
    let risultato = (searchp inizio fine k pesi (Graph succ)) in 
    print_string("il risultato trovato è il seguente = "); stampalista risultato ;
     print_string("il costo del cammino è il seguente = ");   print_int (costocamm risultato pesi); print_newline();;
    
    

 