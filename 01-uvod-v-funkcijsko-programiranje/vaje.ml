(*----------------------------------------------------------------------------*
 # Uvod v funkcijsko programiranje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Vektorji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razteg : float -> float list -> float list`, ki vektor,
 predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let razteg k v = List.map (fun x -> k *. x) v
let razteg2 k v = List.map (( *. ) k) v

let primer_vektorji_1 = razteg 2.0 [1.0; 2.0; 3.0]
(* val primer_vektorji_1 : float list = [2.; 4.; 6.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sestej : float list -> float list -> float list`, ki vrne
 vsoto dveh vektorjev.
[*----------------------------------------------------------------------------*)

let sestej u v = List.map2 ((+.)) u v
let sestej u v = List.map2 (fun x y -> x +. y) u v
let primer_vektorji_2 = sestej [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_2 : float list = [5.; 7.; 9.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `skalarni_produkt : float list -> float list -> float`, ki
 izračuna skalarni produkt dveh vektorjev. Pri tem si lahko pomagate s funkcijo
 `vsota_seznama : float list -> float`, definirano prek funkcije
 `List.fold_left`, ki jo bomo spoznali kasneje:
[*----------------------------------------------------------------------------*)

let vsota_seznama = List.fold_left (+.) 0.

let skalarni_produkt v1 v2 = 
  List.combine v1 v2
  |> List.map (fun (x, y) -> x *. y)
  |> vsota_seznama
let skalarni_produkt2 v1 v2 = vsota_seznama (List.map2 ( *. ) v1 v2)
let skalarni_produkt3 v1 v2 = List.map2 (( *. ) ) v1 v2 |> vsota_seznama
let primer_vektorji_3 = skalarni_produkt [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_3 : float = 32. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `norma : float list -> float`, ki vrne evklidsko normo
 vektorja.
[*----------------------------------------------------------------------------*)

let norma v = 
  let kvadrat x = x *. x in
  List.map kvadrat v
  |> vsota_seznama
  |> sqrt

let primer_vektorji_4 = norma [3.0; 4.0]
(* val primer_vektorji_4 : float = 5. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vmesni_kot : float list -> float list -> float`, ki izračuna
 kot med dvema vektorjema v radianih.
[*----------------------------------------------------------------------------*)

let vmesni_kot a b = 
  let stevec = skalarni_produkt a b in
  let imenovalec = norma a *. norma b in
  acos (stevec /. imenovalec)

let primer_vektorji_5 = vmesni_kot [1.0; 0.0] [0.0; 1.0]
(* val primer_vektorji_5 : float = 1.57079632679489656 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `normirani : float list -> float list`, ki normira dani
 vektor.
[*----------------------------------------------------------------------------*)

let normirani v = 
  let dolzina = norma v in
  List.map (fun x -> x /. dolzina) v
let normirani2 v = razteg (1. /. norma v) v
let primer_vektorji_6 = normirani [3.0; 4.0]
(* val primer_vektorji_6 : float list = [0.600000000000000089; 0.8] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `projeciraj : float list -> float list -> float list`, ki
 izračuna projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projekcija prvi drugi = 
  let stevec = skalarni_produkt prvi drugi in
  let imenovalec = norma drugi *. norma drugi in
  List.map (fun x -> x *. (stevec /. imenovalec)) drugi

let primer_vektorji_7 = projekcija [3.0; 4.0] [1.0; 0.0]
(* val primer_vektorji_7 : float list = [3.; 0.] *)

(*----------------------------------------------------------------------------*
 ## Generiranje HTML-ja
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML
 oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.
[*----------------------------------------------------------------------------*)

let ovij oznaka vsebina = "<" ^ oznaka ^ ">" ^ vsebina ^ "</" ^ oznaka ^ ">"

let primer_html_1 = ovij "h1" "Hello, world!"
(* val primer_html_1 : string = "<h1>Hello, world!</h1>" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število
 presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za
 ustrezno število presledkov.
[*----------------------------------------------------------------------------*)

let zamakni zamik niz = 
  let presledki = String.make zamik ' ' in
  let vrstice = String.split_on_char '\n' niz in
  let zamaknjene_vrstice = List.map (fun vrstica -> presledki ^ vrstica) vrstice in
  String.concat "\n" zamaknjene_vrstice
let zamakni2 zamik niz = 
  niz 
  |> String.split_on_char '\n'
  |> List.map (fun vrstica -> String.make zamik ' ' ^ vrstica)
  |> String.concat "\n"
let primer_html_2 = zamakni 4 "Hello,\nworld!"
(* val primer_html_2 : string = "    Hello,\n    world!" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne
 niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:
[*----------------------------------------------------------------------------*)

let ul seznam_nizov = 
  let li_elementi = List.map (fun niz -> ovij "li" niz) seznam_nizov in
  String.concat "\n" li_elementi

let primer_html_3 = ul ["ananas"; "banana"; "čokolada"]
(* val primer_html_3 : string =
  "<ul>\n  <li>ananas</li>\n  <li>banana</li>\n  <li>čokolada</li>\n</ul>" *)

(*----------------------------------------------------------------------------*
 ## Nakupovalni seznam
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme
 niz, ki vsebuje vejico, loči na del pred in del za njo.
[*----------------------------------------------------------------------------*)

let razdeli_vrstico niz = 
  let deli = String.split_on_char ',' niz in
  match deli with
  | [pred; za] -> (String.trim pred, String.trim za)
  | _ -> failwith "Napačna oblika vrstice"
let razdeli_vrstico2 niz = 
  let vejica = String.index niz ',' in
  let levi = String.sub niz 0 vejica in
  let desni = String.sub niz (vejica + 1) (String.length niz - vejica - 1) in
  levi, desni
let primer_seznam_1 = razdeli_vrstico "mleko, 2"
(* val primer_seznam_1 : string * string = ("mleko", "2") *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`,
 ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike `"izdelek,
 vrednost"`, in vrne seznam ustreznih parov.
[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov niz = 
  let vrstice = String.split_on_char '\n' niz in
  List.map razdeli_vrstico vrstice
let pretvori_v_seznam_parov2 niz = 
  String.split_on_char '\n' niz
  |> List.map razdeli_vrstico
let primer_seznam_2 = pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"
(* val primer_seznam_2 : (string * string) list =
  [("mleko", "2"); ("kruh", "1"); ("jabolko", "5")] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list
 -> (string * 'b) list`, ki dano funkcijo uporabi na vseh drugih komponentah
 elementov seznama.
[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente f seznam = 
  List.map (fun (x, y) -> (x, f y)) seznam

let primer_seznam_3 =
  let seznam = [("ata", "mama"); ("teta", "stric")] in
  pretvori_druge_komponente String.length seznam
(* val primer_seznam_3 : (string * int) list = [("ata", 4); ("teta", 5)] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki
 sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni
 znesek nakupa.
[*----------------------------------------------------------------------------*)

let izracunaj_skupni_znesek cenik nakupovalni_seznam = 
  let seznam_cen = pretvori_v_seznam_parov cenik in
  let seznam_nakupov = pretvori_v_seznam_parov nakupovalni_seznam in
  let seznam_cen_float = pretvori_druge_komponente float_of_string seznam_cen in
  let seznam_nakupov_float = pretvori_druge_komponente float_of_string seznam_nakupov in
  let rec najdi_ceno izdelek = function
    | [] -> failwith ("Izdelek " ^ izdelek ^ " ni v ceniku")
    | (ime, cena) :: _ when ime = izdelek -> cena
    | _ :: ostalo -> najdi_ceno izdelek ostalo
  in
  List.fold_left (fun acc (izdelek, kolicina) ->
    let cena = najdi_ceno izdelek seznam_cen_float in
    acc +. (cena *. kolicina)
  ) 0.0 seznam_nakupov_float

let primer_seznam_4 = 
  let nakupovalni_seznam = "mleko, 2\njabolka, 5"
  and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
  izracunaj_skupni_znesek cenik nakupovalni_seznam
(* val primer_seznam_4 : float = 5.5 *)
