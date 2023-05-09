
(* Code review Zosia Rusiłowicz *)
open List;;

(* Punkt na płaszczyźnie *)
type point = float * float;;

(* Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int;;

(* Mówi po której stronie prostej leży punkt *)
type det = Po_lewej | Na | Po_prawej;;

(* [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
(lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
w pozostałych przypadkach 0 razy *)
let prostokat ((x1, y1) : point) ((x2, y2) : point) : kartka =
  fun (xp, yp) -> 
    if x1 <= xp && xp <= x2 && y1 <= yp && yp <= y2 
      then 1 
      else 0;;

(* [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku w punkcie [p] i promieniu [r] 
Gdy w kartkę tę wbije się szpilkę wewnątrz
(lub na krawędziach) kola, kartka zostanie przebita 1 raz,
w pozostałych przypadkach 0 razy *)
let kolko ((x1, y1) : point) r : kartka =
  fun (xp, yp) -> 
    if sqrt (abs_float ((xp -. x1) *. (xp -. x1)) 
      +. abs_float((yp -. y1) *. (yp -. y1))) <= r
      then 1 
      else 0;;

(* Sprawdza gdzie leży punkt (x3, y3) względem prostej wyznaczonej przez punkty (x1, y1) (x2, y2) *)
let wyznacznik ((x1, y1) : point) ((x2, y2) : point) ((x3, y3) : point) =
  let det = x1 *. y2 +. x2 *. y3 +. x3 *. y1 -. y2 *. x3 -. y3 *. x1 -. y1 *. x2 in
  if det > 1e-14 then Po_lewej 
  else if det < -1e-14 then Po_prawej 
  else Na;;

(* Wylicza współrzędne punktu odbicia lustrzanego punktu (x3, y3)
Względem prostej wyznaczonej przez punkty (x1, y1) (x2, y2) *)
let hokuspokus ((x1, y1) : point) ((x2, y2) : point) ((x3, y3) : point) =
  if x1 = x2 then (2. *. x1 -. x3, y3) else
    let a, b, c =
      let m = (y2 -. y1) /. (x2 -. x1) in
      (m, -1., y1 -. m *. x1) in
    let temp = (-2.) *. (a *. x3 +. b *. y3 +. c) /. (a *. a +. b *. b) in
    let x = temp *. a +. x3 in
    let y = temp *. b +. y3 in
    (x, y);;

(*   [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
przebicie po prawej stronie prostej powinno więc zwrócić 0.
Przebicie dokładnie na prostej powinno zwrócić tyle samo,
co przebicie kartki przed złożeniem. Po stronie lewej -
tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
który nałożył się na punkt przebicia. *)
let zloz (p1 : point) (p2 : point) (k : kartka) : kartka =
  fun p3 ->
    match wyznacznik p1 p2 p3 with
    | Po_lewej -> (k p3) + k (hokuspokus p1 p2 p3)
    | Na -> k p3
    | Po_prawej -> 0;;

(* [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
z listy *) 
let skladaj l k =
  let f ks (p1, p2) = zloz p1 p2 ks in
  fold_left f k l;;
