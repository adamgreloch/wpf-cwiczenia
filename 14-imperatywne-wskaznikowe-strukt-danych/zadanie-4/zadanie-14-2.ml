(*
Dana jest definicja typu elementów tworzących listy wskaźnikowe:
type a' option = None | Some of a'
type a' elem = {v: a'; mutable next: a' lista}
and a' lista = a' elem option
(a) Napisz procedurę petla : lista → unit, która mając daną listę jednokierunkową,
tworzy z niej listę cykliczną, ale z odwróconym kierunkiem wskaźników. Możesz
założyć, że dana lista jest poprawną listą jednokierunkową, to znaczy ma koniec i
nie zapętla się.
(b) Napisz procedurę przeplot : lista → lista → unit, która splata obie listy w
jedną listę postaci: pierwszy rekord pierwszej listy, pierwszy rekord drugiej listy,
drugi rekord pierwszej listy, drugi rekord drugiej listy, itd. Jeśli jedna z list jest
dłuższa, to jej końcówka stanowi końcówkę listy wynikowej.
*)
