module CNF

open PropositionalLogic
open FormulaTransformations

let rec cnf(phi: Formula) : Formula =
    match phi with
    | TOP -> TOP
    | BOT -> BOT
    | ATOM(atom) -> ATOM (atom)
    | NOT(form) -> NOT (form)
    | AND (form1, form2) -> AND (cnf form1, cnf form2)
    | OR (form1, form2) -> distr (cnf form1) (cnf form2)
    | _ -> phi // should not happen if called correctly