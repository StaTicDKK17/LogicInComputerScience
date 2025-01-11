module FormulaTransformations

open PropositionalLogic

let rec impl_free (formula: Formula) : Formula =
    match formula with
    | TOP -> TOP
    | BOT -> BOT
    | ATOM(atom) -> ATOM atom
    | NOT(form) -> NOT (impl_free form)
    | AND(form1, form2) -> AND (impl_free form1, impl_free form2)
    | OR(form1, form2) -> OR (impl_free form1, impl_free form2)
    | IMPL(form1, form2) -> OR (NOT (impl_free form1), impl_free form2)

and nnf (formula: Formula) : Formula =
    // printfn "%A" formula
    match formula with
    | TOP -> TOP
    | BOT -> BOT
    | ATOM(atom) -> ATOM(atom)
    | NOT(form) -> 
        match form with
        | ATOM(atom) -> NOT (ATOM (atom))
        | BOT -> TOP
        | TOP -> BOT
        | AND(aForm1, aForm2) -> OR (nnf (NOT aForm1), nnf (NOT aForm2))
        | OR(oForm1, oForm2) -> AND (nnf (NOT oForm1), nnf (NOT oForm2))
        | NOT(form) -> form
        | IMPL(_, _)-> nnf form
    | AND(form1, form2) -> AND (nnf form1, nnf form2)
    | OR(form1, form2) -> OR (nnf form1, nnf form2)
    | IMPL(form1, form2) -> OR (NOT (nnf form1), nnf form2)

and distr (n1: Formula) (n2: Formula) : Formula =
    match (n1, n2) with
    | (AND (n11, n12), _) -> AND (distr n11 n2, distr n12 n2)
    | (_, AND (n21, n22)) -> AND (distr n1 n21, distr n1 n22)
    | _ -> OR (n1, n2)