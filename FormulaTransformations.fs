module FormulaTransformations

open PropositionalLogic

let rec impl_free (formula: Formula) : ImplFreeFormula =
    match formula with
    | TOP -> ImplFreeFormula.TOP
    | BOT -> ImplFreeFormula.BOT
    | ATOM(atom) -> ImplFreeFormula.ATOM atom
    | NOT(form) -> ImplFreeFormula.NOT (impl_free form)
    | AND(form1, form2) -> ImplFreeFormula.AND (impl_free form1, impl_free form2)
    | OR(form1, form2) -> ImplFreeFormula.OR (impl_free form1, impl_free form2)
    | IMPL(form1, form2) -> ImplFreeFormula.OR (ImplFreeFormula.NOT (impl_free form1), impl_free form2)

and nne (formula: ImplFreeFormula) : NegatedNormalFormFormula =
    // printfn "%A" formula
    match formula with
    | ImplFreeFormula.TOP -> NegatedNormalFormFormula.TOP
    | ImplFreeFormula.BOT -> NegatedNormalFormFormula.BOT
    | ImplFreeFormula.ATOM(atom) -> NegatedNormalFormFormula.ATOM(atom)
    | ImplFreeFormula.NOT(form) -> 
        match form with
        | ImplFreeFormula.ATOM(atom) -> NegatedNormalFormFormula.NOT (NegatedNormalFormFormula.ATOM (atom))
        | ImplFreeFormula.BOT -> NegatedNormalFormFormula.TOP
        | ImplFreeFormula.TOP -> NegatedNormalFormFormula.BOT
        | ImplFreeFormula.AND(aForm1, aForm2) -> NegatedNormalFormFormula.OR (nne (ImplFreeFormula.NOT aForm1), nne (ImplFreeFormula.NOT aForm2))
        | ImplFreeFormula.OR(oForm1, oForm2) -> NegatedNormalFormFormula.AND (nne (ImplFreeFormula.NOT oForm1), nne (ImplFreeFormula.NOT oForm2))
        | ImplFreeFormula.NOT(form) -> form
    | ImplFreeFormula.AND(form1, form2) -> NegatedNormalFormFormula.AND (nne form1, nne form2)
    | ImplFreeFormula.OR(form1, form2) -> NegatedNormalFormFormula.OR (nne form1, nne form2)

and distr (n1: ConjunctiveNormalFormFormula) (n2: ConjunctiveNormalFormFormula) : ConjunctiveNormalFormFormula =
    match (n1, n2) with
    | (ConjunctiveNormalFormFormula.AND (n11, n12), _) -> ConjunctiveNormalFormFormula.AND (distr n11 n2, distr n12 n2)
    | (_, ConjunctiveNormalFormFormula.AND (n21, n22)) -> ConjunctiveNormalFormFormula.AND (distr n1 n21, distr n1 n22)
    | _ -> ConjunctiveNormalFormFormula.OR (n1, n2)