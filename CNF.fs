module CNF

open PropositionalLogic
open FormulaTransformations

let rec cnf(phi: NegatedNormalFormFormula) : ConjunctiveNormalFormFormula =
    match phi with
    | NegatedNormalFormFormula.TOP -> ConjunctiveNormalFormFormula.TOP
    | NegatedNormalFormFormula.BOT -> ConjunctiveNormalFormFormula.BOT
    | NegatedNormalFormFormula.ATOM(atom) -> ConjunctiveNormalFormFormula.ATOM (atom)
    | NegatedNormalFormFormula.NOT(form) -> ConjunctiveNormalFormFormula.NOT (form)
    | NegatedNormalFormFormula.AND (form1, form2) -> ConjunctiveNormalFormFormula.AND (cnf form1, cnf form2)
    | NegatedNormalFormFormula.OR (form1, form2) -> distr (cnf form1) (cnf form2)