module PropositionalLogic

type Atom = string

type Literal = ATOM of Atom | TOP | BOT

type ImplFreeFormula = 
    | TOP
    | BOT
    | ATOM    of Atom
    | NOT     of ImplFreeFormula
    | AND     of ImplFreeFormula * ImplFreeFormula
    | OR      of ImplFreeFormula * ImplFreeFormula

type NegatedNormalFormFormula = ImplFreeFormula

type ConjunctiveNormalFormFormula = ImplFreeFormula

type Formula = 
    | TOP
    | BOT
    | ATOM    of Atom
    | NOT     of Formula
    | AND     of Formula * Formula
    | OR      of Formula * Formula
    | IMPL    of Formula * Formula

type Evaluation = Map<Atom, bool>

let get_value (evaluation: Evaluation) (atom: Atom) = evaluation.Item atom

let rec evaluate (formula: Formula) (evaluation: Evaluation) : bool = 
    match formula with
    | TOP -> true
    | BOT -> false
    | ATOM(atom) -> get_value evaluation atom
    | NOT(formula) -> not (evaluate formula evaluation)
    | AND(form1, form2) -> (evaluate form1 evaluation) && (evaluate form2 evaluation)
    | OR(form1, form2) -> (evaluate form1 evaluation) || (evaluate form2 evaluation)
    | IMPL(form1, form2) -> 
        match (evaluate form1 evaluation, evaluate form2 evaluation) with
        | (true, false) -> false
        | _ -> true

