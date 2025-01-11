open PropositionalLogic
open FormulaTransformations
open CNF

let formula = IMPL (AND (NOT (ATOM ("p")), ATOM ("q")), AND(ATOM ("p"), IMPL (ATOM ("r"), ATOM ("q"))))

let step1 = impl_free formula

// printfn "%A" step1

let step2 = nne step1

// printfn "%A" step2

let step3 = cnf step2

printf "%A" step3