//
// Analyzer for SimpleC programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input
// program is legal, otherwise the string "type_error: ..."
// is returned denoting an invalid SimpleC program.
//
// Modified by:
//   Logan Lucas
//
// Original author:
//   Prof. Joe Hummel
//   University of Illinois Chicago
//   CS 341, Spring 2022
// Modified by:
//   Ellen Kidane
//   University of Illinois Chicago
//   CS 341, Spring 2024
//

namespace compiler

module checker =
  //
  // NOTE: all functions in the module must be indented.
  //

  // beginswith
  //
  // a wrapper function for StartsWith that ensures that the
  // literal parameter is a string
  let beginswith (pattern : string) (literal : string) =
    literal.StartsWith (pattern)


  let checkArithmetic tokens symbols =
    printfn "checkArithmetic"
    let firstOperand = List.head tokens
    let secondOperand = List.head (List.tail (List.tail tokens))

    printfn "%A" firstOperand
    printfn "%A" secondOperand
    true


  let checkComparison tokens symbols =
    printfn "checkComparison"

  let checkDeclaration tokens symbols =
    printfn "checkDeclaration"

  let checkAssignment tokens symbols =
    printfn "checkAssignment"
    let arithmeticOps = ["+"; "-"; "*"; "/"; "^"]
    if List.contains (List.head (List.tail(List.tail(List.tail tokens)))) arithmeticOps then
      checkArithmetic (List.tail(List.tail tokens)) symbols
    else
      true

  let checkIfStmt tokens symbols =
    printfn "checkIfStmt"

  let rec private parseTokens tokens symbols =
    let nextToken = List.head tokens

    let arithmeticOps = ["+"; "-"; "*"; "/"; "^"]
    let comparisonOps = ["<"; "<="; ">"; ">="; "=="; "!="]

    match tokens with
    | hd::tl -> if List.contains (List.head tl) comparisonOps then
                  let result = checkComparison tokens symbols
                  parseTokens (List.tail tokens) symbols
                elif hd = "int" || hd = "real" then
                  let result = checkDeclaration tokens symbols
                  parseTokens (List.tail tokens) symbols
                elif (List.head tl) = "=" then
                  let result = checkAssignment tokens symbols
                  parseTokens (List.tail tokens) symbols
                elif hd = "if" then
                  let result = checkIfStmt tokens symbols
                  parseTokens (List.tail tokens) symbols
                else
                  parseTokens (List.tail tokens) symbols
    | _ -> printfn "Done!"



  let private simpleC tokens symboltable =
    parseTokens tokens symboltable


  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable =
    try
      let T2 = simpleC tokens symboltable
      "success"
    with
      | ex -> "type_error: " + ex.Message

