//
// Analyzer for SimpleC programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
//
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//
//   [("x", "int"); ("y", "int"); ("z", "real")]
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

module analyzer =
  //
  // NOTE: all functions in the module must be indented.
  //

  // beginswith
  //
  // a wrapper function for StartsWith that ensures that the
  // literal parameter is a string
  let beginswith (pattern : string) (literal : string) =
    literal.StartsWith (pattern)
    

  let sliceIdentifier (input : string) =
    let identifierSegments = input.Split(':')

    if identifierSegments.Length > 1 then
      identifierSegments.[1]
    else
      ""
  

  // matchToken
  // 
  // when called, checks if the next token matches the expected token
  // an exception is thrown if no match is found; otherwise, 
  // returns the rest of the tokens
  let private matchToken tokens symbols =
    List.tail tokens
  

  let rec private empty tokens symbols =
    matchToken tokens symbols // ;


  let rec private vardecl tokens symbols =
    let nextToken = List.head tokens
    let nextNextToken = List.head (List.tail tokens)

    // gets only the part after the ':'; 
    // ex : gets 'x' from 'identifier:x'
    let identifier = sliceIdentifier nextNextToken
    
    let toAdd = (identifier, nextToken)

    let T2 = matchToken tokens symbols // int/real
    let T3 = matchToken T2 symbols // identifier
    matchToken tokens toAdd::symbols // ; + adds new var declarations
  

  let rec private input tokens symbols =
    let T2 = matchToken tokens symbols // cin
    let T3 = matchToken T2 symbols // >>
    matchToken tokens symbols // identifier


  let rec private expr_value tokens symbols =
    matchToken tokens symbols // true/false + identifier + int/str_literal


  let rec private expr_op tokens symbols =
    matchToken tokens symbols // all expr-ops


  let rec private expr tokens symbols =
    let nextNextToken = List.head (List.tail tokens)

    // all valid operators; for checking with the nextNextToken
    let operators = ["+"; "-"; "*"; "/"; "^"; "<"; "<="; ">"; ">="; "=="; "!="]

    if List.contains nextNextToken operators then
      let T2 = expr_value tokens symbols
      let T3 = expr_op T2 symbols
      expr_value T3 symbols
    else
      expr_value tokens symbols


  let rec private output_value tokens symbols =
    let nextToken = List.head tokens
    
    if nextToken = "endl" then
      matchToken tokens symbols // endl
    else
      expr_value tokens symbols


  let rec private output tokens symbols =
    let T2 = matchToken tokens symbols // cout
    let T3 = matchToken T2 symbols // <<
    let T4 = output_value T3 symbols
    matchToken T4 symbols


  let rec private assignment tokens symbols =
    let T2 = matchToken tokens symbols
    let T3 = matchToken T2 symbols
    let T4 = expr T3 symbols
    matchToken T4 symbols


  let rec private condition tokens symbols =
    expr tokens symbols


  let rec private stmt tokens symbols =
    let nextToken = if beginswith "identifier" (List.head tokens) then "identifier" else List.head tokens

    match nextToken with
    | ";" -> empty tokens symbols
    | "int" -> vardecl tokens symbols
    | "real" -> vardecl tokens symbols
    | "cin" -> input tokens symbols
    | "cout" -> output tokens symbols
    | "identifier" -> assignment tokens symbols
    | "if" -> ifstmt tokens symbols
    | "else" -> else_part tokens symbols
    | _ -> symbols // return symbols table
  

  and private then_part tokens symbols =
    stmt tokens symbols


  and private else_part tokens symbols =
    let nextToken = List.head tokens

    if nextToken = "else" then
      let T2 = matchToken tokens symbols
      stmt T2 symbols
    else
      tokens
  

  and private ifstmt tokens symbols =
    let T2 = matchToken tokens symbols // if
    let T3 = matchToken T2 symbols // (
    let T4 = condition T3 symbols
    let T5 = matchToken T4 symbols // )
    let T6 = then_part T5 symbols
    else_part T6 symbols

  let rec private morestmts tokens symbols =
    let nextToken = List.head tokens

    if nextToken <> "}" then
      let T2 = stmt tokens symbols
      morestmts T2 symbols
    else
      (tokens, symbols)

  let rec private stmts tokens symbols =
    let T2 = stmt tokens symbols
    morestmts T2 symbols


  let private simpleC tokens = 
    let T2 = matchToken tokens []
    let T3 = matchToken tokens []
    let T4 = matchToken tokens []
    let T5 = matchToken tokens []
    let T6 = matchToken tokens []
    let T7 = stmts T6 []
    let T8 = matchToken tokens T7.[1]
    let T9 = matchToken tokens T7.[1] // end-of-file; no more tokens
    (tokens, T7.[1])


  //
  // build_symboltable tokens
  //
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  //
  // On success, the symboltable is a list of tuples of the
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])
