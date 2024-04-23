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

  // beginswith
  //
  // a wrapper function for StartsWith that ensures that the
  // literal parameter is a string
  let beginswith (pattern : string) (literal : string) =
    literal.StartsWith (pattern)


  // sliceIdentifier
  //
  // splits an identifier token into two segments:
  // 1) the segment "BLANK_identifier:"
  // 2) the segment after the ':' (the identifier/variable name)
  // returns the variable name
  let sliceIdentifier (input : string) =
    let identifierSegments = input.Split(':') // splits into two segments

    if identifierSegments.Length > 1 then
      identifierSegments.[1] // returns name segment
    else
      ""


  // matchToken
  //
  // when called, checks if the next token matches the expected token
  // an exception is thrown if no match is found; otherwise,
  // returns the rest of the tokens
  let private matchToken tokens symbols =
    List.tail tokens

  // createSymbolTuple
  //
  // creates a tuple in the format of (type, variable name)
  // this is eventually used to add a new symbol pair to the symbols table
  let private createSymbolTuple varType identifier =
    let varName = sliceIdentifier identifier
    (varName, varType)


  // checkForDups
  //
  // checks through the symbols table to determine if symbolPair already exists
  // if a duplicate is found, return true; otherwise, return false
  let rec checkForDups symbolPair symbols =
    match symbols with
    | hd::tl -> if fst hd = fst symbolPair then // checks first element of hd and symbolPair
                  true
                else
                  checkForDups symbolPair tl
    | [] -> false


  // checkIfDeclared
  //
  // checks through the symbols table to determine if identifier exists
  // if the identifier is found, return true; otherwise, return false
  let rec checkIfDeclared identifier symbols =
    match symbols with
    | hd::tl -> if fst hd = identifier then // checks first element of hd tuple and compares
                  true
                else
                  checkIfDeclared identifier tl
    | [] -> false

  // parseTokens
  //
  // checks through the tokens to find new symbols (reals and ints)
  // when a new symbol is found, adds it to the table
  //
  // performs error checking for redefinition of variables and
  // variables that haven't been defined
  let rec private parseTokens tokens symbols =
    match tokens with
    | hd::tl -> if hd = "int" || hd = "real" then
                  let toAdd = createSymbolTuple hd (List.head tl)
                  if checkForDups toAdd symbols then // redefinition branch
                    failwith ("redefinition of variable '" + fst toAdd + "'")
                  else
                    parseTokens tl ([toAdd]@symbols) // adds toAdd to the symbols table and continues
                else
                  if beginswith "identifier" hd then
                    let identifier = sliceIdentifier hd // gets the variable name
                    if not (checkIfDeclared identifier symbols) then // undefined variable
                      failwith ("variable '" + identifier + "' undefined")
                    else
                      parseTokens tl symbols
                  else
                    parseTokens tl symbols
    | [] -> symbols

  // simpleC
  //
  // returns a tuple of (tokens, symbols table)
  let private simpleC tokens =
    let symbols = parseTokens tokens []
    (tokens, symbols)


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
