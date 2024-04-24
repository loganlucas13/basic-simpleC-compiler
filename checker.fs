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
  

  // getType
  //
  // matches a variable name with its type
  // runs through the symbols table
  // returns the type of the variable
  let rec getType nameToFind symbols =
    match symbols with
    | (varName, varType)::tl -> if varName = nameToFind then
                                  varType
                                else
                                  getType nameToFind tl // recursive step
    | [] -> "" // passes an empty type if not found (theoretically, this should never happen)


  // matchToken
  //
  // this version of matchToken does NO checking for
  // expected_token and tokens
  let private matchToken expected_token tokens =
    List.tail tokens


  // empty
  //
  // BNF:
  // ;
  let rec private empty tokens symbols =
    matchToken ";" tokens
  

  // vardecl
  //
  // BNF:
  // int identifier; | real identifier
  let rec private vardecl tokens symbols =
    let nextToken = List.head tokens
    
    if nextToken = "int" then
      let T2 = matchToken "int" tokens
      let T3 = matchToken "identifier" T2
      matchToken ";" T3
    else
      let T2 = matchToken "real" tokens
      let T3 = matchToken "identifier" T2
      matchToken ";" T3
  

  // input
  //
  // BNF:
  // cin >> identifier;
  let rec private input tokens symbols =
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    matchToken "identifier" T3
  

  // expr-value
  // 
  // BNF:
  // identifier | int_literal | real_literal | str_literal | true | false
  let rec private expr_value tokens symbols =
    let nextToken = List.head tokens

    // the checks below are to pass the correct type as a return
    if nextToken = "true" || nextToken = "false" then
      (matchToken nextToken tokens, "bool")
    elif beginswith "identifier" nextToken then
      (matchToken nextToken tokens, getType (sliceIdentifier nextToken) symbols)
    elif beginswith "int_literal" nextToken then
      (matchToken nextToken tokens, "int")
    elif beginswith "str_literal" nextToken then
      (matchToken nextToken tokens, "str")
    elif beginswith "real_literal" nextToken then
      (matchToken nextToken tokens, "real")
    else
      failwith ("expecting identifier or literal, but found " + nextToken)


  // expr_op
  // 
  // BNF:
  // + | - | * | / | ^ | < | <= | > | >= | == | !=
  let rec private expr_op tokens symbols =
    let nextToken = List.head tokens

    let valid = ["+"; "-"; "*"; "/"; "^"; "<"; "<="; ">"; ">="; "=="; "!="]

    if List.contains nextToken valid then // checks if the nextToken is included in the operator list
      matchToken nextToken tokens
    else
      failwith ("expecting expression operator, but found " + nextToken)
  

  // expr
  //
  // BNF:
  // <expr-value> <expr-op> <expr-value>
  // <expr-value>
  let rec private expr tokens symbols =
    let nextToken = List.head tokens // the first expr-value
    let nextNextToken = List.head (List.tail tokens) // the operator symbol, if it exists
    let nextNextNextToken = List.head (List.tail (List.tail tokens)) // the second expr-value, if it exists

    // all valid operators; for checking with the nextNextToken
    let arithmeticOps = ["+"; "-"; "*"; "/"; "^"]
    let comparisonOps = ["<"; "<="; ">"; ">="; "=="; "!="]

    if List.contains nextNextToken (arithmeticOps@comparisonOps) then
      let (T2, leftType) = expr_value tokens symbols // decouples tuple into two variables
      let T3 = expr_op T2 symbols // this doesn't return a type, so no decoupling is required
      let (T3, rightType) = expr_value T3 symbols // decouples tuple into two variables
      if List.contains nextNextToken arithmeticOps then // arithmetic operator
        if leftType <> "int" && leftType <> "real" then // rule 2 of the checker (int or real)
          failwith("operator " + nextNextToken + " must involve 'int' or 'real'")
        elif rightType <> "int" && rightType <> "real" then // rule 2 of the checker (int or real)
          failwith("operator " + nextNextToken + " must involve 'int' or 'real'")
        elif leftType <> rightType then // rule 2 of the checker (same type)
          failwith("type mismatch '" + leftType + "' " + nextNextToken + " '" + rightType + "'")
        else
          (T3, leftType)
      else // comparison operator
        if (leftType = "real" && rightType = "real") && nextNextToken = "==" then // rule 6 of the checker
          printfn "warning: comparing real numbers with == may never be true"
          (T3, "bool") // returns, since the warning only pops up when leftType & rightType are both "real"
        else
          if leftType <> rightType then // rule 3 of the checker (operands of the same type)
            failwith("type mismatch '" + leftType + "' " + nextNextToken + " '" + rightType + "'")
          else
            (T3, "bool")
    else // <value> path
      expr_value tokens symbols


  // output-value
  //
  // BNF:
  // <expr-value>
  // | endl
  let rec private output_value tokens symbols =
    let nextToken = List.head tokens

    if nextToken = "endl" then
      (matchToken "endl" tokens, "endl") // passes endl as a type; this will fail every check
    else
      expr_value tokens symbols


  // output
  //
  // BNF:
  // cout << <output-value> ;
  let rec private output tokens symbols =
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let (T4, outputType) = output_value T3 symbols // decouples tuple into two variables
    matchToken ";" T4
  
  
  // assignment
  //
  // BNF:
  // identifier = <expr> ;
  let rec private assignment tokens symbols =
    let nextToken = List.head tokens
    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    let (T4, exprType) = expr T3 symbols // decouples tuple into two variables

    let identifierType = getType (sliceIdentifier nextToken) symbols
    if identifierType <> exprType then
      if identifierType = "real" && exprType = "int" then // rule 4b of the checker
        matchToken ";" T4
      else
        failwith("cannot assign '" + exprType + "' to variable of type '" + identifierType + "'")
    else // rule 4a of the checker
      matchToken ";" T4


  // condition
  //
  // BNF:
  // <expr>
  let rec private condition tokens symbols =
    expr tokens symbols
  

  // stmt
  // 
  // BNF:
  // <empty> | <vardecl> | <input> | <output> | <assignment> | <ifstmt>
  let rec private stmt tokens symbols =
    // to ensure proper matching since the lexer bundles identifiers with their corresponding values
    let nextToken = if beginswith "identifier" (List.head tokens) then "identifier" else List.head tokens
    
    // checks if at the end of tokens; if so, breaks the loop
    if tokens = ["}"; "$"] then
      tokens
    else
      // redirects program to next step
      match nextToken with
      | ";" -> empty tokens symbols
      | "int" -> vardecl tokens symbols
      | "real" -> vardecl tokens symbols
      | "cin" -> input tokens symbols
      | "cout" -> output tokens symbols
      | "identifier" -> assignment tokens symbols
      | "if" -> ifstmt tokens symbols
      | _ -> else_part tokens symbols // added for edge case with nested if-elses


  // then-part
  //
  // BNF:
  // <stmt>
  and private then_part tokens symbols =
    stmt tokens symbols


  // else-part
  //
  // BNF:
  // else <stmt>
  // | EMPTY
  and private else_part tokens symbols =
    let nextToken = List.head tokens

    if nextToken = "else" then
      let T2 = matchToken "else" tokens // match and discard "else"
      stmt T2 symbols // parse with remaining tokens; resets statement counter to ensure that else is not empty
    else
      tokens // to handle the 'EMPTY' case in the else-part's BNF definition


  // ifstmt
  //
  // BNF:
  // if ( <condition> ) <then-part> <else-part>
  and private ifstmt tokens symbols =
    let nextToken = List.head tokens

    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let (T4, conditionType) = condition T3 symbols
    if conditionType <> "bool" then // rule 5 of the checker
      failwith("if condition must be 'bool', but found '" + conditionType + "'")
    else
      let T5 = matchToken ")" T4
      let T6 = then_part T5 symbols
      else_part T6 symbols


  // morestmts
  //
  // BNF:
  // <stmt> <morestmts>
  // | EMPTY
  let rec private morestmts tokens symbols =
    let nextToken = List.head tokens

    // checks if there are no more tokens ("}" essentially means no more arguments)
    if nextToken <> "}" then
      let T2 = stmt tokens symbols
      morestmts T2 symbols
    else
      tokens


  // stmts
  //
  // BNF:
  // <stmt> <morestmts>
  let rec private stmts tokens symbols =
    let T2 = stmt tokens symbols
    // increments so that an early exit doesn't happen
    // (this is mainly for the edge case where main() is empty)
    morestmts T2 symbols
  
  // simpleC
  //
  // starts the program
  // BNF:
  // void main ( ) { <stmts> } $
  let private simpleC tokens symboltable =
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6 symboltable // passes the symboltable for type-checking
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8 // end-of-file; no more tokens
    T9


  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  let typecheck tokens symboltable =
    try
      let T2 = simpleC tokens symboltable
      "success"
    with
      | ex -> "type_error: " + ex.Message

