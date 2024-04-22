//
// Parser for SimpleC programs.  This component checks
// the input program to see if it meets the syntax rules
// of SimpleC.  The parser returns a string denoting
// success or failure.
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// Logan Lucas
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =

  // beginswith
  //
  // a wrapper function for StartsWith that ensures that the
  // literal parameter is a string
  let beginswith (pattern : string) (literal : string) =
    literal.StartsWith (pattern)


  // matchToken
  // 
  // when called, checks if the next token matches the expected token
  // an exception is thrown if no match is found; otherwise, 
  // returns the rest of the tokens
  let private matchToken expected_token tokens =
    let next_token = List.head tokens

    // the following if statements check if the next token is
    // an identifier, str_literal, or int_literal since the
    // value is bundled with the token in the lexer
    if beginswith "identifier" next_token && expected_token = "identifier" then
      List.tail tokens
    else
      if beginswith "int_literal" next_token && expected_token = "int_literal" then
        List.tail tokens
      else
        if beginswith "str_literal" next_token && expected_token = "str_literal" then
          List.tail tokens
        else // below is the original starter code for matchToken
          if expected_token = next_token then
            List.tail tokens
          else
            failwith ("expecting " + expected_token + ", but found " + next_token)


  // empty
  //
  // BNF:
  // ;
  let rec private empty tokens hasStmt = 
    matchToken ";" tokens
  

  // vardecl
  //
  // BNF:
  // int identifier;
  let rec private vardecl tokens hasStmt =
    let T2 = matchToken "int" tokens
    let T3 = matchToken "identifier" T2
    matchToken ";" T3
  

  // input
  //
  // BNF:
  // cin >> identifier;
  let rec private input tokens hasStmt =
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    matchToken "identifier" T3
  

  // expr-value
  // 
  // BNF:
  // identifier | int_literal | str_literal | true | false
  let rec private expr_value tokens hasStmt =
    let nextToken = List.head tokens

    if nextToken = "true" || nextToken = "false" then
      matchToken nextToken tokens
    else
      if beginswith "identifier" nextToken || beginswith "int_literal" nextToken || beginswith "str_literal" nextToken then
        matchToken nextToken tokens // removes the need for multiple checks since matchToken only checks if the two params are equal
      else
        failwith ("expecting identifier or literal, but found " + nextToken)


  // expr_op
  // 
  // BNF:
  // + | - | * | / | ^ | < | <= | > | >= | == | !=
  let rec private expr_op tokens hasStmt =
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
  let rec private expr tokens hasStmt =
    let nextToken = List.head tokens
    let nextNextToken = List.head (List.tail tokens) // token after nextToken

    // all valid operators; for checking with the nextNextToken
    let operators = ["+"; "-"; "*"; "/"; "^"; "<"; "<="; ">"; ">="; "=="; "!="]

    // branches down the <value> -> <op> -> <value> path
    if List.contains nextNextToken operators then
      let T2 = expr_value tokens hasStmt
      let T3 = expr_op T2 hasStmt
      expr_value T3 hasStmt
    else // <value> path
      expr_value tokens hasStmt


  // output-value
  //
  // BNF:
  // <expr-value>
  // | endl
  let rec private output_value tokens hasStmt =
    let nextToken = List.head tokens

    if nextToken = "endl" then
      matchToken "endl" tokens
    else
      expr_value tokens hasStmt


  // output
  //
  // BNF:
  // cout << <output-value> ;
  let rec private output tokens hasStmt =
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3 hasStmt
    matchToken ";" T4
  
  
  // assignment
  //
  // BNF:
  // identifier = <expr> ;
  let rec private assignment tokens hasStmt =
    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    let T4 = expr T3 hasStmt
    matchToken ";" T4


  // condition
  //
  // BNF:
  // <expr>
  let rec private condition tokens hasStmt =
    expr tokens hasStmt
  

  // stmt
  // 
  // BNF:
  // <empty> | <vardecl> | <input> | <output> | <assignment> | <ifstmt>
  let rec private stmt tokens hasStmt =
    // to ensure proper matching since the lexer bundles identifiers with their corresponding values
    let nextToken = if beginswith "identifier" (List.head tokens) then "identifier" else List.head tokens
    
    // checks if at the end of tokens; if so, breaks the loop
    // hasStmt ensures that there is AT LEAST 1 statement in main()
    if tokens = ["}"; "$"] && hasStmt > 0 then
      tokens
    else
      // redirects program to next step
      match nextToken with
      | ";" -> empty tokens hasStmt
      | "int" -> vardecl tokens hasStmt
      | "cin" -> input tokens hasStmt
      | "cout" -> output tokens hasStmt
      | "identifier" -> assignment tokens hasStmt 
      | "if" -> ifstmt tokens hasStmt
      | "else" -> else_part tokens hasStmt // added for edge case with nested if-elses
      | _ -> failwith ("expecting statement, but found " + nextToken)


  // then-part
  //
  // BNF:
  // <stmt>
  and private then_part tokens hasStmt =
    stmt tokens hasStmt


  // else-part
  //
  // BNF:
  // else <stmt>
  // | EMPTY
  and private else_part tokens hasStmt =
    let nextToken = List.head tokens

    if nextToken = "else" then
      let T2 = matchToken "else" tokens // match and discard "else"
      stmt T2 0 // parse with remaining tokens; resets statement counter to ensure that else is not empty
    else
      tokens // to handle the 'EMPTY' case in the else-part's BNF definition


  // ifstmt
  //
  // BNF:
  // if ( <condition> ) <then-part> <else-part>
  and private ifstmt tokens hasStmt =
    let nextToken = List.head tokens

    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3 hasStmt
    let T5 = matchToken ")" T4
    let T6 = then_part T5 hasStmt
    else_part T6 hasStmt


  // morestmts
  //
  // BNF:
  // <stmt> <morestmts>
  // | EMPTY
  let rec private morestmts tokens hasStmt =
    let nextToken = List.head tokens

    // checks if there are no more tokens ("}" essentially means no more arguments)
    if nextToken <> "}" then
      let T2 = stmt tokens hasStmt
      morestmts T2 hasStmt
    else
      tokens


  // stmts
  //
  // BNF:
  // <stmt> <morestmts>
  let rec private stmts tokens hasStmt =
    let T2 = stmt tokens hasStmt
    // increments so that an early exit doesn't happen
    // (this is mainly for the edge case where main() is empty)
    morestmts T2 1


  // simpleC
  //
  // BNF:
  // void main ( ) { <stmts> } $
  let private simpleC tokens =
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6 0
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8 // end-of-file; no more tokens
    T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid SimpleC program.  Returns
  // the string "success" if valid, otherwise returns a
  // string of the form "syntax_error:...".
  //
  let parse tokens =
    try
      let result = simpleC tokens
      "Success!"
    with
      | ex -> "syntax_error: " + ex.Message
