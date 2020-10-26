# Lab 3

### Shift-reduce conflict: 

1. ```
   lvalue: ID .
   	  | ID . LBRACK exp RBRACK
   ```
   
This shift-reduce conflict is not harmful because its default action is shift:
   
   ```
   LPAREN  shift, and go to state 15
   LBRACK  shift, and go to state 16
   LBRACE  shift, and go to state 17
   
   LBRACK    [reduce using rule 62 (lvalue)]
   $default  reduce using rule 62 (lvalue)
   ```
   
   which is what we want because we want ID or ID LBRACK exp RBRACK to be reduced into lvalue.
   
   
   
2. ```
   tydecs: tydec .
         | tydec . tydecs
   ```

   This shift-reduce conflict is also not harmful because its default action is shift:

   ```
   TYPE  shift, and go to state 27
   
   TYPE      [reduce using rule 49 (tydecs)]
   $default  reduce using rule 49 (tydecs)
   ```

   which is what we want because we want tydec or tydec tydecs to be reduced into tydecs.

   

3. ```
   fundecs: fundec .
          | fundec . fundecs
   ```

   This shift-reduce is also not harmful because its default action is shift:

   ```
   FUNCTION  shift, and go to state 25
   
   FUNCTION  [reduce using rule 55 (fundecs)]
   $default  reduce using rule 55 (fundecs)
   ```

   which is what we want because we want fundec or fundec fundecs to be reduced into fundecs.

