The main synthesis procedure is called `saturation-synthesis` and lives in `saturation-synthesis.rkt`.

Here we are attempting to synthesize a term rewriting system to replace a component within the Halide compiler called the variable solver, which attempts to 'solve' an expression for a specified variable (the target variable) by isolating it to the left as much as possible. 

This method takes a set of input expressions, an initial term rewriting system (can be empty), an initial blacklist (can be empty), and a procedure that can take a candidate LHS expression and attempt to synthesize a RHS that would form a valid rule.

For each expression in the set of input expressions, `saturation-synthesis` will call `synthesis-iteration`, a method that attempts to learn rules that could further rewrite that expression. We first normalize the expression with the existing TRS. We traverse the subtrees of the input expression, bottom up. For each subtree, we find all candidate LHSs that can match that subtree. Each candidate LHS is passed to the `synth-func` method, which attempts to synthesize a valid rule. If a rule is found, it is added to the TRS; we then recurse, normalizing the expression with the enhanced TRS and continuing our traversal. (If we are unable to synthesize a rule using a candidate LHS, we add that LHS to the blacklist so we do not attempt to synthesize it again.) Once we have traversed the entire expression and are unable to find any rule that could further rewrite it, we return, passing the enhanced TRS and blacklist back to `saturation-synthesis`, which continues on to the next input expression. 

We can use different strategies for synthesizing individual rules by passing different `synth-func` methods to `saturation-synthesis`. These methods encode a TRS specification, conceived of at a high level as a reduction order, or an order over terms, such that every rule in the desired TRS will rewrite an input to be strictly less in that order than its original form. These methods don't directly express that order; instead, they encode a series of strategies for breaking up the search space for finding valid rules w.r.t that order, providing more efficient synthesis. Two methods are implemented here:
 - `synthesize-rule`, which attempts to find RHS expressions that 
  -- don't contain the target variable at all, 
  -- consist *only* of the target variable, or 
  -- are in the form `t * e` where `t` is the target variable, `*` is some operation, and `e` is an expression that does not contain the target variable.
 - `synthesize-gradual-rule`, which searches for the above types of RHSs, but then also attempts to find 
  -- a RHS with fewer instances of the target variable than the LHS, 
  -- a RHS in which the target variable has moved left w.r.t its position in the LHS, and 
  -- a RHS in which the target variable has moved up in the AST w.r.t its position in the LHS.

The synthesis calls themselves are implemented in `varsolver-synthesis.rkt` and `typed-fixed-synthesis-sketch.rkt`. The interpreter that encodes the semantics of the Halide expression language is implemented in `typed-halide-lang.rkt`.