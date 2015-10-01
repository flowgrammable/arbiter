concept UnaryFunction(Function F) {
    Arity(F) == 1;
}

concept HomogenousFunction(Function F) {
   Arity(F) > 0;
   forall(int i, int j)
      i< Arity(F) and j<Arity(F) => InputType(F, i) == InputType(F, j);
}

Regular Domain(HomogenousFunction T) {
    return InputType(T, 0);
}

template<UnaryFunction F>
axiom regular_unary_function(F f1) {
   forall(F f2) forall(Domain(F) x1, Domain(F) x2)
      (f1 == f2 and x1 == x2) => (f1(x1) == f2(x2));
}


