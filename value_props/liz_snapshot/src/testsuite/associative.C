concept HomogenousFunction(Function F) {
   Arity(F) > 0;

   forall(int i, int j) i < Arity(F) and j < Arity(F) =>
      InputType(F, i) == InputType(F, j);
}

typename Domain(HomogenousFunction F) {
   return InputType(F, 0);
}

concept Operation(HomogenousFunction Op) {
   Codomain(Op) == Domain(Op);
}

concept BinaryOperation(Operation Op) {
   Arity(Op) == 2;
}

template<BinaryOperation Op>
axiom associative(Op op) {
   forall(Domain(Op) a, Domain(Op) b, Domain(Op) c)
      op(op(a, b), c) == op(a, op(b, c));
}
