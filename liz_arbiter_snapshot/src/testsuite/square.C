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
Domain(Op) square(const Domain(Op)& x, Op op) {
   return op(x, x);
}
