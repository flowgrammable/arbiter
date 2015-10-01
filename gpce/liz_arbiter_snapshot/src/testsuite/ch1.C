// 1.7 Concepts
concept UnaryFunction(Function F) {
   Arity(F) == 1;
}

concept HomogeneousFunction(Function F) {
   Arity(F) > 0;

   forall(int i, int j) i < Arity(F) and j < Arity(F) =>
      InputType(F, i) == InputType(F, j);
}

typename Domain(HomogeneousFunction F) {
      return InputType(F, 0);
}

concept Operation(HomogeneousFunction Op) {
  Codomain(Op) == Domain(Op);
}

concept BinaryOperation(Operation Op) {
  Arity(Op) == 2;
}

template<BinaryOperation Op>
//template<typename Op>
//requires(BinaryOperation(Op))
Domain(Op) square(const Domain(Op)& x, Op op) {
  return op(x, x);
}

int mult(int x, int y)
{
  return x*y;
}

square(4, mult);
