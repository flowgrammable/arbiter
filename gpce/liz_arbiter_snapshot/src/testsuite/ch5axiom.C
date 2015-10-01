// ch.3 Concepts
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

concept UnaryFunction(Function F) {
   Arity(F) == 1;
}

concept Transformation(Operation F) {
  UnaryFunction(F);
  int DistanceType(Transformation);
}

template<BinaryOperation Op> requires(Regular(Domain(Op)))
axiom identity_element(Domain(Op) e, Op op) {
   forall(Domain(Op) a)
     {
       op(a,e) == op(e,a);
       op(a,e) == a;
     }
}

template<Transformation F, BinaryOperation Op> 
requires(Domain(F) == Domain(Op) and Regular(Domain(F)))
axiom inverse_operation(F inv, Domain(Op) e, Op op) {
forall(Domain(Op) a)
   {
     op(a,inv(a)) == op(inv(a),a);
     op(a,inv(a)) == e;
   }
}

template<BinaryOperation Op> requires(Regular(Domain(Op)))
axiom commutative(Op op) {
  forall(Domain(Op) a, Domain(Op) b) op(a,b) == op(b,a);
}


