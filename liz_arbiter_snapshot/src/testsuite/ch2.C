// Defined in Ch.1
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

// 2.1 Transformations
concept Predicate(Function P) {
 Codomain(P) == bool;
}

concept HomogeneousPredicate(Predicate P) {
  HomogeneousFunction(P);
}

concept UnaryPredicate(Predicate P) {
  UnaryFunction(P);
}
 
concept Operation(HomogeneousFunction Op) {
  Codomain(Op) == Domain(Op);
}

concept Transformation(Operation F) {
  UnaryFunction(F);
  int DistanceType(Transformation);
}

template<Transformation F>
Domain(F) power_unary(Domain(F) x, int n, F f)
{
  while (n != 0) {
    n = n - 1;
    x = f(x);
  }
 return x;
}

int plus10(int x){ return x + 10;}

//power_unary(4,3,plus10); //4+3*10

// 2.2 Orbits
//template<Transformation F>
//DistanceType(F) distance(Domain(F) x, Domain(F) y, F f)
//{
// typedef DistanceType(F) N;
// N n(0);
// while (x != y) {
//    x = f(x);
//    n = n + N(1);
// }
// return n;
//}

// 2.3 Collision Point
//template<typename F, typename P>
//  requires(Transformation(F) &&  UnaryPredicate(P)
//           && Domain(F) == Domain(P))
template<Transformation F, UnaryPredicate P>
Domain(F) collision_point(const Domain(F)& x, F f, P p)
{

  // Precondition: p(x) <==> f(x) is defined
  if (! p(x)) return x;
  Domain(F) slow = x;
  Domain(F) fast = f(x);
  while (fast != slow)
    {
      slow = f(slow);
      if (!p(fast)) return fast;
      fast = f(fast);
      if (!p(fast)) return fast;
      fast = f(fast);
    }
  return fast;
   // Postcondition: return value is terminal point or collision point
}








