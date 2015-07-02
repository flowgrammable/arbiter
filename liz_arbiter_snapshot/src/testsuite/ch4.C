// Ch.3
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

// Ch.2
concept Predicate(Function P) {
 Codomain(P) == bool;
}

// 4.1 Classification of Relations
concept Relation(Predicate Op){
  HomogeneousFunction(Op);
  Arity(Op) == 2;
}

// 4.3 Order Selection

// Minimum(2)
//template<typename R>
//requires(Relation(R))
template<Relation R>
Domain(R) select_0_2(Domain(R) a, Domain(R) b, R r)
{
  // Precondition: weak_ordering(r)
  if(r(b,a)) return b;
  return a;
}

// Maximum(2)
template<Relation R>
const Domain(R)& select_1_2(const Domain(R)& a, const Domain(R)& b, R r)
{
  // Precondition: weak_ordering(r)
  if(r(b,a)) return a;

  return b;
}

// Minimum(3)
template<Relation R>
Domain(R) select_0_3(Domain(R) a, Domain(R) b, Domain(R) c, R r)
{
  Domain(R) x = select_0_2(a,b,r);
  return select_0_2(x, c, r);
  //return select_0_2(select_0_2(a,b,r), c, r);
}

// Median with first two elements are ordered
//template<Relation R>
//Domain(R) select_1_3_ab(Domain(R) a, Domain(R) b, Domain(R) c, R r)
//{
//if(!r(c,b)) return b;
//return select_1_2(a,c,r);
//}

// Order first two elements to find the median
//template<Relation R>
//Domain(R) select_1_3(Domain(R) a, Domain(R) b, Domain(R) c, R r)
//{

//if(r(b,a)) return select_1_3_ab(b,a,c,r);
//return select_1_3_ab(a,b,c,r);
//}


bool less_than(int x, int y)
{
  if(x < y) return true;
  else return false;
}

bool less_char(char x, char y)
{
  if(x < y) return true;
  else return false;
}

select_0_2(2, 3, less_than); // min
select_0_2(3, 2, less_than); // min
select_0_2('a', 'b', less_char); // min
//select_1_2(2, 3, less_than); // max
//select_0_3(3, 2, 4, less_than); // min
//select_1_3(4, 2, 3, less_than); // median

//template<bool strict, Relation R>
//struct compare_strict_or_reflexive<true, R>
//{
//bool operator()(const Domain(R)& a, const Domain(R)& b, R r)
//  return r(a,b);
//}
