// Previous Concepts
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

concept TotallyOrdered(Regular T){
  bool operator<(T,T);
  //total_ordering(<); //pg. 51
}


// 5.1 Basic Algebraic Structures

concept AdditiveSemigroup(Regular T)
{
  T operator+(T, T);
  // associative(+) pg. 31
  // commutative(+) pg. 66
}

concept AdditiveMonoid(AdditiveSemigroup T)
{
  T operator{}(); //0 element of T
  //identity_element(0,+);
}

concept AdditiveGroup(AdditiveMonoid T)
{
  T operator-(T);
  //inverse_operation(unary -, 0, +);
  T operator-(T a, T b)
  {
    a + (-b);
  }
}


// 5.2 Ordered Algebraic Structures

concept OrderedAdditiveSemigroup(AdditiveSemigroup T)
{
  TotallyOrdered(T);
  forall(T a, T b, T c) a < b =>
   a + c < b + c;
}

concept OrderedAdditiveMonoid(OrderedAdditiveSemigroup T)
{
  AdditiveMonoid(T);
}

concept OrderedAdditiveGroup(OrderedAdditiveMonoid T)
{
  AdditiveGroup(T);
}

//template<OrderedAdditiveGroup T>
//T abs(T a)
//{
//if(a < 0) return -a;
//else return a;
//}


// 5.3 Remainder

concept CancellableMonoid(OrderedAdditiveMonoid T)
{
  T operator-(T, T);
  //forall(T a, T b) b <= a => a - b + b == a;
}

//template<CancellableMonoid T>
//T slow_remainder(T a, T b)
//{
// Precondition: a >= 0 and b > 0
//while(b <= a) a = a - b;
//return a;
//}

concept ArchimedeanMonoid(CancellableMonoid T)
{
  //forall(T a, T b)
//(a >= 0 and b > 0) => slow_remainder(a,b) terminates;
  int QuotientType(ArchimedeanMonoid);
}

//mplate<ArchimedeanMonoid T>
//QuotientType(T) slow_quotient(T a, T b)
//{
// Precondition: a >= 0 and b > 0
//QuotientType(T) n = 0;
//while( b<= a)
//  {
//    a = a - b;
//    n = n + 1;
//  }
//return n;
//}

//template<ArchimedeanMonoid T>
//T largest_doubling(T a, T b)
//{
////Precondition a >= 0 and b > 0
//while(b <= a - b) b = b + b;
//return b;
//}

//concept HalvableMonoid(ArchimedeanMonoid T){
//T half(T);
//forall(T a, T b)
//(b > 0 and a = b + b) => half(a) = b;
//}

//template<HalvableMonoid T>
//T remainder_nonnegative_iterative(T a, T b)
//{
//// Precondition: a >= 0 and b > 0
//if(a < b) return a;
//T c = largest_doubling(a,b);
//a = a - c;
//while(c != b)
//{
//c = half(c);
//if(c <= a) a = a - c;
//}
//return a;
//}


// 5.4 GCD

//template<ArchimedeanMonoid T>
//T subtractive_gcd_nonzero(T a, T b)
//{
//// Precondition: a > 0 and b > 0
//while(true)
//{
//if(b < a) a = a - b;
//else if(a < b) b = b - a;
//else return a;
//}
//}
