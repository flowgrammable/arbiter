// 3.1 Associative Operations 
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

// 3.2 Computing Powers 

template<BinaryOperation Op>
Domain(Op) power_0(Domain(Op) a, int n, Op op)
{
  //Precondition: associative(op) && n > 0
  if(n == 1) return a;
  if(n % 2 == 0)
    {
      return power_0(op(a,a), n / 2, op);
    }
  return op(power_0(op(a,a), n / 2, op), a);
}

template<BinaryOperation Op>
//template<typename Op>
//requires(BinaryOperation(Op))
Domain(Op) power_accumulate_positive_0(Domain(Op) r, Domain(Op) a, int n, Op op)
{
  //Precondition: associative(op) && n > 0
  while(true) 
    {
      if(n % 2 != 0)
	{
	  r = op(r,a);
	  if(n == 1) return r;
	}
      a = op(a,a);
      n = n / 2;
    }
}

int mult(int x, int y) {
  return x * y;
}

int add(int x, int y){
  return x + y;
}

power_0(4, 3, mult); // 4^3 = 64

power_accumulate_positive_0(2,4,3,mult); // 2*4^3 = 128

power_accumulate_positive_0(1,4,3,add); // 4*3 = 12

//pair<int, int> fibonacci_matrix_multiply(const pair<int, int>& x, const pair<int, int>& y)
//{
//return pair<int, int>(x.m0 * (y.m1 + y.m0) + x.m1 * y.m0, x.m0 * y.m0 + x.m1 * y.m1);
//}

//int fibonacci(int n)
//{
  //Precondition: n >= 0
  //if(n == 0) return 0;
  //return power_0(pair<int, int>(1, 0), n, fibonacci_matrix_multiply<int>).m0;
//}

