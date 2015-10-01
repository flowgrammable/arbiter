// Previously Defined Concepts

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

concept MyTransformation(Operation F) {
  InputType(F, 0) == int;
  //Domain(F) == Codomain(F);
  Codomain(F) == int;
  UnaryFunction(F);
}

concept Predicate(Function P) {
 Codomain(P) == bool;
}

concept UnaryPredicate(Predicate P) {
  UnaryFunction(P);
}

concept MyRelation(Predicate Op){
  InputType(Op, 0) == int;
  Arity(Op) == 2;
  HomogeneousFunction(Op);
}

// 6.2 Iterators

concept Readable(Regular T)
{
  Regular ValueType(Readable);
  ValueType(T) source(T);
}

concept Iterator(Regular T)
{
  int DistanceType(Iterator);
  T successor(T n);  // successor is not necessairly a regular
}

int successor(int x) {return x+1;}
int predecessor(int x){ return x-1;}
int source(int x){ return x;}

//template<Iterator I>
void increment(int x)
{
  x = successor(x);
}

// Ch. 6.3 Ranges
//template<Iterator I>
//I operator+(I f, int n)
//{
  // Precondition: n >= 0 and weak_range(f,n)
  //while (n != 0)
  //{
//n = predecessor(n);
// f = successor(f);
//}
//return f;
//}

template<Iterator I>
int operator-(I f, I l)
{
  // Precondition: n >= 0 and weak_range(f,n)
  int n = 0;
  while (f != l)
  {
    n = n + 1;
    f = successor(f);
  }
  return n;
}

// 6.4 Readable Ranges
//template<Iterator I, Transformation Proc>
//Proc for_each(I f, I l, Proc proc)
//template<MyTransformation Proc>
//int for_each(int f, int l, Proc proc)
//{
  // Precondition: readable_bounded_range(f,l)
  //int x;
  //while(f != l)
  //{
//x = proc(f);
//f = successor(f);
//}
//return x;
//}

//int plus10(int x){ return x + 10;}
//for_each(4, 8, plus10); // 8 + 10 = 18

//template<Iterator I>
//I find(I f, I l, int x)
int find(int f, int l, int x)
{
  // Precondition: readable_bounded_range(f,L)
  while(f != l && source(f) != x) 
    f = successor(f);
return f;
}

find(4,10,8);  // found
find(4,10,12);  // not found

//template<Iterator I, BinaryOperation Op, UnaryFunction F>
//Domain(Op) reduce_nonempty(I f, I l, Op op, F func)
//template<BinaryOperation Op, UnaryFunction F>
//Domain(Op) reduce_nonempty(int f, int l, Op op)
//{
//Domain(Op) r = source(f);
//f = successor(f);
//while(f != l)
// {
    //r = op(r, source(f));
    //f = successor(f);
    // }
//return r;
//}



//template<Iterator I, Relation R>
//I find_adjacent_mismatch(I f, I l, R r)
template<MyRelation R>
int find_adjacent_mismatch(int f, int l, R r)
{
  // Precondition: readable_bounded_range(f,l)
  if(f == l) return l;
  int x = source(f);
  f = successor(f);
  while(f != l && r(x, f))
  {
    x = source(f);
    f = successor(f);
  }
  return f;
}


bool less_than(int x, int y)
{
  if(x < y) return true;
  else return false;
}

bool greater_than(int x, int y)
{
  if(x > y) return true;
  else return false;
}

find_adjacent_mismatch(4, 10, less_than); // 10
find_adjacent_mismatch(4, 10, greater_than); // 5

//6.6  Forward Iterators
//template<Iterator I, UnaryPredicate P>
//I partition_point_n(I f, int n, P p)
//{
  // Precondition: readable_counted_range(f,n) & partition_n(f,n,p)
  //while(n != 0)
//{
//    int h = n/2;
//    I m = f + h;
//    if(p(source(m)))
//	{
//	  n = h;
//	}
//    else
//	{
//	  n = n - successor(h);
//	  f = successor(m);
//	}
//  }
//return f;
//}

//template<UnaryPredicate P>
//int partition_point_n(int f, int n, P p)
//{
  // Precondition: readable_counted_range(f,n) & partition_n(f,n,p)
  //while(n != 0)
  //{
//int h = n/2;
//int m = f + h;
//if(p(source(m)))
//n = h;
//else
//{
      //n = n - successor(h);
      //f = successor(m);
//}
//}
//return f;
//}
