//******* CH. 2 *******//

// 2.3 Collision Point
template<typename F>
  requires(Transformation(F))
Domain(F) convergent_point(Domain(F) x0, Domain(F) x1, F f)
{
  while (x0 != x1)
    {
      x0 = f(x0);
      x1 = f(x1);
    }
}

template<typename F, typename P>
  requires(Transformation(F) && UnaryPredicate(P) && Domain(F) == Domain(P))
 Domain(F) connection_point(const Domain(F)& x, F f, P p)
{
  //Precondition: p(x) <==> f(x) is defined
  Domain(F) y = collision_point(x, f, p);
  if(!p(y)) return y;
  return convergent_point(x, f(y), f);
}




//******* CH.3 *******//

// SPECIAL CASE: With Integer(I)
template<typename I, typename Op>
	requires(Integer(I) && BinaryOperation(Op))
Domain(Op) power_accumulate_positive(Domain(Op) r, Domain(Op) a, I n, Op op)
{
	//Precondition: associative(op) && positive(n)
	while(true){
		if(odd(n)){
			r = op(r,a);
			if(one(n)) return r;
		}
		a = op(a,a);
		n = half_nonnegative(n);
	}
}

template<typename I, typename Op>
	requires(Integer(I) && BinaryOperation(Op))
Domain(Op) power(Domain(Op) a, I n, Op op, Domain(Op) id)
{
	//Precondition: associative(op) && positive(n)
	while(even(n)){					//SC
		a = op(a,a);				//SC
		n = half_nonnegative(n);	//SC
	}
	n = half_nonnegative(n)			//SC
	if(zero(n)) return a;
	return power_accumulate_positive(a, op(a,a), n, op)
}

template<typename I, typename Op>
	requires(Integer(I) && BinaryOperation(Op))
Domain(Op) power(Domain(Op) a, I n, Op op, Domain(Op) id)
{
	//Precondition: associative(op) && not(negative(n))
	if(zero(n)) return id;
	return power(a, n, op);
}






//******* CH. 4 *******//

template<Relation R>
axiom transitive(R r) {
  forall(Domain(R) a, Domain(R) b, Domain(R) c)
    r(a,b) and r(b,c) => r(a,c);
}

// maximum
template<Relation R>
const Domain(R)& select_0_2(const Domain(R)& a, const Domain(R)& b, R r)
{
  // Precondition: weak_ordering(r)
  if(r(b,a)) return a;
  return b;
}

// minimum: 3 arguements
template<Relation R>
const Domain(R)& select_0_3(const Domain(R)& a, const Domain(R)& b, const Domain(R)& c, R r)
{
  return select_0_2(select_0_2(a,b,r),c,r);
}

// maximum: select_1_2

// Median
// first two elements are ordered
template<Relation R>
const Domain(R)& select_1_3_ab(const Domain(R)& a, const Domain(R)& b, const Domain(R)& c, R r)
{
  if(!r(c,b)) return b;
  return select_1_2(a,c,r);
}

// order two elements
template<Relation R>
const Domain(R)& select_1_3(const Domain(R)& a, const Domain(R)& b, const Domain(R)& c, R r)
{

  if(r(b,a)) return select_1_3_ab(b,a,c,r);
  return select_1_3_ab(a,b,c,r);
}



//******* CH.5 *******//
// associative axiom
// 

template<Transformation F, Regular T, BinaryOperation Op>
axiom inverse_operation(F inv, T e, Op op) 
{
   forall(T a)
     {
       op(a,inv(a)) == op(inv(a),a);
       op(a,inv(a)) == e;
     }
}

template<BinaryOperation Op>
axiom commutative(Op op) {
  forall(Domain(Op) a, Domain(Op) b)
    op(a,b) == op(b,a);
}

