concept BifurcateCoordinate(Regular T) {
  int WeightType(BifurcateCoordinate);
  bool empty(T);
  bool has_left_successor(T);
  bool has_right_successor(T);
  T left_successor(T);
  T right_successor(T);
  forall(T i, T j) left_successor(i) == j or right_successor(i) == j =>
  !empty(j);
}


concept BidirectionalBifurcateCoordinate(BifurcateCoordinate T)
{
  bool has_predecessor(T);
  //forall(T i) !empty(i) => has_predecessor(i) is defined
  T predecessor(T);
  //forall(T i) has_left_successor(i) => predecessor(left_successor(i)) is defined and equals i
  //forall(T i) has_right_successor(i) => predecessor(right_successor(i)) is defined and equals i
  //forall(T i) has_predecessor(i) => is_left_successor(i) or is_right_successor(i);
}

template<BidirectionalBifurcateCoordinate T>
bool is_left_successor(T j)
{
  // Precondition: has_predecessor(j)
  T i = predecessor(j);
  return has_left_successor(i) && left_successor(i) == j;
}

template<BidirectionalBifurcateCoordinate T>
bool is_left_successor(T j)
{
  // Precondition: has_predecessor(j)
  T i = predecessor(j);
  return has_right_successor(i) && right_successor(i) == j;
}

//template<BifurcateCoordinate C>
//WeightType(C) weight_recursive(C c)
//{
  // Precondition: tree(c)
  //if(empty(c)) return 0;
//}
