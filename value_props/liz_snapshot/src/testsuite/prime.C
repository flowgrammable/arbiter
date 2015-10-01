int abs(int x){
  if(x < 0) return -x; else return x;
}

//template<typename N>
axiom prime(int n){
  forall(int u, int v)
    (u*v == n) => (abs(u) == 1 or abs(v) == 1);
}
