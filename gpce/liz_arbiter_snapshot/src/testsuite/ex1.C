concept Integer(Regular I) {
    I successor(I n) {
        n = n+1;
    }
}


concept Iterator(Regular T) {
    Integer DistanceType(Iterator);
    T successor(T);
}
