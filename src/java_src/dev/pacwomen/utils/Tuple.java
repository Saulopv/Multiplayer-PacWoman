package dev.pacwomen.utils;


public class Tuple<X, Y> {
  private final X x;
  private final Y y;

  public Tuple(X x, Y y) {
    this.x = x;
    this.y = y;
  }

  @Override
  public boolean equals(Object obj) {
    if(this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;

    Tuple<?, ?> tuple = (Tuple<?, ?>) obj;
    if (!x.equals(tuple.x)) return false;
    return y.equals(tuple.y);
  }

  @Override
  public int hashCode() {
    int result = x.hashCode();
    result = 31 * result + y.hashCode();
    return result;
  }

  public X getX() {
    return this.x;
  }

  public Y getY() {
    return this.y;
  }

  @Override
  public String toString() {
    return "{"+x+","+y+"}";
  }
}
