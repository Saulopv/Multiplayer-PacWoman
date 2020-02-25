package dev.pacwomen.utils;


public class Tripple<X, Y, Z> {
    private final X x;
    private final Y y;
    private final Z z;

    public Tripple(X x, Y y, Z z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    @Override
    public boolean equals(Object obj) {
        if(this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;

        Tripple<?, ?, ?> tripple = (Tripple<?, ?, ?>) obj;
        if ((!x.equals(tripple.x)) && (!z.equals(tripple.z))) return false;
        return y.equals(tripple.y);
    }

    @Override
    public int hashCode() {
        int result = x.hashCode();
        result = 31 * result + y.hashCode() + z.hashCode();
        return result;
    }

    public X getX() {
        return this.x;
    }

    public Y getY() {
        return this.y;
    }
    
    public Z getZ() {
        return this.z;
    }

    @Override
    public String toString() {
        return "{"+x+","+y+","+z+"}";
    }
}
