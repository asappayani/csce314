import java.util.Objects;

public final class MyMath {

    private MyMath() { }

    public static <T extends Number> double add(T a, T b) {
        requireNotNull(a, b);
        return a.doubleValue() + b.doubleValue();
    }

    public static <T extends Number> double subtract(T a, T b) {
        requireNotNull(a, b);
        return a.doubleValue() - b.doubleValue();
    }

    public static <T extends Number> double multiply(T a, T b) {
        requireNotNull(a, b);
        return a.doubleValue() * b.doubleValue();
    }

    public static <T extends Number> double divide(T a, T b) throws DivisionByZeroException {
        requireNotNull(a, b);
        double denom = b.doubleValue();
        if (Math.abs(denom) < 1e-15) throw new DivisionByZeroException();
        return a.doubleValue() / denom;
    }

    private static void requireNotNull(Object... objs) {
        for (Object o : objs) Objects.requireNonNull(o, "Null numeric argument.");
    }

    public static void main(String[] args) {
        int passed = 0, total = 0;

        java.util.function.BiConsumer<Boolean, String> check = (ok, name) -> {
            if (ok) System.out.println("[PASS] " + name);
            else System.out.println("[FAIL] " + name);
        };

        total++; { boolean ok = Math.abs(MyMath.add(2, 3) - 5.0) < 1e-9; check.accept(ok, "add(int,int)"); if (ok) passed++; }
        total++; { boolean ok = Math.abs(MyMath.add(2.5, 0.5) - 3.0) < 1e-9; check.accept(ok, "add(double,double)"); if (ok) passed++; }
        total++; { boolean ok = Math.abs(MyMath.subtract(10L, 3L) - 7.0) < 1e-9; check.accept(ok, "subtract(long,long)"); if (ok) passed++; }
        total++; { boolean ok = Math.abs(MyMath.multiply(1.5f, 2.0f) - 3.0) < 1e-9; check.accept(ok, "multiply(float,float)"); if (ok) passed++; }

        try {
            total++;
            double r = MyMath.divide(9, 3);
            boolean ok = Math.abs(r - 3.0) < 1e-9;
            check.accept(ok, "divide(int,int)");
            if (ok) passed++;
        } catch (Exception e) {
            check.accept(false, "divide(int,int) threw " + e);
        }

        try {
            total++;
            MyMath.divide(1, 0);
            check.accept(false, "divide by zero should throw");
        } catch (DivisionByZeroException ex) {
            check.accept(true, "divide by zero throws DivisionByZeroException");
            passed++;
        } catch (Exception ex) {
            check.accept(false, "wrong exception type: " + ex);
        }

        System.out.printf("Unit tests: %d/%d passed%n", passed, total);
    }
}
