public class DemoExceptions {
    static int safeDivide(int a, int b) throws DivisionByZeroException {
        if (b == 0) throw new DivisionByZeroException();
        return a / b;
    }

    static int sqrtLike(int x) {
        if (x < 0) throw new NegativeNumberException("No sqrt of negative numbers here!");
        return x;
    }

    public static void main(String[] args) {
        try {
            System.out.println("10 / 2 = " + safeDivide(10, 2));
            System.out.println("10 / 0 = " + safeDivide(10, 0));
        } catch (DivisionByZeroException e) {
            System.out.println("Caught: " + e.getMessage());
        }

        try {
            System.out.println("sqrtLike(9) = " + sqrtLike(9));
            System.out.println("sqrtLike(-1) = " + sqrtLike(-1));
        } catch (NegativeNumberException e) {
            System.out.println("Caught runtime: " + e.getMessage());
        }
    }
}
