public interface Operable {
    void start();
    void stop();
    void accelerate(int amount); // amount could be knots for boats, mph for cars, etc.
}
