public class Car extends Vehicle {
    private int numDoors;

    public Car(String make, String model, int year, int numDoors) {
        super(make, model, year);
        this.numDoors = numDoors;
    }

    @Override
    public void start() { System.out.println("Car " + getId() + " engine on."); }

    @Override
    public void stop() { System.out.println("Car " + getId() + " engine off."); }

    @Override
    public void accelerate(int amount) {
        System.out.println("Car " + getId() + " accelerates by " + amount + " mph.");
    }
}