public class Truck extends Vehicle {
    private int numDoors;
    
    public Truck(String make, String model, int year, int numDoors){
        super(make, model, year);
        this.numDoors = numDoors;
    }

    @Override
    public void start() { System.out.println("Truck " + getId() + " engine on."); }

    @Override
    public void stop() { System.out.println("Truck " + getId() + " engine off."); }

    @Override
    public void accelerate(int amount) {
        System.out.println("Truck " + getId() + " accelerates by " + amount + " mph.");
    }

    @Override
    public String toString() {
        return String.format("%s, doors=%d", super.toString(), numDoors);
    }
}