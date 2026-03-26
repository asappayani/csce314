public class Jetski extends Vehicle {
    private int numDoors;
    
    public Jetski(String make, String model, int year, int numDoors){
        super(make, model, year);
        this.numDoors = numDoors;
    }

    @Override
    public void start() { System.out.println("Jetski " + getId() + " engine on."); }

    @Override
    public void stop() { System.out.println("Jetski " + getId() + " engine off."); }

    @Override
    public void accelerate(int amount) {
        System.out.println("Jetski " + getId() + " accelerates by " + amount + " mph.");
    }
}