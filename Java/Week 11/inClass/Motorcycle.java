public class Motorcycle extends Vehicle {
    private int numDoors;
    
    public Motorcycle(String make, String model, int year, int numDoors){
        super(make, model, year);
        this.numDoors = numDoors;
    }

    @Override
    public void start() { System.out.println("Motorcycle " + getId() + " engine on."); }

    @Override
    public void stop() { System.out.println("Motorcycle " + getId() + " engine off."); }

    @Override
    public void accelerate(int amount) {
        System.out.println("Motorcycle " + getId() + " accelerates by " + amount + " mph.");
    }
}