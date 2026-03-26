public class Tractor extends Vehicle {
    private int numDoors;
    
    public Tractor(String make, String model, int year, int numDoors){
        super(make, model, year);
        this.numDoors = numDoors;
    }

    @Override
    public void start() { System.out.println("Tractor" + getId() + " engine on."); }

    @Override
    public void stop() { System.out.println("Tractor " + getId() + " engine off."); }

    @Override
    public void accelerate(int amount) {
        System.out.println("Tractor " + getId() + " accelerates by " + amount + " mph.");
    }
}