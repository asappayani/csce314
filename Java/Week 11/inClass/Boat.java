public class Boat extends Vehicle {
    private int numEngines;
    
    public Boat(String make, String model, int year, int numEngines){
        super(make, model, year);
        this.numEngines = numEngines;
    }

    @Override
    public void start() { System.out.println("Boat " + getId() + " engine on."); }

    @Override
    public void stop() { System.out.println("Boat " + getId() + " engine off."); }

    @Override
    public void accelerate(int amount) {
        System.out.println("Boat " + getId() + " accelerates by " + amount + " knots.");
    }

    @Override
    public String toString() {
        return String.format("%s, numEngines=%d", super.toString(), numEngines);
    }
}