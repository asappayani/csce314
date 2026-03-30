public class Motorcycle extends Vehicle {
    private int cc;
    
    public Motorcycle(String make, String model, int year, int cc){
        super(make, model, year);
        this.cc = cc;
    }

    @Override
    public void start() { System.out.println("Motorcycle " + getId() + " engine on."); }

    @Override
    public void stop() { System.out.println("Motorcycle " + getId() + " engine off."); }

    @Override
    public void accelerate(int amount) {
        System.out.println("Motorcycle " + getId() + " accelerates by " + amount + " mph.");
    }

    @Override
    public String toString() {
        return String.format("%s, CC=%d", super.toString(), cc);
    }
}