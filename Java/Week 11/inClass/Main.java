import java.util.*;
public class Main {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        List<Vehicle> garage = new ArrayList<>();
        boolean running = true;
        while (running) {
            System.out.println("""
            1) Add vehicle
            2) List all
            3) Operate by ID
            4) Search by type
            5) Summary by type
            0) Quit""");

            System.out.print("Choice: ");
            String choice = sc.nextLine().trim();

            switch (choice) {
                case "1" -> addVehicle(sc, garage);
                case "2" -> listAll(garage);
                case "3" -> operate(sc, garage);
                case "4" -> searchByType(sc, garage);
                case "5" -> summarize(garage);
                case "0" -> running = false;
                default -> System.out.println("Invalid choice.");
            }
        }
    }

    static void addVehicle(Scanner sc, List<Vehicle> garage) {
        System.out.print("Type (Car/Boat/Truck/Motorcycle/Tractor/JetSki/Airplane): ");
        String type = sc.nextLine().trim();

        System.out.print("Make: ");
        String make = sc.nextLine().trim();

        System.out.print("Model: ");
        String model = sc.nextLine().trim();

        System.out.print("Year: ");
        int year = Integer.parseInt(sc.nextLine().trim());

        Vehicle v = switch (type.toLowerCase()) {
            case "car" -> {
                System.out.print("Num doors: ");
                int doors = Integer.parseInt(sc.nextLine().trim());
                yield new Car(make, model, year, doors);
            }
            case "boat" -> new Boat(make, model, year, 4.0);
            case "truck" -> new Truck(make, model, year, 3);
            case "motorcycle" -> new Motorcycle(make, model, year, 650);
            case "tractor" -> new Tractor(make, model, year, 120);
            case "jetski" -> new Jetski(make, model, year, 2);
            case "airplane" -> new Airplane(make, model, year, 2);
            default -> null;
            };

            if (v != null) {
                garage.add(v);
                System.out.println("Added: " + v);
            } else {
                System.out.println("Unknown type, nothing added.");
            }
        }

    static void listAll(List<Vehicle> garage) {
        if (garage.isEmpty()) System.out.println("No vehicles yet.");
        else garage.forEach(System.out::println);
    }

    static void operate(Scanner sc, List<Vehicle> garage) {
        System.out.print("Enter vehicle ID: ");
        int id = Integer.parseInt(sc.nextLine().trim());

        Vehicle v = garage.stream().filter(x -> x.getId() == id).findFirst().orElse(null);
        if (v == null) { System.out.println("Not found."); return; }

        System.out.print("Action (start/stop/accelerate): ");
        String action = sc.nextLine().trim().toLowerCase();

        switch (action) {
            case "start" -> v.start();
            case "stop" -> v.stop();
            case "accelerate" -> {
                System.out.print("By how much? ");
                int amt = Integer.parseInt(sc.nextLine().trim());
                v.accelerate(amt);
            }
            default -> System.out.println("Unknown action.");
        }
    }

    static void searchByType(Scanner sc, List<Vehicle> garage) {
        System.out.print("Type to list: ");
        String type = sc.nextLine().trim().toLowerCase();
        garage.stream()
        .filter(v -> v.getClass().getSimpleName().toLowerCase().equals(type))
        .forEach(System.out::println);
    }

    static void summarize(List<Vehicle> garage) {
        Map<String, Long> counts = new TreeMap<>();
        for (Vehicle v : garage) {
            String key = v.getClass().getSimpleName();
            counts.put(key, counts.getOrDefault(key, 0L) + 1);
        }

        if (counts.isEmpty()) System.out.println("No vehicles yet.");
        else counts.forEach((k, v) -> System.out.println(k + ": " + v));
    }
}