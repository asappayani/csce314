import java.util.List;

public class Main {
    public static void main(String[] args) {
        Library lib = new Library();

        LibraryItem hb = new Hardback("Effective Java", "Joshua Bloch", 416);
        LibraryItem pb = new Paperback("Clean Code", "Robert C. Martin", 464);
        LibraryItem mag = new Periodical("ACM Communications", "Oct 2025");
        LibraryItem phm = new Hardback("Project Hail Mary", "Andy Weir", 496);
        LibraryItem tg = new Paperback("The Giver", "I Forgot", 67);
        LibraryItem wh = new Hardback("Wuthering Heights", "Emily Bronte", 41);

        lib.addItem(hb); 
        lib.addItem(pb); 
        lib.addItem(mag);
        lib.addItem(phm); 
        lib.addItem(tg); 
        lib.addItem(wh);

        Patron alice = new Patron("Alice");
        Patron bob = new Patron("Bob");
        Patron chris = new Patron("Chris");
        Patron julian = new Patron("Julian");
        Patron brix = new Patron("Brixton");

        lib.addPatron(alice); 
        lib.addPatron(bob);
        lib.addPatron(chris); 
        lib.addPatron(julian);
        lib.addPatron(brix); 

        lib.printInventory(); 
        System.out.println();

        System.out.println("=== Interface Demo (Checkable) ===");
        for (Checkable c : List.of(hb, pb, mag, phm, tg, wh)) {
            System.out.println(((LibraryItem)c).getTitle() + " checked out? " + c.isCheckedOut());
        }
        System.out.println();

        boolean a1 = lib.checkout(alice.getId(), hb.getId());
        boolean a2 = lib.checkout(alice.getId(), pb.getId());
        System.out.printf("Alice borrow results: %b, %b%n", a1, a2);

        boolean b1 = lib.checkout(bob.getId(), mag.getId());
        boolean b2 = lib.checkout(bob.getId(), pb.getId()); // should fail
        System.out.printf("Bob borrow results: %b, %b%n", b1, b2);

        boolean c1 = lib.checkout(chris.getId(), phm.getId());
        System.out.printf("Chris borrow results: %b%n", c1);

        boolean d1 = lib.checkout(julian.getId(), tg.getId());
        System.out.printf("Julian borrow results: %b%n", d1);

        boolean e1 = lib.checkout(brix.getId(), wh.getId());
        System.out.printf("Brixton borrow results: %b%n", e1);
        System.out.println();
        
        lib.printInventory(); 
        System.out.println();
        lib.printPatrons(); 
        System.out.println();

        System.out.println("=== Returns and Re-Checkouts ===");
        boolean r1 = lib.checkin(pb.getId());              // Alice returns paperback
        boolean r2 = lib.checkout(bob.getId(), pb.getId()); // Bob borrows it
        System.out.printf("Return then borrow: %b, %b%n", r1, r2);
        System.out.println();

        lib.printInventory(); 
        System.out.println();
        lib.printPatrons();
    }
}
