import java.util.*;

/**
 * Minimal CLI scaffold.
 *
 * MENU:
 * 1) Add contact
 * 2) List all
 * 3) Find by prefix
 * 4) Get by name
 * 5) Remove by name
 * 6) Count
 * 0) Quit
 *
 * NOTE: Expect DuplicateContactException when adding a duplicate name.
 */
public class Main {
    private static final Scanner in = new Scanner(System.in);

    public static void main(String[] args) {
        ContactStore store = new ContactMap();

        println("Contact Manager (Map + Interface + Exception)");
        boolean running = true;
        while (running) {
            showMenu();
            String choice = prompt("Choice: ");
            switch (choice) {
                case "1":
                    handleAdd(store);
                    break;
                case "2":
                    handleListAll(store);
                    break;
                case "3":
                    handlePrefix(store);
                    break;
                case "4":
                    handleGet(store);
                    break;
                case "5":
                    handleRemove(store);
                    break;
                case "6":
                    println("Total contacts: " + store.count());
                    break;
                case "0":
                    running = false;
                    println("Goodbye!");
                    break;
                default:
                    println("Unknown option.");
            }
        }
    }

    private static void handleAdd(ContactStore store) {
        String name = prompt("Name: ");
        String phone = prompt("Phone: ");
        try {
            store.addContact(name, phone);
            println("[OK] Added: " + name);
        } catch (DuplicateContactException ex) {
            println("[ERROR] " + ex.getMessage());
        } catch (Exception ex) {
            println("[ERROR] " + ex);
        }
    }

    private static void handleListAll(ContactStore store) {
        for (Contact c : store.listAll()) {
            println(c.toString());
        }
    }

    private static void handlePrefix(ContactStore store) {
        String prefix = prompt("Prefix: ");
        for (Contact c : store.listByPrefix(prefix)) {
            println(c.toString());
        }
    }

    private static void handleGet(ContactStore store) {
        String name = prompt("Name: ");
        Contact c = store.get(name);
        println(c == null ? "(not found)" : c.toString());
    }

    private static void handleRemove(ContactStore store) {
        String name = prompt("Name: ");
        boolean ok = store.remove(name);
        println(ok ? "[OK] Removed." : "(not found)");
    }

    private static void showMenu() {
        println("");
        println("1) Add contact");
        println("2) List all");
        println("3) Find by prefix");
        println("4) Get by name");
        println("5) Remove by name");
        println("6) Count");
        println("0) Quit");
    }

    private static String prompt(String msg) {
        System.out.print(msg);
        return in.nextLine().trim();
    }

    private static void println(String s) {
        System.out.println(s);
    }
}
