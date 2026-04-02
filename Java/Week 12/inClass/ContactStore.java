import java.util.List;

/**
 * Store interface for managing contacts.
 * REQUIRED methods for this exercise:
 *  - addContact(name, phone): adds or throws DuplicateContactException if name exists
 *  - count(): number of contacts
 *  - listAll(): all contacts (any order ok; map order is fine)
 *  - listByPrefix(prefix): contacts whose names start with prefix
 *  - get(name): return the Contact or null if not found
 *  - remove(name): remove by name; return true if removed
 */
public interface ContactStore {
    void addContact(String name, String phone) throws DuplicateContactException;
    int count();
    List<Contact> listAll();
    List<Contact> listByPrefix(String prefix);
    Contact get(String name);
    boolean remove(String name);
}
