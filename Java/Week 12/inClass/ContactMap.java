import java.util.*;

public class ContactMap implements ContactStore {
    private final Map<String, String> map = new HashMap<>(); // name -> phone

    @Override
    public void addContact(String name, String phone) throws DuplicateContactException {
        // TODO:
        // 1. Decide what key should be used for storage (trimmed name is a good idea).
        // 2. If the name already exists in the map, throw
        //    new DuplicateContactException("Contact already exists: " + name);
        // 3. Otherwise add the contact to the map.
        Contact c = new Contact(name, phone);

        if (map.containsKey(c.getName())) {
            throw new DuplicateContactException("Contact already exists: " + name);
        }

        map.put(c.getName(), c.getPhone());
    }

    @Override
    public int count() {
        // TODO: return the number of contacts in the map
        return map.size();
    }

    @Override
    public List<Contact> listAll() {
        // TODO:
        // Convert each map entry into a Contact object and return as a List<Contact>.
        List<Contact> contactList = new ArrayList<>();
        
        for (Map.Entry<String, String> entry : map.entrySet()) {
            String key = entry.getKey();
            String val = entry.getValue();

            Contact newContact = new Contact(key, val);
            contactList.add(newContact);
        }

        return contactList;
    }

    @Override
    public List<Contact> listByPrefix(String prefix) {
        // TODO:
        // 1. If prefix is null, treat it as "".
        // 2. Return contacts whose names start with the prefix.
        // 3. Convert matching map entries into Contact objects.
        if (prefix == null) {
            prefix = "";
        }

        List<Contact> contactList = new ArrayList<>();

        for (Map.Entry<String, String> entry : map.entrySet()) {
            String key = entry.getKey();
            String val = entry.getValue();
            
            if (key.contains(prefix)){
                Contact newContact = new Contact(key, val);
                contactList.add(newContact);
            }
        }

        return contactList;
    }

    @Override
    public Contact get(String name) {
        // TODO:
        // 1. Look up the phone number by name.
        // 2. If not found, return null.
        // 3. Otherwise return a new Contact(name, phone).
        if (map.get(name) == null) return null;

        String phone = map.get(name);
        return new Contact(name, phone);
    }

    @Override
    public boolean remove(String name) {
        // TODO:
        // Remove the contact by name.
        // Return true if something was removed, otherwise false.
        if (!map.containsKey(name)) return false;

        map.remove(name);
        return true;
    }
}
