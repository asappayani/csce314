import java.util.Scanner;

public class ScoreboardTests {

    private static void printMenu() {
        System.out.println("\n=== Scoreboard CLI ===");
        System.out.println("1) Show scoreboard");
        System.out.println("2) Set home team name");
        System.out.println("3) Set away team name");
        System.out.println("4) Add home score");
        System.out.println("5) Add away score");
        System.out.println("6) Show last action");
        System.out.println("7) Undo last action");
        System.out.println("8) Clear scores");
        System.out.println("9) Run edge-case diagnostics");
        System.out.println("0) Exit");
        System.out.print("Choose: ");
    }

    private static void showState(Team home, Team away, TeamManager teamManager, ScoreManager scoreManager) {
        System.out.println("\n--- Current State ---");
        System.out.println("Home: " + teamManager.getHomeTeamName() + " | Score: " + home.getScore());
        System.out.println("Away: " + teamManager.getAwayTeamName() + " | Score: " + away.getScore());
        System.out.println("Last Action: " + scoreManager.displayLastAction());
    }

    private static int readInt(Scanner sc, String prompt) {
        while (true) {
            System.out.print(prompt);
            String line = sc.nextLine();
            try {
                return Integer.parseInt(line.trim());
            } catch (NumberFormatException ex) {
                System.out.println("Invalid number. Try again.");
            }
        }
    }

    private static String readText(Scanner sc, String prompt) {
        System.out.print(prompt);
        return sc.nextLine();
    }

    private static void runDiagnostics(ScoreManager scoreManager) {
        System.out.println("\nRunning diagnostics...");
        System.out.println("1) Calling displayLastAction() before any scoring action.");
        try {
            String action = scoreManager.displayLastAction();
            System.out.println("No error. Result: " + action);
        } catch (Exception e) {
            System.out.println("Caught error: " + e.getClass().getSimpleName() + " -> " + e.getMessage());
        }

        System.out.println("2) Calling undo() before any scoring action.");
        try {
            scoreManager.undo();
            System.out.println("No error.");
        } catch (Exception e) {
            System.out.println("Caught error: " + e.getClass().getSimpleName() + " -> " + e.getMessage());
        }
    }

    public static void main(String[] args) {
        Team home = new Team();
        Team away = new Team();

        TeamManager teamManager = new TeamManager(home, away);
        ScoreManager scoreManager = new ScoreManager(home, away);

        Scanner sc = new Scanner(System.in);
        boolean running = true;

        System.out.println("Scoreboard CLI started.");
        showState(home, away, teamManager, scoreManager);

        while (running) {
            printMenu();
            int choice = readInt(sc, "");

            try {
                switch (choice) {
                    case 1:
                        showState(home, away, teamManager, scoreManager);
                        break;

                    case 2: {
                        String name = readText(sc, "New home team name: ");
                        teamManager.setHomeTeamName(name);
                        System.out.println("Home team renamed.");
                        showState(home, away, teamManager, scoreManager);
                        break;
                    }

                    case 3: {
                        String name = readText(sc, "New away team name: ");
                        teamManager.setAwayTeamName(name);
                        System.out.println("Away team renamed.");
                        showState(home, away, teamManager, scoreManager);
                        break;
                    }

                    case 4: {
                        int points = readInt(sc, "Points to add to home: ");
                        String type = readText(sc, "Score type label (e.g. TD, FG): ");
                        scoreManager.updateHomeScore(points, type);
                        System.out.println("Home score updated.");
                        showState(home, away, teamManager, scoreManager);
                        break;
                    }

                    case 5: {
                        int points = readInt(sc, "Points to add to away: ");
                        String type = readText(sc, "Score type label (e.g. TD, FG): ");
                        scoreManager.updateAwayScore(points, type);
                        System.out.println("Away score updated.");
                        showState(home, away, teamManager, scoreManager);
                        break;
                    }

                    case 6:
                        System.out.println("Last action: " + scoreManager.displayLastAction());
                        break;

                    case 7:
                        scoreManager.undo();
                        System.out.println("Undo attempted.");
                        showState(home, away, teamManager, scoreManager);
                        break;

                    case 8:
                        scoreManager.clear();
                        System.out.println("Scores cleared.");
                        showState(home, away, teamManager, scoreManager);
                        break;

                    case 9:
                        runDiagnostics(scoreManager);
                        break;

                    case 0:
                        running = false;
                        break;

                    default:
                        System.out.println("Unknown option.");
                }
            } catch (IllegalStateException e) {
                System.out.println("Validation error: " + e.getMessage());
            } catch (Exception e) {
                System.out.println("Operation failed: " + e.getClass().getSimpleName() + " -> " + e.getMessage());
            }
        }

        sc.close();
        System.out.println("CLI exited.");
    }
}