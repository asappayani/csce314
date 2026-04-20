public class ScoreboardTests {

    // some of my model's methods throw errors, so this will make sure an error is thrown properly etc.
    private static void assertThrows(Class<? extends Throwable> expected, Runnable action, String message) {
        try {
            action.run();
            throw new AssertionError(message + " - expected " + expected.getSimpleName());
        } catch (Throwable actual) {
            if (!expected.isInstance(actual)) {
                throw new AssertionError(
                    message + " - expected " + expected.getSimpleName()
                    + ", but got " + actual.getClass().getSimpleName()
                );
            }
        }
    }

    public static void main(String[] args) {
        try {
            Team home = new Team();
            Team away = new Team();
            TeamManager teamManager = new TeamManager(home, away);
            ScoreManager scoreManager = new ScoreManager(home, away);

            assert scoreManager.displayLastAction().equals("None")
                : "Initial last action should be None";

            assertThrows(IllegalStateException.class,
                () -> scoreManager.updateHomeScore(6, "Touchdown"),
                "Scoring before team names are set should fail"
            );

            assertThrows(IllegalStateException.class,
                () -> scoreManager.undo(),
                "Undo before any scoring action should fail"
            );

            teamManager.setHomeTeamName("Aggies");
            teamManager.setAwayTeamName("Gamecocks");

            assert teamManager.getHomeTeamName().equals("Aggies")
                : "Home team name should be Aggies";
            assert teamManager.getAwayTeamName().equals("Gamecocks")
                : "Away team name should be Gamecocks";

            scoreManager.updateHomeScore(6, "Touchdown");
            assert home.getScore() == 6 : "Home score should be 6";
            assert scoreManager.displayLastAction().equals("Aggies +6 (Touchdown)")
                : "Last action should reflect the home score";

            scoreManager.updateAwayScore(3, "Field Goal");
            assert away.getScore() == 3 : "Away score should be 3";
            assert scoreManager.displayLastAction().equals("Gamecocks +3 (Field Goal)")
                : "Last action should reflect the away score";

            scoreManager.undo();
            assert away.getScore() == 0 : "Undo should revert the away score";
            assert scoreManager.displayLastAction().equals("Gamecocks +3 (Field Goal)")
                : "Undo should not erase the last action text";

            scoreManager.clear();
            assert home.getScore() == 0 : "Home score should be cleared";
            assert away.getScore() == 0 : "Away score should be cleared";
            assert scoreManager.displayLastAction().equals("None")
                : "Clear should reset last action to None";

            assertThrows(IllegalStateException.class,
                () -> scoreManager.undo(),
                "Undo after clear should fail when last action is null"
            );

            System.out.println("All tests PASS");
        } catch (Throwable t) {
            System.out.println("FAIL: " + t.getMessage());
        }
    }
}