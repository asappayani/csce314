package model;

public class ScoreboardTests {

    public static void main(String[] args) {
        try {
            Team home = new Team();
            Team away = new Team();
            TeamManager teamManager = new TeamManager(home, away);
            ScoreManager scoreManager = new ScoreManager(home, away);

            assert scoreManager.displayLastAction().equals("None")
                : "Initial last action should be None";

            boolean blockedHomeScore = false;
            try {
                scoreManager.updateHomeScore(6, "Touchdown");
            } catch (IllegalStateException e) {
                blockedHomeScore = true;
            }
            assert blockedHomeScore : "Scoring before team names are set should fail";

            boolean blockedUndo = false;
            try {
                scoreManager.undo();
            } catch (IllegalStateException e) {
                blockedUndo = true;
            }
            assert blockedUndo : "Undo before any scoring action should fail";

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

            boolean blockedUndoAfterClear = false;
            try {
                scoreManager.undo();
            } catch (IllegalStateException e) {
                blockedUndoAfterClear = true;
            }
            assert blockedUndoAfterClear
                : "Undo after clear should fail when last action is null";

            System.out.println("All tests PASS");
        } catch (Throwable t) {
            System.out.println("FAIL: " + t.getMessage());
        }
    }
}