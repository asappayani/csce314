public class ScoreManager {

    private final Team homeTeam;
    private final Team awayTeam;

    private Team lastTeam;
    private int lastScoreChange;
    private String scoreType;

    public ScoreManager(Team team1, Team team2){
        this.homeTeam = team1;
        this.awayTeam = team2;
    }

    private void scoringAllowed() {
        String homeName = homeTeam.getTeamName();
        String awayName = awayTeam.getTeamName();

        boolean homeMissing = homeName == null || homeName.trim().isEmpty();
        boolean awayMissing = awayName == null || awayName.trim().isEmpty();

        if (homeMissing || awayMissing) {
            throw new IllegalStateException("Set both team names before scoring.");
        }
    }

    public void updateHomeScore(int score, String scoreType) {
        scoringAllowed();
        homeTeam.updateScore(score);

        updateLastAction(homeTeam, score, scoreType);
    }

    public void updateAwayScore(int score, String scoreType){
        scoringAllowed();
        awayTeam.updateScore(score);
        
        updateLastAction(awayTeam, score, scoreType);
    }

    public void updateLastAction(Team team, int score, String scoreType) {
        this.lastTeam = team;
        this.lastScoreChange = score;
        this.scoreType = scoreType;
    }

    public String displayLastAction(){
        if (lastTeam == null) return "None";

        return lastTeam.getTeamName() + " +" + lastScoreChange + " (" + scoreType + ")"; 
    }

    public void undo(){
        if (lastTeam == null) throw new IllegalStateException("There is no previous action to undo.");
        
        lastTeam.updateScore(-lastScoreChange);
    }

    public void clear(){
        homeTeam.resetScore();
        awayTeam.resetScore();

        lastTeam = null;
    }
}