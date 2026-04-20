public class TeamManager {
    private final Team homeTeam;
    private final Team awayTeam;

    public TeamManager(Team team1, Team team2) {
        this.homeTeam = team1;
        this.awayTeam = team2;
    }

    public String getHomeTeamName() {
        return homeTeam.getTeamName();
    }

    public String getAwayTeamName() {
        return awayTeam.getTeamName();
    }

    public void setHomeTeamName(String teamName) {
        validateTeamName(teamName);
        homeTeam.updateTeamName(teamName);
    }

    public void setAwayTeamName(String teamName) {
        validateTeamName(teamName);
        awayTeam.updateTeamName(teamName);
    }

    public void validateTeamName(String teamName) {
        if (teamName == null || teamName.trim().isEmpty())
            throw new IllegalStateException("Team name cannot be empty!");
    }
}