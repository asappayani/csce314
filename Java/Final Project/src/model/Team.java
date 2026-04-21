package model;

public class Team {
    private String teamName;
    private int score;

    public Team() {
        this.teamName = "";
        this.score = 0;
    }

    public Team (String teamName) {
        this.teamName = teamName;
        this.score = 0;
    }

    //if i feel like it then overload the constructor more to cover all cases

    public String getTeamName(){
        return this.teamName;
    }

    public int getScore(){
        return this.score;
    }

    public void updateTeamName(String teamName){
        this.teamName = teamName;
    }

    public void updateScore(int score){
        this.score += score; 
    }

    public void resetScore(){
        this.score = 0;
    }
}