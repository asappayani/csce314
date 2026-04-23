javac -d src `
    src\model\ScoreManager.java `
    src\model\Team.java `
    src\model\TeamManager.java `
    src\model\ScoreboardTests.java

java -ea -cp src model.ScoreboardTests