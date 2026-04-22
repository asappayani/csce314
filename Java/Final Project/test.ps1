$javaSources = Get-ChildItem -Path src\model -Recurse -Filter *.java |
    Select-Object -ExpandProperty FullName

javac -d src $javaSources

java -ea -cp src model.ScoreboardTests