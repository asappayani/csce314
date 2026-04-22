$javafx = "C:\Users\trecv\Downloads\openjfx-26_windows-x64_bin-sdk\javafx-sdk-26\lib"
$outDir = "out"

if (!(Test-Path $outDir)) {
    New-Item -ItemType Directory -Path $outDir | Out-Null
}

$javaSources = Get-ChildItem -Path src -Recurse -Filter *.java |
    Where-Object { $_.Name -notmatch "Test" } |
    Select-Object -ExpandProperty FullName

if ($javaSources.Count -eq 0) {
    Write-Error "No Java source files found."
    exit 1
}

javac --module-path $javafx --add-modules javafx.controls,javafx.fxml -d $outDir $javaSources

if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
}

# Recreate resource folder to avoid stale nested copies like out\view\view
Remove-Item -Path "$outDir\view" -Recurse -Force -ErrorAction SilentlyContinue
New-Item -ItemType Directory -Path "$outDir\view" | Out-Null
Copy-Item -Path src\view\* -Destination "$outDir\view" -Recurse -Force

java --module-path $javafx --add-modules javafx.controls,javafx.fxml --enable-native-access=javafx.graphics -cp $outDir app.App