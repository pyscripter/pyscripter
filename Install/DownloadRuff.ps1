# Script to download and extract ONLY ruff.exe from the latest Ruff release
$Repo = "astral-sh/ruff"
$Pattern = "ruff-x86_64-pc-windows-msvc.zip"

# Step 1: Get the latest release information from GitHub API
Write-Host "Fetching latest release information for $Repo..."
$ReleaseInfo = Invoke-RestMethod -Uri "https://api.github.com/repos/$Repo/releases/latest"

# Step 2: Find the specific asset we're looking for
$Asset = $ReleaseInfo.assets | Where-Object { $_.name -eq $Pattern }

if (-not $Asset) {
    Write-Error "Asset matching pattern '$Pattern' was not found in the latest release."
    exit 1
}

$ZipFile = $Asset.name
$DownloadUrl = $Asset.browser_download_url
$TempDir = "ruff-temp-$(Get-Random)"

Write-Host "Found latest release: $($ReleaseInfo.name)"
Write-Host "Downloading $ZipFile..."

# Step 3: Download the ZIP file
Invoke-WebRequest -Uri $DownloadUrl -OutFile $ZipFile

# Step 4: Extract to temp directory and copy only ruff.exe
Write-Host "Extracting ruff.exe..."
Expand-Archive -Path $ZipFile -DestinationPath $TempDir -Force

# Step 5: Find and move ruff.exe to current directory
$RuffExePath = Join-Path $TempDir "ruff.exe"
if (Test-Path $RuffExePath) {
    Move-Item -Path $RuffExePath -Destination ".\ruff.exe" -Force
    Write-Host "ruff.exe extracted successfully"
} else {
    Write-Error "ruff.exe was not found in the extracted files"
}

# Step 6: Clean up
Remove-Item -Path $ZipFile -Force
Remove-Item -Path $TempDir -Recurse -Force

Write-Host "Process completed successfully. ruff.exe is ready in the current directory."