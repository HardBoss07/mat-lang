# Self-Elevate to Administrator if not already running as Admin
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
if (-not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "==> Requesting Administrator Privileges..." -ForegroundColor Yellow
    Start-Process powershell -Verb RunAs -ArgumentList "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`""
    exit
}

$ErrorActionPreference = "Stop"

# Configuration
$LLVM_VERSION = "llvmorg-18.1.8"         # Stable LLVM 18 Release
$INSTALL_DIR = "C:\llvm-msvc"             # Target install destination
$WORK_DIR = "$env:TEMP\llvm-build-src"     # Temporary source/build folder

Write-Host "==> Running with Administrator Privileges" -ForegroundColor Green
Write-Host "==> Target Install Directory: $INSTALL_DIR" -ForegroundColor Cyan
Write-Host "==> Temporary Work Directory: $WORK_DIR" -ForegroundColor Cyan

$buildSuccess = $false

try {
    # 1. State Check: Check if repository already exists from a previous run
    if (Test-Path -Path "$WORK_DIR\llvm") {
        Write-Host "`n[State Check] Existing LLVM source repo found at $WORK_DIR" -ForegroundColor Yellow
        $response = Read-Host "Do you want to REUSE the existing repo to save download time? (Y/n)"
        if ($response -eq "" -or $response -match "^[Yy]") {
            Write-Host "==> Reusing existing LLVM repository." -ForegroundColor Green
        } else {
            Write-Host "==> Removing old repository and re-cloning..." -ForegroundColor Yellow
            Remove-Item -Path $WORK_DIR -Recurse -Force
            Write-Host "==> Cloning LLVM repository ($LLVM_VERSION)..." -ForegroundColor Green
            git clone --depth 1 --branch $LLVM_VERSION https://github.com/llvm/llvm-project.git $WORK_DIR
        }
    } else {
        Write-Host "==> Cloning LLVM repository ($LLVM_VERSION)..." -ForegroundColor Green
        git clone --depth 1 --branch $LLVM_VERSION https://github.com/llvm/llvm-project.git $WORK_DIR
    }

    if (-not (Test-Path -Path "$WORK_DIR\llvm")) {
        throw "Failed to locate or clone LLVM source repository at $WORK_DIR\llvm"
    }

    # 2. Setup or clean build directory
    $BUILD_DIR = Join-Path $WORK_DIR "build"
    if (-not (Test-Path -Path $BUILD_DIR)) {
        New-Item -ItemType Directory -Path $BUILD_DIR -Force | Out-Null
    }
    Set-Location -Path $BUILD_DIR

    # 3. Configure CMake targeting Visual Studio 2022 MSVC
    Write-Host "==> Configuring CMake for MSVC..." -ForegroundColor Green
    cmake -S "$WORK_DIR\llvm" -B . -G "Ninja"`
      -DCMAKE_BUILD_TYPE=Release `
      -DCMAKE_INSTALL_PREFIX="$INSTALL_DIR" `
      -DLLVM_TARGETS_TO_BUILD="all" `
      -DLLVM_BUILD_TOOLS=ON `
      -DLLVM_BUILD_UTILS=ON `
      -DLLVM_INCLUDE_UTILS=ON `
      -DLLVM_BUILD_LLVM_C_DYLIB=OFF `
      -DLLVM_ENABLE_RTTI=ON `
      -DLLVM_ENABLE_EH=ON `
      -DLLVM_INCLUDE_TESTS=OFF `
      -DLLVM_INCLUDE_EXAMPLES=OFF `
      -DLLVM_INCLUDE_BENCHMARKS=OFF

    # 4. Build Release configuration
    Write-Host "==> Compiling LLVM (this may take 20-40 minutes depending on CPU core count)..." -ForegroundColor Green
    cmake --build . --config Release --parallel 16

    # 5. Install to C:\llvm-msvc
    Write-Host "==> Installing binaries to $INSTALL_DIR..." -ForegroundColor Green
    cmake --build . --config Release --target install

    if (-not (Test-Path -Path "$INSTALL_DIR\bin\llvm-config.exe")) {
        throw "Compilation finished, but $INSTALL_DIR\bin\llvm-config.exe was not created."
    }

    # 6. Environmental Check & Cleanup / Update for System Variables
    Write-Host "==> Inspecting & configuring System Environment Variables..." -ForegroundColor Green

    # Set LLVM_SYS_180_PREFIX system-wide
    [System.Environment]::SetEnvironmentVariable("LLVM_SYS_180_PREFIX", $INSTALL_DIR, [System.EnvironmentVariableTarget]::Machine)

    # Clean & update System PATH
    $systemPath = [System.Environment]::GetEnvironmentVariable("PATH", [System.EnvironmentVariableTarget]::Machine)
    $llvmBinPath = "$INSTALL_DIR\bin"

    if ($systemPath -notlike "*$llvmBinPath*") {
        $cleanPath = ($systemPath -split ';' | Where-Object {$_ -ne "" }) -join ';'
        $newPath = "$cleanPath;$llvmBinPath"
        [System.Environment]::SetEnvironmentVariable("PATH", $newPath, [System.EnvironmentVariableTarget]::Machine)
        Write-Host "==> Added $llvmBinPath to System PATH" -ForegroundColor Green
    } else {
        Write-Host "==> $llvmBinPath is already present in System PATH" -ForegroundColor Yellow
    }

    # 7. Environmental Check & Cleanup for Git Bash ~/.bashrc
    $userHome = [System.Environment]::GetFolderPath('UserProfile')
    $bashrcPath = Join-Path $userHome ".bashrc"

    $bashExports = @"

# LLVM MSVC Configuration for inkwell / llvm-sys
export LLVM_SYS_180_PREFIX="C:/llvm-msvc"
export PATH="`$PATH:/c/llvm-msvc/bin"
"@

    if (Test-Path -Path $bashrcPath) {
        $currentBashrc = Get-Content -Path $bashrcPath -Raw
        if ($currentBashrc -notlike "*LLVM_SYS_180_PREFIX*") {
            Add-Content -Path $bashrcPath -Value $bashExports
            Write-Host "==> Appended LLVM exports to $bashrcPath" -ForegroundColor Green
        } else {
            Write-Host "==> Git Bash ~/.bashrc already configured." -ForegroundColor Yellow
        }
    } else {
        Set-Content -Path $bashrcPath -Value $bashExports
        Write-Host "==> Created $bashrcPath with LLVM exports" -ForegroundColor Green
    }

    $buildSuccess = $true
    Write-Host "`n==> SUCCESS: LLVM 18 installed and system variables configured!" -ForegroundColor Cyan
    Write-Host "==> Verification test running from $INSTALL_DIR\bin\llvm-config.exe:" -ForegroundColor Cyan
    & "$INSTALL_DIR\bin\llvm-config.exe" --version

} catch {
    Write-Host "`n[ERROR] BUILD FAILED AT STEP: $_" -ForegroundColor Red
    Write-Host "[ERROR] Details: $($_.Exception.Message)" -ForegroundColor Red
    Write-Host "[ERROR] Script line: $($_.InvocationInfo.ScriptLineNumber)" -ForegroundColor Red
} finally {
    # 8. Prompt Before Deleting Source Repository
    Set-Location -Path $env:TEMP
    if (Test-Path -Path $WORK_DIR) {
        Write-Host "`n[Cleanup Check] Source repository directory exists at $WORK_DIR" -ForegroundColor Yellow
        $deletePrompt = Read-Host "Do you want to DELETE the source folder ($WORK_DIR)? (y/N)"
        if ($deletePrompt -match "^[Yy]") {
            Remove-Item -Path $WORK_DIR -Recurse -Force -ErrorAction SilentlyContinue
            Write-Host "==> Removed temporary source directory." -ForegroundColor Green
        } else {
            Write-Host "==> Retaining source directory for future reuse or debugging." -ForegroundColor Yellow
        }
    }
}

if (-not $buildSuccess) {
    Write-Host "`nBuild terminated with errors. Review the messages above." -ForegroundColor Red
} else {
    Write-Host "`nNote: Restart open terminal windows (PowerShell / Git Bash) for changes to take effect." -ForegroundColor Yellow
}

pause