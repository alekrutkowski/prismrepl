# Exercise the installed package in a real Windows ConPTY session.
# Requires Windows 10 1809 or newer, PowerShell, R, and the matching Rtools.
# Production prismrepl does not require PowerShell, .NET, or this test probe.
[CmdletBinding()]
param(
    [string] $Rterm = "",
    [int] $TimeoutSeconds = 45,
    [string] $LogPath = "windows-console-smoke.log"
)
$ErrorActionPreference = 'Stop'
if ([Environment]::OSVersion.Platform -ne [PlatformID]::Win32NT) {
    throw 'This integration test requires Windows and the ConPTY API.'
}
$packageRoot = Split-Path -Parent $PSScriptRoot
$nativeRoot = Join-Path $PSScriptRoot 'native'
$temporary = Join-Path ([IO.Path]::GetTempPath()) ('prismrepl-conpty-' + [Guid]::NewGuid())
$session = $null
$esc = [char]27
$oldEnvironment = @{}
foreach ($name in @('TERM', 'COLORTERM', 'NO_COLOR', 'PRISM_CONSOLE_PROBE')) {
    $oldEnvironment[$name] = [Environment]::GetEnvironmentVariable($name, 'Process')
}

function Get-PrismPlainText([string] $text) {
    $text = [regex]::Replace($text, '\x1b\][^\x07\x1b]*(?:\x07|\x1b\\)', '')
    $text = [regex]::Replace($text, '\x1b\[[0-?]*[ -/]*[@-~]', '')
    $text = [regex]::Replace($text, '\x1b[78]', '')
    return $text.Replace("`r", '')
}

function Wait-PrismText([string] $marker) {
    $timer = [Diagnostics.Stopwatch]::StartNew()
    while ($timer.Elapsed.TotalSeconds -lt $TimeoutSeconds) {
        $plain = Get-PrismPlainText $session.Snapshot()
        if ($plain.Contains($marker)) { return }
        if ($session.HasExited) { break }
        Start-Sleep -Milliseconds 50
    }
    $plain = Get-PrismPlainText $session.Snapshot()
    $tail = $plain.Substring([Math]::Max(0, $plain.Length - 10000))
    throw "Missing console output '$marker'.`n$tail"
}

function Send-PrismExpression([string] $code) {
    $session.Send("$esc[200~" + $code + "$esc[201~")
    Start-Sleep -Milliseconds 100
    $session.Send("`r")
}

function Convert-PrismCursorColor([string] $value) {
    if ($value -match '^#[0-9a-fA-F]{6}$') { return $value.ToUpperInvariant() }
    if ($value -match '^rgb:([0-9a-fA-F]{1,4})/([0-9a-fA-F]{1,4})/([0-9a-fA-F]{1,4})$') {
        $channels = @($Matches[1], $Matches[2], $Matches[3])
        $rgb = foreach ($channel in $channels) {
            $maximum = [Math]::Pow(16, $channel.Length) - 1
            '{0:X2}' -f [int][Math]::Round(255 * [Convert]::ToInt32($channel, 16) / $maximum)
        }
        return '#' + ($rgb -join '')
    }
    return ''
}

function Wait-PrismCursorColor([string] $color, [int] $after = 0) {
    $timer = [Diagnostics.Stopwatch]::StartNew()
    while ($timer.Elapsed.TotalSeconds -lt $TimeoutSeconds) {
        $raw = $session.Snapshot()
        if ($raw.Length -gt $after) {
            foreach ($match in [regex]::Matches($raw.Substring($after),
                     '\x1b\]12;([^\x07\x1b]*)(?:\x07|\x1b\\)')) {
                if ((Convert-PrismCursorColor $match.Groups[1].Value) -eq $color) { return }
            }
        }
        if ($session.HasExited) { break }
        Start-Sleep -Milliseconds 50
    }
    throw "Cursor color $color did not reach the ConPTY output stream."
}

try {
    New-Item -ItemType Directory -Path $temporary | Out-Null
    if (-not $Rterm) {
        $command = Get-Command Rterm.exe -ErrorAction SilentlyContinue
        if ($command) { $Rterm = $command.Source }
        else {
            $rscript = (Get-Command Rscript.exe -ErrorAction Stop).Source
            $Rterm = (& $rscript --vanilla -e 'cat(file.path(R.home("bin"), "Rterm.exe"))').Trim()
        }
    }
    $Rterm = (Resolve-Path -LiteralPath $Rterm).Path
    $rExecutable = Join-Path (Split-Path -Parent $Rterm) 'R.exe'
    if (-not (Test-Path -LiteralPath $rExecutable)) {
        $rExecutable = (Get-Command R.exe -ErrorAction Stop).Source
    }
    # The probe deliberately starts from VT disabled and an OEM output page.
    # This makes the output-mode lifetime regression observable in CI.
    Copy-Item (Join-Path $nativeRoot 'windows-console-probe.c') $temporary
    Push-Location $temporary
    try {
        & $rExecutable CMD SHLIB --preclean -o prismprobe.dll windows-console-probe.c
        if ($LASTEXITCODE -ne 0) { throw 'The Windows console test probe did not compile.' }
    } finally { Pop-Location }
    if (-not ('PrismRepl.Tests.ConptyHost' -as [type])) {
        Add-Type -Path (Join-Path $nativeRoot 'ConptyHost.cs')
    }
    [Environment]::SetEnvironmentVariable('TERM', 'xterm-256color', 'Process')
    [Environment]::SetEnvironmentVariable('COLORTERM', 'truecolor', 'Process')
    [Environment]::SetEnvironmentVariable('NO_COLOR', $null, 'Process')
    [Environment]::SetEnvironmentVariable('PRISM_CONSOLE_PROBE',
        (Join-Path $temporary 'prismprobe.dll'), 'Process')
    $session = [PrismRepl.Tests.ConptyHost]::new($Rterm,
        '--vanilla --quiet --interactive', $packageRoot, 110, 34)
    Wait-PrismText '> '
    $bootstrap = (Join-Path $nativeRoot 'windows-console-session.R').Replace('\', '/')
    $quoted = ConvertTo-Json -InputObject $bootstrap -Compress
    $session.Send('source(' + $quoted + ");`r")
    Wait-PrismText 'NATIVE_OUTPUT_GUARD_OK'
    Wait-PrismText ('NATIVE_FRAME_' + [char]0x6f22 + [char]0x5b57 + ' ' + [char]::ConvertFromUtf32(0x1f9ea))
    Wait-PrismText 'r> '
    Wait-PrismCursorColor '#FFFFFF'

    # Both theme changes use an empty input prompt at the same terminal size.
    # These assertions verify the real Rterm -> ConPTY output path; the R model
    # tests separately verify cursor geometry and contrast against the surface.
    $whiteOffset = $session.Snapshot().Length
    Send-PrismExpression ':theme white'
    Wait-PrismCursorColor '#000000' $whiteOffset
    $blackOffset = $session.Snapshot().Length
    Send-PrismExpression ':theme black'
    Wait-PrismCursorColor '#FFFFFF' $blackOffset

    Send-PrismExpression ':ai'
    Wait-PrismText 'AI provider (current:'
    $session.Send("/ChatGPT`r`r")
    Wait-PrismText 'AI provider: ChatGPT'
    Send-PrismExpression ':ai mistral'
    Wait-PrismText 'AI provider: Mistral'

    # Real submitted output, not a marker that already appears in the input.
    Send-PrismExpression 'cat(paste0("EVAL_", 6 * 7, "\n"))'
    Wait-PrismText 'EVAL_42'
    Send-PrismExpression 'warning("ConPTY warning"); message("ConPTY message")'
    Wait-PrismText '[WARNING]'
    Wait-PrismText '[MESSAGE]'
    Send-PrismExpression 'stop("ConPTY error")'
    Wait-PrismText '[ERROR]'
    Send-PrismExpression 'cat(paste0("RECOVERY_", 2 + 3, "\n"))'
    Wait-PrismText 'RECOVERY_5'

    # Both input identifiers and output contain non-ASCII characters.
    $objectName = 'caf' + [string][char]0xe9
    $objectValue = [string][char]0x6f22 + [string][char]0x5b57 + ' ' +
        [char]::ConvertFromUtf32(0x1f9ea)
    $unicodeCode = $objectName + ' <- "' + $objectValue + '"; cat(paste0("UNICODE_", ' +
        $objectName + ', "\n"))'
    Send-PrismExpression $unicodeCode
    Wait-PrismText ('UNICODE_' + $objectValue)

    # Enter inside bracketed paste cannot submit the buffer.
    $session.Send("$esc[200~cat(paste0(`"PASTE_`", 99, `"\n`"))`n$esc[201~")
    Start-Sleep -Milliseconds 250
    if ((Get-PrismPlainText $session.Snapshot()).Contains('PASTE_99')) {
        throw 'Bracketed paste executed before an explicit Enter.'
    }
    $session.Send("`r")
    Wait-PrismText 'PASTE_99'

    # A complete expression followed by Ctrl+J stays in the multiline editor.
    $session.Send('cat(paste0("CTRLJ_", 40, "\n"))' + [char]10)
    Start-Sleep -Milliseconds 250
    if ((Get-PrismPlainText $session.Snapshot()).Contains('CTRLJ_40')) {
        throw 'Ctrl+J submitted the expression instead of inserting a newline.'
    }
    $session.Send('cat(paste0("CTRLJ_SECOND_", 2, "\n"))' + "`r")
    Wait-PrismText 'CTRLJ_40'
    Wait-PrismText 'CTRLJ_SECOND_2'

    # Cancel an incomplete expression, then confirm the console remains usable.
    $session.Send('this_must_not_run(' + [char]3)
    Start-Sleep -Milliseconds 150
    Send-PrismExpression 'cat(paste0("CANCEL_", 21 * 2, "\n"))'
    Wait-PrismText 'CANCEL_42'

    Send-PrismExpression 'prism_completion_object <- 17L; cat(paste0("READY_", "COMPLETION", "\n"))'
    Wait-PrismText 'READY_COMPLETION'
    $session.Send('cat(paste0("COMPLETION_", prism_completion_o' + [char]9)
    Start-Sleep -Milliseconds 100
    $session.Send(', "\n"))' + "`r")
    Wait-PrismText 'COMPLETION_17'
    $session.Send('cat(paste0("PIPE_", 1:3' + $esc + 'm')
    Start-Sleep -Milliseconds 100
    $session.Send(' sum(), "\n"))' + "`r")
    Wait-PrismText 'PIPE_6'

    $session.Resize(90, 32)
    Start-Sleep -Milliseconds 250
    $resizeOffset = $session.Snapshot().Length
    Send-PrismExpression 'cat(paste0("SIZE_", paste(prismrepl:::term_size(), collapse="x"), "\n"))'
    Wait-PrismText 'SIZE_90x32'
    $resized = $session.Snapshot().Substring($resizeOffset)
    foreach ($match in [regex]::Matches($resized, '\x1b\[(\d+);(\d+)H')) {
        if ([int]$match.Groups[1].Value -gt 32 -or [int]$match.Groups[2].Value -gt 90) {
            throw ('Cursor paint exceeded the resized viewport: ' + $match.Value)
        }
    }
    $raw = $session.Snapshot()
    if ($raw -notmatch '\x1b\[[0-9;]*38;2;') {
        throw 'No truecolor foreground sequence reached the ConPTY output stream.'
    }
    if (-not $raw.Contains("$esc[?1049h")) {
        throw 'The fullscreen console did not enter the alternate screen.'
    }
    Send-PrismExpression ':quit'
    Wait-PrismText 'WINDOWS_SESSION_RESTORE_OK'
    if (-not $session.WaitForExit(10000) -or $session.ExitCode -ne 0) {
        throw 'The R process did not exit successfully.'
    }
    $cursorControls = [regex]::Matches($session.Snapshot(),
        '\x1b\](12;[^\x07\x1b]*|112)(?:\x07|\x1b\\)')
    if (-not $cursorControls.Count -or $cursorControls[$cursorControls.Count - 1].Groups[1].Value -ne '112') {
        throw 'Leaving the REPL did not forward the cursor-color reset through ConPTY.'
    }
    Write-Output 'PASS: Windows ConPTY theme-aware cursor contrast and reset, buffered Unicode frames, mouse input, Ctrl+J, output lifetime, restoration, conditions, paste, completion, pipe, and resize.'
} finally {
    if ($session) {
        try {
            [IO.File]::WriteAllText([IO.Path]::GetFullPath($LogPath),
                                  $session.Snapshot(), [Text.UTF8Encoding]::new($false))
        } finally { $session.Dispose() }
    }
    foreach ($name in $oldEnvironment.Keys) {
        [Environment]::SetEnvironmentVariable($name, $oldEnvironment[$name], 'Process')
    }
    if (Test-Path -LiteralPath $temporary) { Remove-Item -LiteralPath $temporary -Recurse -Force }
}
