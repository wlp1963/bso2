# SPDX-License-Identifier: MIT
# Copyright (c) 2026 95west.us
# See LICENSE for full license text.

[CmdletBinding()]
param(
    [switch]$RegenerateMdHtml,
    [switch]$Quiet
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Write-Info {
    param([string]$Message)
    if (-not $Quiet) {
        Write-Host $Message
    }
}

function Resolve-EdgePath {
    $edgeCmd = Get-Command msedge.exe -ErrorAction SilentlyContinue
    if ($edgeCmd) {
        return $edgeCmd.Source
    }

    $candidates = @(
        'C:\Program Files\Microsoft\Edge\Application\msedge.exe',
        'C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe'
    )

    foreach ($candidate in $candidates) {
        if (Test-Path $candidate) {
            return $candidate
        }
    }

    throw 'Could not find msedge.exe. Install Microsoft Edge or add msedge.exe to PATH.'
}

function Resolve-PandocPath {
    $pandocCmd = Get-Command pandoc -ErrorAction SilentlyContinue
    if ($pandocCmd) {
        return $pandocCmd.Source
    }
    return $null
}

function Assert-FileExists {
    param([string]$PathToCheck, [string]$Label)
    if (-not (Test-Path $PathToCheck)) {
        throw "$Label is missing: $PathToCheck"
    }
}

function To-FileUri {
    param([string]$PathToConvert)
    $resolved = (Resolve-Path $PathToConvert).Path
    return ([System.Uri]$resolved).AbsoluteUri
}

function New-PdfWithEdge {
    param(
        [string]$EdgePath,
        [string]$InputPath,
        [string]$OutputPath
    )

    Assert-FileExists -PathToCheck $InputPath -Label 'Input document'
    $inputUri = To-FileUri -PathToConvert $InputPath

    Write-Info "PDF: $(Split-Path -Leaf $InputPath) -> $(Split-Path -Leaf $OutputPath)"
    & $EdgePath --headless --disable-gpu "--print-to-pdf=$OutputPath" $inputUri

    Assert-FileExists -PathToCheck $OutputPath -Label 'Generated PDF'
}

$scriptDir = Split-Path -Parent $PSCommandPath
$repoRoot = (Resolve-Path (Join-Path $scriptDir '..\..')).Path
$docsRef = Join-Path $repoRoot 'DOCS\reference'

$monitorHtml = Join-Path $docsRef 'monitor-usage.html'
$monitorPdf = Join-Path $docsRef 'monitor-usage.pdf'
$zeroPageMd = Join-Path $docsRef 'zero-page-usage.md'
$zeroPageHtml = Join-Path $docsRef 'zero-page-usage.html'
$zeroPagePdf = Join-Path $docsRef 'zero-page-usage.pdf'

Assert-FileExists -PathToCheck $monitorHtml -Label 'Monitor usage HTML'
Assert-FileExists -PathToCheck $zeroPageMd -Label 'Zero-page Markdown'

$edgePath = Resolve-EdgePath
Write-Info "Using Edge: $edgePath"

if ($RegenerateMdHtml) {
    $pandocPath = Resolve-PandocPath
    if ($pandocPath) {
        Write-Info 'HTML: zero-page-usage.md -> zero-page-usage.html'
        & $pandocPath $zeroPageMd -o $zeroPageHtml
    } else {
        Write-Info 'pandoc not found; skipping zero-page HTML regeneration.'
    }
}

New-PdfWithEdge -EdgePath $edgePath -InputPath $monitorHtml -OutputPath $monitorPdf

$zeroPagePdfInput = if (Test-Path $zeroPageHtml) { $zeroPageHtml } else { $zeroPageMd }
New-PdfWithEdge -EdgePath $edgePath -InputPath $zeroPagePdfInput -OutputPath $zeroPagePdf

Write-Info 'Docs regeneration complete.'
