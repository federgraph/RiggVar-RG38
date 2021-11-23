# How to recreate the dproj

You need to recreate the dproj file because it is in gitignore.

Do this for RG38 and other projects as needed.

## Option 1

Just open the project via the .dpr file.

Then you should restore the default output directories in the project options:

```
.\$(Platform)\$(Config)
```

The output dir defaults are lost when the dproj is recreated, that is why.

## Option 2

1. Create a new FMX project in the default new project area.
1. Save the project as RG38.
1. Copy the new dproj file to your RG38 working directory.
1. Open the project in the IDE and run in the debugger with **F9**.

It may be necessary to use **Shift-F9** to build successfully.

## How to fix output dirs

> This is best done manually in the project options dialog.

But I have tried to check it with a PowerShell script:

```ps1
param ([string]$ProjectName = $(throw "DprojPath parameter is required."))
$dproj = $ProjectName + ".dproj"
$DprojPath = Join-Path $PSScriptRoot $dproj

$condi = "'$" + "(Base)'!=''"
$ns = @{x = 'http://schemas.microsoft.com/developer/msbuild/2003'}
$propGroupPath = @"
  //x:Project/x:PropertyGroup[@Condition="$condi"]
"@
$exeOutputPath = @"
  //x:Project/x:PropertyGroup[@Condition="$condi"]/x:DCC_ExeOutput
"@
$dcuOutputPath = @"
  //x:Project/x:PropertyGroup[@Condition="$condi"]/x:DCC_DcuOutput
"@
$defaultValue =  ".\$" + "(Platform)" + "\$" + "(Config)"
 
$xml = New-Object -TypeName XML
$xml.Load($DprojPath)

$propGroup = Select-XML -Xml $xml -XPath $propGroupPath -Namespace $ns
if ($propGroup) {
  $needSave = $false;
  $exeOutput = Select-XML -Xml $xml -XPath $exeOutputPath -Namespace $ns
  if ($exeOutput) {
    Write-Host 'DCC_ExeOutput exists.' -ForegroundColor Green
  }
  else {
    Write-Host 'DCC_ExeOutput node not found.' -ForegroundColor Yellow
    $newNode = $xml.CreateElement('DCC_ExeOutput', $propGroup.Node.NamespaceURI)
    $newNode.InnerText = $defaultValue
    $newNode.RemoveAttribute("xmlns")
    $propGroup.Node.AppendChild($newNode)
    $needSave = $true;
  }

  $dcuOutput = Select-XML -Xml $xml -XPath $dcuOutputPath -Namespace $ns
  if ($dcuOutput) {
    Write-Host 'DCC_DcuOutput exists.' -ForegroundColor Green
  }
  else {
    Write-Host 'DCC_DcuOutput node not found.' -ForegroundColor Yellow
    $newNode = $xml.CreateElement('DCC_DcuOutput', $propGroup.Node.NamespaceURI)
    $newNode.InnerText = $defaultValue
    $newNode.RemoveAttribute("xmlns")
    $propGroup.Node.AppendChild($newNode)
    $needSave = $true;
  }

  if ($needSave) {
    Write-Host 'Saving xml.' -ForegroundColor Cyan
    $xml.Save($DprojPath)
  }
}
```

You could save the above PowerShell script as Edit-Dproj.ps1, open a PowerShell command prompt and run it with the project name as parameter.

```
PS D:\DelphiProjects\RG\RG38> .\Edit-Dproj.ps1 RG38
DCC_ExeOutput exists.
DCC_DcuOutput exists.
PS D:\DelphiProjects\RG\RG38>
```

## How to script a release build

This is an exercise.
I don't know the best way but I have tried something.
Note that Bat and ps1 script files are in gitignore.
Here I will document my attempt with the following snippets.

#### DoReleaseBuild.bat

```bat
@Echo off

powershell .\Remove-BeforeBuild.ps1

powershell .\DoReleaseBuild.ps1 -ProjectName RG10
powershell .\DoReleaseBuild.ps1 -ProjectName RG69
powershell .\DoReleaseBuild.ps1 -ProjectName RG76
powershell .\DoReleaseBuild.ps1 -ProjectName RG38
```

#### DoReleaseBuild.ps1

```ps1
param (
    [string]$ProjectName = $(throw "ProjectName parameter is required.")
)

$delphiBinDir = "c:\program files (x86)\embarcadero\studio\19.0\bin"
$delphiExePath = $delphiBinDir + "\bds.exe"

Write-Host "Release build for" $ProjectName -ForegroundColor Magenta

powershell -File .\Test-Dproj.ps1 -ProjectName $ProjectName -DelphiPath $delphiExePath
powershell -File .\Edit-Dproj.ps1 -ProjectName $ProjectName

$dproj = $ProjectName + ".dproj"
.\internal-build.bat $dproj "$delphiBinDir"

powershell .\Stop-Delphi.ps1
powershell .\Remove-AfterBuild.ps1

```

#### internal-build.bat

```ps1
:: param 1 = ProjectName, without extension
:: param 2 =  BinPath to Delphi, without TrailingPathDelimiter
@Echo off
Set pn=%~1
Set bp=%~2
call "%bp%\rsvars.bat"
msbuild %pn% /target:build /verbosity:m /p:DCC_BuildAllUnits=true /p:"Config=Release" /p:"Platform=Win32" /nologo
```

#### Remove-BeforeBuild.ps1

```
Remove-Item -Recurse -Force $PSScriptRoot\Win32\Release -ErrorAction Ignore

Get-Childitem . -Include __history -Recurse -Force | Remove-Item -Recurse -Force
Get-Childitem . -Include __recovery -Recurse -Force | Remove-Item -Recurse -Force
```

#### Remove-AfterBuild.ps1

```
Get-ChildItem * -Include *.dcu -Recurse | Remove-Item
```

#### Test-Dproj.ps1

```ps1
param (
    [string]$ProjectName = $(throw "ProjectName parameter is required."),
    [string]$DelphiPath = $(throw "DelphiPath parameter is required.")
)

$dproj = $ProjectName + ".dproj"
$dpr = $ProjectName + ".dpr"

Write-Host $ProjectName -ForegroundColor Cyan
Write-Host $DelphiPath -ForegroundColor Cyan
Write-Host $dproj -ForegroundColor Yellow
Write-Host $dpr -ForegroundColor Yellow

if (!(Test-Path $dproj)) {
    Write-Host "Dproj does not exist."
    if (Test-Path $dpr) {
        Write-Host "Dpr exists."
        $temp = Get-ChildItem -File $dpr
        # count of files found is expected to be one
        $c = ($temp | Measure-Object).Count
        if ($c -eq 1) {
            Write-Host "Dpr file is unique."

            $fn = $temp.FullName
            Write-Host "fn =" $fn

            Write-Host "DelphiPath =" $DelphiPath

            Write-Host "starting up Delphi IDE..."
            Start-Process -FilePath $DelphiPath -ArgumentList $fn

            Write-Host "waiting for dproj file to be written to disk..."
            while (!(Test-Path $dproj)) { Start-Sleep 10 }
        }
    }
    else {
        Write-Host "Dpr does not exist."
    }    
}
```

#### Stop-Delphi.ps1

```ps1
$DelphiProcessName = 'bds';
$proc = Get-Process $DelphiProcessName -ErrorAction SilentlyContinue

if ($proc) {
    $procId = $proc | Select-Object -expand id
    Write-Host "Process ID of Delphi IDE is " $procId

    if ($procid -gt 0) {
        $DelphiIDE = Get-Process -Id $procId
        if ($DelphiIDE) {
            Write-Host "Trying to close the IDE..."
            [void] $DelphiIDE.CloseMainWindow()
            if (!$DelphiIDE.HasExited) {
                Write-Host "Waiting for exit of Delphi IDE..."
                $DelphiIDE.WaitForExit(5000)
                Write-Host "Waiting has finished."
            }
        }
    }
    Write-Host "Script has finished".
}
```

#### Start-Delphi.ps1

```ps1
param (
    [string]$ProjectName = $(throw "ProjectName parameter is required."),
    [string]$DelphiPath = $(throw "DelphiPath parameter is required.")
)
$dproj = $ProjectName + ".dproj"

$temp = Get-ChildItem -File $dproj

# count of files found is expected to be one
$c = ($temp | Measure-Object).Count

if ($c -eq 1) {
    Write-Host "Dproj exists."
    $fn = $temp.FullName
    Write-Host "fn =" $fn
    Write-Host "DelphiPath =" $DelphiPath
    Write-Host "starting up Delphi IDE..."
    Start-Process -FilePath $DelphiPath -ArgumentList $fn
}
```