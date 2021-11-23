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

This is best done manually in the project options dialog.

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
}```