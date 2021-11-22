unit RiggVar.Util.AppUtils;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

type
  TAppUtils = class
  public
    class function GetProjectDir: string;
    class function GetFullExeName: string;
    class function GetFileNameWithoutExtension: string;
{$ifdef IOS}
    class function GetIOSDataDirectory: string;
{$endif}
{$ifdef Android}
    class function GetDocumentDirectory: string;
{$endif}
{$ifdef MACOS}
    class function GetAppDataDir: string;
    class function GetDocumentDirectory: string;
    class function GetUserPicturesDir: string;
{$endif}
{$ifdef MSWINDOWS}
    class function GetSystem32Dir: string;
    class function GetUserDocumentsDir: string;
    class function GetUserPicturesDir: string;
    class function GetRoamingDir: string;
    class function GetLocalDir: string;
    class function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
{$endif}
  end;

implementation

uses
{$ifdef IOS}
  System.IOUtils,
{$endif}
{$ifdef Android}
  System.IOUtils,
{$endif}
{$ifdef MSWINDOWS}
  Winapi.Windows,
  Winapi.SHFolder,
  Winapi.SHLObj,
{$endif}
  System.SysUtils,
  System.StrUtils,
  RiggVar.FB.Classes;

class function TAppUtils.GetProjectDir: string;
var
  p: Integer;
  dn: string;
begin
  result := '';
  dn := ExtractFileDir(ParamStr(0));
  dn := IncludeTrailingPathDelimiter(dn);
  if DirectoryExists(dn) then
  begin
{$ifdef Win32}
    p := Pos('Win32', dn);
{$endif}
{$ifdef Win64}
    p := Pos('Win64', dn);
{$endif}
{$ifdef MACOS}
    p := Pos('OSX32', dn);
{$endif}
{$ifdef Android}
    p := Pos('Android', dn);
{$endif}
    if p > 0 then
      result := LeftStr(dn, p - 1)
    else
      result := dn;
  end;
end;

class function TAppUtils.GetFullExeName: string;
begin
  result := ParamStr(0);
end;

class function TAppUtils.GetFileNameWithoutExtension: string;
begin
  result := LowerCase(ChangeFileExt(ExtractFileName(GetFullExeName), ''));
end;

{$ifdef MSWINDOWS}
class function TAppUtils.GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
var
  FilePath: array [0..255] of char;
begin
  SHGetSpecialFolderPath(0, @FilePath[0], FOLDER, CanCreate);
  Result := FilePath;
end;

class function TAppUtils.GetLocalDir: string;
begin
  result := GetSpecialFolderPath(CSIDL_APPDATA, True);
  result := IncludeTrailingPathDelimiter(result) + 'RiggVar\RG\';
  ForceDirectories(result);
end;

class function TAppUtils.GetRoamingDir: string;
begin
  result := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA, True);
  result := IncludeTrailingPathDelimiter(result) + 'RiggVar\RG\';
  ForceDirectories(result);
end;

class function TAppUtils.GetUserDocumentsDir: string;
begin
  result := GetSpecialFolderPath(CSIDL_PERSONAL, True);
  result := IncludeTrailingPathDelimiter(result);
  ForceDirectories(result);
end;

class function TAppUtils.GetUserPicturesDir: string;
begin
  result := GetSpecialFolderPath(CSIDL_MYPICTURES, True);
  result := IncludeTrailingPathDelimiter(result);
  ForceDirectories(result);
end;

class function TAppUtils.GetSystem32Dir: string;
begin
  result := GetSpecialFolderPath(CSIDL_SYSTEM, False);
end;

{$endif}

{$ifdef MACOS}

class function TAppUtils.GetDocumentDirectory: string;
begin
  result := GetHomePath + PathDelim + 'Documents' + PathDelim;
end;

class function TAppUtils.GetUserPicturesDir: string;
begin
  result := GetHomePath + PathDelim + 'Pictures' + PathDelim;
end;

class function TAppUtils.GetAppDataDir: string;
begin
  result := GetHomePath;
  result := IncludeTrailingPathDelimiter(result) + 'RiggVar/RG/';
  ForceDirectories(result);
end;
{$endif}

{$ifdef Android}
class function TAppUtils.GetDocumentDirectory: string;
begin
  result := TPath.GetDocumentsPath + PathDelim;
end;
{$endif}

{$ifdef IOS}
class function TAppUtils.GetIOSDataDirectory: string;
begin
  result := TPath.GetDocumentsPath + PathDelim;
end;
{$endif}

end.

