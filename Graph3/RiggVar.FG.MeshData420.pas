unit RiggVar.FG.MeshData420;

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

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  RiggVar.FG.MeshDataBase;

type
  THullMeshData = class(TMeshDataBase)
  private
    Test: Boolean;
    ABC: Boolean;

    WantSpiegel: Boolean;
    WantDeck: Boolean;
    Mode: Integer;

    CV: Integer;
    CT: Integer;
    CountV: Integer;
    CountT: Integer;

    procedure Dreieck(i, j, k: Integer);

    procedure GetPoints;
    procedure GetTriangles;

    procedure GetPoints1;
    procedure GetTriangles1;

    procedure Punkt(x, y, z: single);
    procedure Start;
    procedure Stop;
    procedure InspectVertexData;
  public
    InitOK: Boolean;
    constructor Create;
    procedure InitMesh; override;
  end;

implementation

uses
  RiggVar.App.Main;

{ THullMesh }

constructor THullMeshData.Create;
begin
  inherited;
  Test := False;
  WantSpiegel := False;
  WantDeck := False;
end;

procedure THullMeshData.Dreieck(i, j, k: Integer);
begin
  if ABC then
    case Mode of
      1: begin
        Inc(CountT);
      end;
      2: begin
        T1[CT] := i-1;
        if Inside then
        begin
          T2[CT] := k-1;
          T3[CT] := j-1;
        end
        else
        begin
          T2[CT] := j-1;
          T3[CT] := k-1;
        end;
        Inc(CT);
      end;
    end;
end;

procedure THullMeshData.Punkt(x, y, z: single);
var
  translateX: single;
  dowsizeAll: single;
  scaleZ: single;
begin
  if Test then
  begin
    translateX := 0.0;
    dowsizeAll := 1.0;
    scaleZ := 1.0;
  end
  else
  begin
    translateX := 0.0;
    dowsizeAll := 1.0;
    scaleZ := 1.0;
  end;

  case Mode of
    1: begin
      Inc(CountV);
    end;
    2: begin
      G[CV].X := x / dowsizeAll - translateX;
      G[CV].Y := y / dowsizeAll;
      G[CV].Z := scaleZ * z / dowsizeAll;
      Inc(CV);
    end;
  end;
end;

procedure THullMeshData.Start;
begin
  ABC := True;
end;

procedure THullMeshData.Stop;
begin
  ABC := False;
end;

procedure THullMeshData.InspectVertexData;
var
  i: Integer;
begin
  xmin := 0;
  xmax := 0;
  ymin := 0;
  ymax := 0;
  zmin := 0;
  zmax := 0;

  for i := 0 to CountV-1 do
  begin
    if G[i].x < xmin then
      xmin := G[i].x;
    if G[i].x > xmax then
      xmax := G[i].x;

    if G[i].y < ymin then
      ymin := G[i].y;
    if G[i].y > ymax then
      ymax := G[i].y;

    if G[i].z < zmin then
      zmin := G[i].z;
    if G[i].z > zmax then
      zmax := G[i].z;
  end;
end;

procedure THullMeshData.GetPoints;
begin
  Punkt(4200, 0, 328);
  Punkt(4194, 0, 260);
  Punkt(4188, 0, 195);
  Punkt(4178, 0, 128);
  Punkt(4168, 0, 78);
  Punkt(4151, 0, 26);
  Punkt(4130, 0, 0);

  Punkt(4100, -157, 325);
  Punkt(4100, -149, 268);
  Punkt(4100, -126, 189);
  Punkt(4100, -100, 131);
  Punkt(4100, -69, 74);
  Punkt(4100, -30, 8);
  Punkt(4100, 0, -48);
  Punkt(4100, 30, 8);
  Punkt(4100, 69, 74);
  Punkt(4100, 100, 131);
  Punkt(4100, 126, 189);
  Punkt(4100, 149, 268);
  Punkt(4100, 157, 325);

  Punkt(4000, -244, 322);
  Punkt(4000, -237, 263);
  Punkt(4000, -219, 186);
  Punkt(4000, -193, 115);
  Punkt(4000, -159, 51);
  Punkt(4000, -88, -41);
  Punkt(4000, 0, -117);
  Punkt(4000, 88, -41);
  Punkt(4000, 159, 51);
  Punkt(4000, 193, 115);
  Punkt(4000, 219, 186);
  Punkt(4000, 237, 263);
  Punkt(4000, 244, 322);

  Punkt(3750, -402, 315);
  Punkt(3750, -387, 263);
  Punkt(3750, -374, 176);
  Punkt(3750, -345, 96);
  Punkt(3750, -281, 7);
  Punkt(3750, -155, -93);
  Punkt(3750, 0, -178);
  Punkt(3750, 155, -93);
  Punkt(3750, 281, 7);
  Punkt(3750, 345, 96);
  Punkt(3750, 374, 176);
  Punkt(3750, 387, 263);
  Punkt(3750, 402, 315);

  Punkt(3400, -570, 308);
  Punkt(3400, -541, 253);
  Punkt(3400, -506, 166);
  Punkt(3400, -445, 42);
  Punkt(3400, -380, -30);
  Punkt(3400, -212, -126);
  Punkt(3400, 0, -202);
  Punkt(3400, 212, -126);
  Punkt(3400, 380, -30);
  Punkt(3400, 445, 42);
  Punkt(3400, 506, 166);
  Punkt(3400, 541, 253);
  Punkt(3400, 570, 308);

  Punkt(3000, -699, 302);
  Punkt(3000, -661, 248);
  Punkt(3000, -619, 163);
  Punkt(3000, -539, 26);
  Punkt(3000, -446, -54);
  Punkt(3000, -251, -138);
  Punkt(3000, 0, -205);
  Punkt(3000, 251, -138);
  Punkt(3000, 446, -54);
  Punkt(3000, 539, 26);
  Punkt(3000, 619, 163);
  Punkt(3000, 661, 248);
  Punkt(3000, 699, 302);

  Punkt(2400, -793, 297);
  Punkt(2400, -749, 245);
  Punkt(2400, -716, 167);
  Punkt(2400, -634, 31);
  Punkt(2400, -500, -65);
  Punkt(2400, -296, -135);
  Punkt(2400, 0, -191);
  Punkt(2400, 296, -135);
  Punkt(2400, 500, -65);
  Punkt(2400, 634, 31);
  Punkt(2400, 716, 167);
  Punkt(2400, 749, 245);
  Punkt(2400, 793, 297);

  Punkt(1800, -800, 290);
  Punkt(1800, -755, 241);
  Punkt(1800, -725, 173);
  Punkt(1800, -634, 34);
  Punkt(1800, -480, -58);
  Punkt(1800, -269, -115);
  Punkt(1800, 0, -161);
  Punkt(1800, 269, -115);
  Punkt(1800, 480, -58);
  Punkt(1800, 634, 34);
  Punkt(1800, 725, 173);
  Punkt(1800, 755, 241);
  Punkt(1800, 800, 290);

  Punkt(1000, -730, 275);
  Punkt(1000, -696, 237);
  Punkt(1000, -674, 185);
  Punkt(1000, -585, 56);
  Punkt(1000, -414, -23);
  Punkt(1000, -214, -64);
  Punkt(1000, 0, -97);
  Punkt(1000, 214, -64);
  Punkt(1000, 414, -23);
  Punkt(1000, 585, 56);
  Punkt(1000, 674, 185);
  Punkt(1000, 696, 237);
  Punkt(1000, 730, 275);

  Punkt(0, -580, 250);
  Punkt(0, -568, 226);
  Punkt(0, -560, 187);
  Punkt(0, -485, 89);
  Punkt(0, -300, 30);
  Punkt(0, -167, 13);
  Punkt(0, 0, 0);
  Punkt(0, 167, 13);
  Punkt(0, 300, 30);
  Punkt(0, 485, 89);
  Punkt(0, 560, 187);
  Punkt(0, 568, 226);
  Punkt(0, 580, 250);
end;

procedure THullMeshData.GetTriangles;
begin
  Start;

  Dreieck(1, 2, 8);
  Dreieck(8, 2, 9);
  Dreieck(2, 3, 9);
  Dreieck(9, 3, 10);
  Dreieck(3, 4, 10);
  Dreieck(10, 4, 11);
  Dreieck(4, 5, 11);
  Dreieck(11, 5, 12);
  Dreieck(5, 6, 12);
  Dreieck(12, 6, 13);
  Dreieck(6, 7, 13);
  Dreieck(13, 7, 14);

  Dreieck(2, 1, 20);
  Dreieck(20, 19, 2);
  Dreieck(3, 2, 19);
  Dreieck(19, 18, 3);
  Dreieck(4, 3, 18);
  Dreieck(18, 17, 4);
  Dreieck(5, 4, 17);
  Dreieck(17, 16, 5);
  Dreieck(6, 5, 16);
  Dreieck(16, 15, 6);
  Dreieck(7, 6, 15);
  Dreieck(15, 14, 7);

  Dreieck(8, 9, 21);
  Dreieck(21, 9, 22);
  Dreieck(9, 10, 22);
  Dreieck(22, 10, 23);
  Dreieck(10, 11, 23);
  Dreieck(23, 11, 24);
  Dreieck(11, 12, 24);
  Dreieck(24, 12, 25);
  Dreieck(12, 13, 25);
  Dreieck(25, 13, 26);
  Dreieck(13, 14, 26);
  Dreieck(26, 14, 27);

  Dreieck(14, 15, 28);
  Dreieck(28, 27, 14);
  Dreieck(15, 16, 29);
  Dreieck(29, 28, 15);
  Dreieck(16, 17, 30);
  Dreieck(30, 29, 16);
  Dreieck(17, 18, 31);
  Dreieck(31, 30, 17);
  Dreieck(18, 19, 32);
  Dreieck(32, 31, 18);
  Dreieck(19, 20, 33);
  Dreieck(33, 32, 19);

  Dreieck(21, 22, 34);
  Dreieck(34, 22, 35);
  Dreieck(22, 23, 35);
  Dreieck(35, 23, 36);
  Dreieck(23, 24, 36);
  Dreieck(36, 24, 37);
  Dreieck(24, 25, 37);
  Dreieck(37, 25, 38);
  Dreieck(25, 26, 38);
  Dreieck(38, 26, 39);
  Dreieck(26, 27, 39);
  Dreieck(39, 27, 40);

  Dreieck(27, 28, 41);
  Dreieck(41, 40, 27);
  Dreieck(28, 29, 42);
  Dreieck(42, 41, 28);
  Dreieck(29, 30, 43);
  Dreieck(43, 42, 29);
  Dreieck(30, 31, 44);
  Dreieck(44, 43, 30);
  Dreieck(31, 32, 45);
  Dreieck(45, 44, 31);
  Dreieck(32, 33, 46);
  Dreieck(46, 45, 32);

  Dreieck(34, 35, 47);
  Dreieck(47, 35, 48);
  Dreieck(35, 36, 48);
  Dreieck(48, 36, 49);
  Dreieck(36, 37, 49);
  Dreieck(49, 37, 50);
  Dreieck(37, 38, 50);
  Dreieck(50, 38, 51);
  Dreieck(38, 39, 51);
  Dreieck(51, 39, 52);
  Dreieck(39, 40, 52);
  Dreieck(52, 40, 53);

  Dreieck(40, 41, 54);
  Dreieck(54, 53, 40);
  Dreieck(41, 42, 55);
  Dreieck(55, 54, 41);
  Dreieck(42, 43, 56);
  Dreieck(56, 55, 42);
  Dreieck(43, 44, 57);
  Dreieck(57, 56, 43);
  Dreieck(44, 45, 58);
  Dreieck(58, 57, 44);
  Dreieck(45, 46, 59);
  Dreieck(59, 58, 45);

  Dreieck(47, 48, 60);
  Dreieck(60, 48, 61);
  Dreieck(48, 49, 61);
  Dreieck(61, 49, 62);
  Dreieck(49, 50, 62);
  Dreieck(62, 50, 63);
  Dreieck(50, 51, 63);
  Dreieck(63, 51, 64);
  Dreieck(51, 52, 64);
  Dreieck(64, 52, 65);
  Dreieck(52, 53, 65);
  Dreieck(65, 53, 66);

  Dreieck(53, 54, 67);
  Dreieck(67, 66, 53);
  Dreieck(54, 55, 68);
  Dreieck(68, 67, 54);
  Dreieck(55, 56, 69);
  Dreieck(69, 68, 55);
  Dreieck(56, 57, 70);
  Dreieck(70, 69, 56);
  Dreieck(57, 58, 71);
  Dreieck(71, 70, 57);
  Dreieck(58, 59, 72);
  Dreieck(72, 71, 58);

  Dreieck(60, 61, 73);
  Dreieck(73, 61, 74);
  Dreieck(61, 62, 74);
  Dreieck(74, 62, 75);
  Dreieck(62, 63, 75);
  Dreieck(75, 63, 76);
  Dreieck(63, 64, 76);
  Dreieck(76, 64, 77);
  Dreieck(64, 65, 77);
  Dreieck(77, 65, 78);
  Dreieck(65, 66, 78);
  Dreieck(78, 66, 79);

  Dreieck(66, 67, 80);
  Dreieck(80, 79, 66);
  Dreieck(67, 68, 81);
  Dreieck(81, 80, 67);
  Dreieck(68, 69, 82);
  Dreieck(82, 81, 68);
  Dreieck(69, 70, 83);
  Dreieck(83, 82, 69);
  Dreieck(70, 71, 84);
  Dreieck(84, 83, 70);
  Dreieck(71, 72, 85);
  Dreieck(85, 84, 71);

  Dreieck(73, 74, 86);
  Dreieck(86, 74, 87);
  Dreieck(74, 75, 87);
  Dreieck(87, 75, 88);
  Dreieck(75, 76, 88);
  Dreieck(88, 76, 89);
  Dreieck(76, 77, 89);
  Dreieck(89, 77, 90);
  Dreieck(77, 78, 90);
  Dreieck(90, 78, 91);
  Dreieck(78, 79, 91);
  Dreieck(91, 79, 92);

  Dreieck(79, 80, 93);
  Dreieck(93, 92, 79);
  Dreieck(80, 81, 94);
  Dreieck(94, 93, 80);
  Dreieck(81, 82, 95);
  Dreieck(95, 94, 81);
  Dreieck(82, 83, 96);
  Dreieck(96, 95, 82);
  Dreieck(83, 84, 97);
  Dreieck(97, 96, 83);
  Dreieck(84, 85, 98);
  Dreieck(98, 97, 84);

  Dreieck(86, 87, 99);
  Dreieck(99, 87, 100);
  Dreieck(87, 88, 100);
  Dreieck(100, 88, 101);
  Dreieck(88, 89, 101);
  Dreieck(101, 89, 102);
  Dreieck(89, 90, 102);
  Dreieck(102, 90, 103);
  Dreieck(90, 91, 103);
  Dreieck(103, 91, 104);
  Dreieck(91, 92, 104);
  Dreieck(104, 92, 105);

  Dreieck(92, 93, 106);
  Dreieck(106, 105, 92);
  Dreieck(93, 94, 107);
  Dreieck(107, 106, 93);
  Dreieck(94, 95, 108);
  Dreieck(108, 107, 94);
  Dreieck(95, 96, 109);
  Dreieck(109, 108, 95);
  Dreieck(96, 97, 110);
  Dreieck(110, 109, 96);
  Dreieck(97, 98, 111);
  Dreieck(111, 110, 97);

  Dreieck(99, 100, 112);
  Dreieck(112, 100, 113);
  Dreieck(100, 101, 113);
  Dreieck(113, 101, 114);
  Dreieck(101, 102, 114);
  Dreieck(114, 102, 115);
  Dreieck(102, 103, 115);
  Dreieck(115, 103, 116);
  Dreieck(103, 104, 116);
  Dreieck(116, 104, 117);
  Dreieck(104, 105, 117);
  Dreieck(117, 105, 118);

  Dreieck(105, 106, 119);
  Dreieck(119, 118, 105);
  Dreieck(106, 107, 120);
  Dreieck(120, 119, 106);
  Dreieck(107, 108, 121);
  Dreieck(121, 120, 107);
  Dreieck(108, 109, 122);
  Dreieck(122, 121, 108);
  Dreieck(109, 110, 123);
  Dreieck(123, 122, 109);
  Dreieck(110, 111, 124);
  Dreieck(124, 123, 110);

  if WantDeck then
  begin
    Dreieck(8, 20, 1); // Deck
    Dreieck(20, 8, 21);
    Dreieck(21, 33, 20);
    Dreieck(33, 21, 34);
    Dreieck(34, 46, 33);
    Dreieck(46, 34, 47);
    Dreieck(47, 59, 46);
    Dreieck(59, 47, 60);
    Dreieck(60, 72, 59);
    Dreieck(72, 60, 73);
    Dreieck(73, 85, 72);
    Dreieck(85, 73, 86);
    Dreieck(86, 98, 85);
    Dreieck(98, 86, 99);
    Dreieck(99, 111, 98);
    Dreieck(111, 99, 112);
    Dreieck(112, 124, 111);
  end;

  if WantSpiegel then
  begin
  Dreieck(125, 123, 124); // Spiegel
  Dreieck(125, 122, 123);
  Dreieck(125, 121, 122);
  Dreieck(125, 120, 121);
  Dreieck(125, 119, 120);
  Dreieck(125, 118, 119);
  Dreieck(125, 117, 118);
  Dreieck(125, 116, 117);
  Dreieck(125, 115, 116);
  Dreieck(125, 114, 115);
  Dreieck(125, 113, 114);
  Dreieck(125, 112, 113);
  end;

  Stop;
end;

procedure THullMeshData.GetPoints1;
begin
  Punkt(-0.103, -0.201, 0);
  Punkt(-1.107, -1.023, 0);
  Punkt( 1.025, -1.078, 0);
  Punkt( 0.012,  1.041, 0);
  Punkt( 1.010,  1.037, 0);
end;

procedure THullMeshData.GetTriangles1;
begin
  Start;
  Dreieck(1, 2, 3);
  Dreieck(3, 4, 5);
  Stop;
end;

procedure THullMeshData.InitMesh;
var
  t: Integer;
begin
  Mode := 1;
  CV := 0;
  CT := 0;
  CountV := 0;
  CountT := 0;
  GetPoints;
  GetTriangles;

  SetLength(G, CountV);
  SetLength(T1, CountT);
  SetLength(T2, CountT);
  SetLength(T3, CountT);

  for t := 0 to CountV-1 do
  begin
    G[t].x := 0;
    G[t].y := 0;
    G[t].z := 0;
  end;

  Mode := 2;
  CV := 0;
  CT := 0;

  if Test then
  begin
    GetPoints1;
    GetTriangles1;
  end
  else
  begin
    GetPoints;
    GetTriangles;
  end;

  InspectVertexData;

  VertexCount := CountV;
  TriangleCount := CountT;
end;

end.
