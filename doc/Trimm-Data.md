﻿# Trimm Data

Trimm data comes as Trimm-Item text or Trimm-File text.

## Trimm-Item example

This is what a Trimm-Item looks like when you copy to the clipboard and paste into notepad:
```
DOCTYPE := Trimm-Item;
Namespace := http://www.riggvar.de/2015/rgg/trimm-item ;
Version := 1;

Name := ;

A0X := 2560;
A0Y := 765;
A0Z := 430;
C0X := 4140;
C0Y := 0;
C0Z := 340;
D0X := 2870;
D0Y := 0;
D0Z := -100;
E0X := 2970;
E0Y := 0;
E0Z := 450;
F0X := -30;
F0Y := 0;
F0Z := 300;
MU := 2600;
MO := 2000;
ML := 6115;
MV := 5010;
CA := 50;
h0 := 56;
h2 := 0;
l2 := 100;
CPMin := 50;
CPPos := 100;
CPMax := 200;
SHMin := 140;
SHPos := 220;
SHMax := 300;
SAMin := 780;
SAPos := 850;
SAMax := 1000;
SLMin := 450;
SLPos := 479;
SLMax := 600;
SWMin := 0;
SWPos := 27;
SWMax := 89;
VOMin := 4400;
VOPos := 4500;
VOMax := 4600;
WIMin := 85;
WIPos := 94;
WIMax := 105;
WLMin := 4050;
WLPos := 4120;
WLMax := 4200;
WOMin := 2000;
WOPos := 2020;
WOMax := 2070;
```

## Trimm-File example

This is what a Trimm-File looks like when you copy to the clipboard and paste into notepad:
```
DOCTYPE := Trimm-File;
Namespace := http://www.riggvar.de/2015/rgg/trimm-file ;
Version := 1;

//Basis-Trimm (Trimm 0)

A0X := 2560;
A0Y := 765;
A0Z := 430;
C0X := 4140;
C0Y := 0;
C0Z := 340;
D0X := 2870;
D0Y := 0;
D0Z := -100;
E0X := 2970;
E0Y := 0;
E0Z := 450;
F0X := -30;
F0Y := 0;
F0Z := 300;

MU := 2600;
MO := 2000;
ML := 6115;
MV := 5000;
CA := 50;

h0 := 56;
h2 := 0;
l2 := 100;

CPMin := 50;
CPPos := 100;
CPMax := 200;

SHMin := 140;
SHPos := 220;
SHMax := 300;

SAMin := 780;
SAPos := 850;
SAMax := 1000;

SLMin := 450;
SLPos := 479;
SLMax := 600;

SWMin := 0;
SWPos := 27;
SWMax := 89;

VOMin := 4400;
VOPos := 4500;
VOMax := 4600;

WIMin := 85;
WIPos := 95;
WIMax := 105;

WLMin := 4050;
WLPos := 4120;
WLMax := 4200;

WOMin := 2000;
WOPos := 2020;
WOMax := 2070;

//Trimm1
Name := T1;
MV := 5010;

//Trimm2
Name := T2;
WLPos := 4140;

//Trimm3
Name := T3;
VOPos := 4530;

//Trimm4
Name := T4;
SHPos := 260;

//Trimm5
Name := T5;
SAPos := 900;

//Trimm6
Name := T6;
VOPos := 4560;
WLPos := 4100;
```

Up to 7 Trimm-Items will be evaluated when the application reads a Trimm-File,
- an unnamed Trimm-Item T0
- up to 6 named Trimm-Items T1-T6

Note that it is not necessary to repeat unchanged data.
Omitted data will be taken from built-in default Trimm,
which you will get when you call Reset on a TRggData instance.

Since the example data was generated by the app it does not use relative data.
you can chose to specify relative values when you edit a Trimm-File manually
with an external editor or modify it programmatically in any way you like.

## Relative Values

It should be convenient for the users to specify the Trimm data for their boats.

Ok, I admit that it is currently *convenient* only if you know how.

When I say convenient, I mean two things:

- don't have to repeat unchanged values
- and (!) can specify relative values

```
//Trimm3
Name := T3;
//VOPos := 4530;
vo := 30; // 4500 + 30 = 4530
```

> Use small caps and no Pos for relative values.

In the snippet above, which is a possible replacement for Trimm3 in the Trimm-File example above,
I have used a relative value for VOPos, the current length of Vorstag (Forestay).

You can see this in the code below.

```pascal
{ from unit RiggVar.FB.DefConst }
const
  cCP = 'cp';
  cSH = 'sh';
  cSA = 'sa';
//  cSL = 'sl';
//  cSW = 'sw';
  cVO = 'vo';
  cWI = 'wi';
  cWL = 'wl';
  cWO = 'wo';

{ from unit RiggVar.RG.Data }
TRggData.Load(AML: TStrings)
var
  s: string;
begin
  //...
  s := AML.Values[cVO];
  VOPos := VOPos + StrToIntDef(s, 0);
  //...
end;
```

Relative means relative to the data already in the instance of the class.

Note that TRggData.Load is called with a StringList full of data.
You need to be aware of what TRggData instance is used to load the data,
which will become clear when you look at TRggData.ReadTrimmFile method,
the method which will also call Reset on the instance of TRggData.

> Default data is used as base for relative values.

T7 (420) is the default data.

> Make T0 contain the same data as T7 (default) to see what you are diffing against in the Trimm-File.