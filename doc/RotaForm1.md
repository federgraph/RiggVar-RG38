# RotaForm 1

RotaForm 1 is the default graphical display for the Rigg, Rgg, or RG model.

```pascal
  // see RggRota.pas in folder Graph
  TRotaForm1 = class(TInterfacedObject, IStrokeRigg)
  private
    //...
  end;
```
TRotaform1 implements a 2D graph.
but note that it can be rotated around 3 axis with the mouse.
The model has 3D coordinates, which will be transformed before drawing to the canvas.

The graph is drawn onto an Image.Bitmap.Canvas
and the image component will handle OnMouseDown, OnMouseMove, and OnMouseUp events.
The most important of these is OnMouseMove.

I am using a TOriginalImage component to support dragging the application from a high resolution monitor to a normal monitor.
This may be of interest to the developer.
But the purpose of this topic is to explain the toolbar buttons for RotaForm 1
because these need an explanation.

## Sofort Berechnen

Or Compute immediately.
There was a button on the tool bar of the original, legacy application.

It means that only part one is done in the computation inside the model code, but not part two.

Part one of the computation deals with kinematics only.
It is enough to support the basic display of the model.

Part two of the computation deals with forces and the relaxed position of the model.
You would not be able to see any difference in the graphical display.
Whether the force in the shrouds is bigger or smaller,
the graph does not give you a clue,
and it is therefore not necessary to do the expensive computation,
if the only purpose was to show the current position of the model in the graph.

If property Sofort Berechnen is et to False the computation is done much faster.
And of course it was important in 1995 to be fast when changing the current parameter by scrolling a scroll bar.

Only when Sofort Berechnen was false I would update the graph while scrolling.
If Sofort Berechnen was true I would only update the graph when scrolling ended.

Even today I don not use threading or tasks to compute in the background.
You may want to do that in the future,
but I am convinced that is a good thing that the project at this time does everything in the main thread.

You need to set Sofort Berechnen to true to see be able to see the relaxed position of the model in a graph,
or to see updated values for the relaxed coordinates in the textual reports.

## Super Buttons

There were other buttons on the original toolbar related to options.

You would set the options via the buttons and then get the desired result,
but only if the selected combination made sense.
While it is pretty straight forward to predict the outcome when you know what these options mean,
it may be confusing to user of the program who is initially not aware of any options at all.

This is why the super buttons are useful.
They are superimposed over the available option.
Super buttons are radio buttons, one of them will be down.
Each super button will select a predefined set of options, as explained below.

<a href="images/RiggVar-RG38-01.png">*super buttons gS-gN-gG-gB-gM-gD-gQ*<br>
![RG38 screenshot](images/RiggVar-RG38-01.png)</a>

Each graph can have its dedicated speed panel.
In the picture above you can see the panel used with RotaForm 1.

```pascal
type
  TActionSpeedBarRG03 = class(TActionSpeedBar)
  private
    ColorModeBtn: TSpeedButton;
    FontSizeBtn: TSpeedButton;

    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;

    BogenBtn: TSpeedButton;
    KoppelBtn: TSpeedButton;

    SimpleBtn: TSpeedButton;
    NormalBtn: TSpeedButton;
    GrauBtn: TSpeedButton;
    BlauBtn: TSpeedButton;
    MultiBtn: TSpeedButton;
    DisplayBtn: TSpeedButton;
    QuickBtn: TSpeedButton;

    LegendBtn: TSpeedButton;
    LineColorBtn: TSpeedButton;
```

### Super Simple

Sofort Berechnen is false in super simple mode, which is also fast.
The graph will show one positions of the model only.

### Super Normal

Sofort Berechnen will be set to true if you select the button.
Otherwise it is the same as super simple.

### Super Grau

Sofort Berechnen is true and two positions of the model will be shown in the graph.
The normal position und load, and the relaxed position of the model.

You can only see the relaxed position of the mast bends, meaning that the force in the shrouds are greater zero.

### Super Blau

Blau = Blue. The *memory position* is drawn with light blue Aqua color.

When you press the Memory button the program should update the reference position,
a set of current values for the model.
At this time the model and the reference are the same - and you cannot see the reference position.
You cannot see it because it is drawn first and therefor behind the current position.

The current position is drawn with lots of colors, the reference position is drawn in blue, or aqua.

So, it is BtnBlau that can be down or not, ok?
You know that now but will use Super Blau button to see the reference position.

Sofort Berechnen will also be set to true.
It makes sense but is not required.
You can see that in unit RiggVar.RG.Main.pas:

```pascal
procedure TRggMain.SetSuperRadio(const Value: TGraphRadio);
begin
  FGraphRadio := Value;
  case Value of
    gSimple:
    begin
      FSofortBerechnen := False;
      FBtnGrauDown := False;
      FBtnBlauDown := False;
    end;

    gNormal:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := False;
      FBtnBlauDown := False;
    end;

    gBlau:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := False;
      FBtnBlauDown := True;
    end;

    gGrau:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := True;
      FBtnBlauDown := False;
    end;

    gMulti:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := True;
      FBtnBlauDown := True;
    end;

    gDisplay:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := False;
      FBtnBlauDown := False;
    end;

    gQuick:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := False;
      FBtnBlauDown := False;
    end;

  end;

  // ...
end;
```

### Super Multi

Both true - Grau and Blau.
Do you know the German color values by now?
Good, very good!

### Super Display

This will give you an alternative way of drawing which uses display items which can be sorted,
so that the lines in front are drawn last.

RotaForm is a 2D graph.
For many years it was good enough as is.
Then I published an application to the store, the FMX application with a 3D graph,
some say that it looks better.

Then, years later, I wanted to update the original application and reintroduce some features,
like for example part two of the computation, and the 2D graph.

Then, month later, I thought that some sort of painters algorithm to improve the 3D impression,
to see what is in front, would be worth attempting.

I don't claim anything,
- whether this is worth the effort,
- whether it works in all situations,
- whether there is prove that is in theory possible at all or not,
- or whether it is fast enough to be usable,
but rather I will let you decide.

You can read about the painters algorithm on Wikipedia.
It is more like a concept than something that can reused easily.
Don't make a mistake, I may know more about the topic than you think,
e.g why it is not easy, but I am not a mathematician,
and therefore will prefer to stay silent.

### Super Quick

Super Quick is not actually quick, it is expected to be slow.
It means that the built in quick sort code is used to sort display items.
Using quick sort is a variation of how the elements are sorted. 
And I needed a button for this when testing.

Actually, I have done extra test project to be able to test it out properly.
I do not claim that the depth sorting is perfect or correct all the time.

### End of Super

You can have a look at the other repository, where the legacy VCL application code is located.
There you can find out more about the history of BtnBlau and BtnGrau, SofortBtn, and so on.

Rename refactoring is super easy today, but what should the better name be?

RotaForm 2, the new one, is also a 2D graph, and it reuses almost the same depth sorting of lines as with Quick,
but it does not show the relaxed position, or the reference position.
RotaForm 2 is leaner and easier to maintain than RotaForm 1.

RotaForm 3, the 3D graph that is expected to appear in folder Graph3 in the future,
is somehow even easier, since I do not have to create the drawing elements myself,
but the complexity there is just shifted towards other areas.

Good news is that you can have all of them in one application.
Special applications that might be derived for certain platforms will have to decide
- if they want to support the computation of forces,
- what kind of graph should be used
- how data will be stored.

The most interesting part is probably not the drawing itself but how input ins handled and used to change the view.
This differs between the views, but should perhaps be generalized, and optimized,
e.g an input throttle should perhaps be applied in a consistent way across the implementation of view.

I do not know yet where I will do the *documentation* of the project
whether I should use the wiki or not.

For now I have decided to add the two images for the top readme to doc/images,
and I think that I will do some readme files here in the doc folder,
because it exists.
This file is the first readme.

Some documentation is already on the website, and I may add some info here or there.
Perhaps there will be a real book, it depends on how the project is going.
Existing unpublished documentation about the rgg model itself may find its way into the book.
The illustrations for that book will be created using the drawing elements in folder Graph2.

> This place is for the code primarily, and for your readme files.

## Legend

There is a legend. It is in RotaForm 1. You can button LG, but only if the display list is used, selected with buttons gD and gQ. And by the way, button LC will change the color scheme used, for the individual display items.

If you select the Super Simple the legend button changes nothing, as expected!