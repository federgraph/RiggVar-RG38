# RotaForm 1

RotaForm 1 is the default graphical display for the Rigg, Rgg, or RG model.

```pascal
  // see RggRota.pas in folder Graph
  TRotaForm1 = class(TInterfacedObject, IStrokeRigg)
  private
    //...
  end;
```
TRotaform1 implements a 2D graph - but note that it can be rotated around 3 axis with the mouse.
The model has 3D coordinates, which will be transformed before drawing to the canvas.

The graph is drawn onto Image.Bitmap.Canvas
and the Image component will handle OnMouseDown, OnMouseMove, and OnMouseUp events.

I am using a special *TOriginalImage* component to support dragging the application from a high resolution monitor to a normal monitor.
This may be of interest to the developer.

But the main purpose of this topic is to explain the toolbar buttons for **RotaForm 1**.

## Sofort Berechnen

Alias *Compute immediately*.
There was a corresponding button on the tool bar of the original, legacy application.

By default the option used to be off.
It means that only *part one* of the computation is carried out, inside the model code, but not *part two*.

**Part one** of the computation deals with kinematics only.
It is all you need to support a basic display of the model.

**Part two** of the computation deals with forces and the computes the relaxed position of the model.
You would not be able to see a difference in the graphical display - whether the force in the shrouds is bigger or smaller,
the graph does not give a clue.
And it is therefore not necessary to do an expensive computation,
if the only purpose is to show the current position of the model, in the graph.

With property *Sofort Berechnen* set to False the computation completes much faster.
And of course it was important in 1995 to be fast when changing the current parameter of the model - by scrolling a scroll bar.

Depending on the value of Sofort Berechnen I would update the graph while scrolling, or only when scrolling ended.

Even as of today I do not use a background task to update the model.
May be in the future, but currently I am convinced that is a good idea that the project does everything in the main thread.

Sofort Berechnen needs to be True to see the relaxed position of the model in the graph,
and/or to see updated values for the relaxed position coordinates in the textual reports.

## Super Buttons

There were other buttons on the original toolbar related to options.

I would set model and view options via the speed buttons and get the desired result,
because I could remember the meaning and the buttons had glyphs.

> Legacy screen shot should appear here?

Now that I have decided to not bother about glyphs any more and go with short captions instead,
it may be a little confusing to users who are not aware of the meaning of the options.

This is why the super buttons are useful.
They are *superimposed* over the available options.
Super buttons are like *radio buttons*, one of them can be down at a time.
Each super button will select a predefined set of options, as explained below.
They are called super because I needed to give a unique name and short caption to a set of new actions with the same purpose.

<a href="images/RiggVar-RG38-01.png">*super buttons gS gN gG gB gM gD and gQ are on the speed panel*<br>
![RG38 screenshot](images/RiggVar-RG38-01.png)</a>

In the picture above you can see the speed panel used with RotaForm 1.
Each graph can have its dedicated speed panel.

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

Sofort Berechnen will be set to true.
Otherwise it is the same as super simple.
This mode is considered the normal mode of operation,
unless you want to be super fast and only need part one of the computation.

### Super Grau

Sofort Berechnen is true and two positions of the model will be shown - 
The normal position and the relaxed position of the model.

You can see the relaxed position only if the mast bends, when the force in the shrouds are greater zero.

### Super Blau

Blau = Blue. The *memory position* used to be drawn with a light blue color known as Aqua.

When you press the Memory button **M** the program should update the reference position,
which is a set of current values for the model.
At this time the model and the reference are the same - and you cannot see the reference position
because it is drawn first, and therefore hides behind the current position.

When you then change the model the reference position will become visible.

So, it is BtnBlau that can be down or not.
You know about its existence but will use Super Blau instead.
It will ensure that Sofort Berechnen will also be set.

Now it is time to inspect the relevant code in unit RiggVar.RG.Main.pas:

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

True both - Grau and Blau.
Do you know the German color values by now?

### Super Display

This will give you an alternative way of drawing, which uses *display items* that can be sorted,
so that lines in front are drawn last.

- RotaForm 1 is a 2D graph.
For many years the original version of it was good enough.
- Then I published an application to the store - the FMX application -
featuring a 3D graph which some say looks better.
- Then, years later, I wanted to update the application and reintroduce features,
like for example part two of the computation, and the 2D graph which can show the result.
- And then, month later, I thought that some sort of a *painters algorithm* to improve the 3D impression would be appropriate,
to see what is in front.

I don't claim anything,
- whether this is worth the effort,
- whether it works in all situations,
- whether there is prove that is in theory possible at all or not,
- or whether it is fast enough to be usable,

but rather I will let you decide which you want to keep.

You can read about the *painters algorithm* on Wikipedia,
it is more like a concept than something that can be reused easily.
I know more about the topic now, e.g why it is not easy, but I am not a mathematician
and therefore will prefer not to talk too much about.

### Super Quick

Super Quick is not so quick actually, it is expected to be slow!
It just means that the built in quick sort code is used to sort display items.
Sorting the drawing elements is slow compared to not sorting them.

Using quick sort with a special comparer is the more general variation of how the elements get sorted. 
I needed a button for this when testing.
Eventually I guess we will probably keep only one button, Super Display or Super Quick.

I have done extra test projects to be able to test out the display list approach.
I recognize that will be difficult to to proper testing within this project.

### End of Super

You could have a look at the other repository, where the legacy VCL application code is located.
There you can find out more about the history of BtnBlau and BtnGrau, SofortBtn, and so on.

> Rename refactoring is super easy today, but what should the better names be?

RotaForm 2, the new one, is also a 2D graph, and it reuses the depth sorting of lines as with Super Quick,
but it does not show the relaxed position, or the reference position.
RotaForm 2 is leaner, and easier to maintain than RotaForm 1.
I would rather want you to have a look at RotaForm 2 and improve that.

RotaForm 3 - the 3D graph that is expected to appear in folder Graph3 in the future -
is somehow even easier, since I can use existing components for the elements,
but note that the complexity there just shifted towards other areas,
like use of orthographic perspective, and persisting rotations.

Good news is that tis project allows you to use all graph implementations side by side, via the interface.

Special applications that may be derived for certain platforms from this one will have to decide
- whether they want to support the computation of forces,
- what kind of graph should be used
- how data will be stored.

The most interesting part of RotaForm 1 is probably not the drawing itself but how input is handled to change the view.
This differs between the views, but should perhaps be generalized and optimized.
For example, an input throttle could be applied in a consistent way across all implementation of view.
The mouse wheel not a notched one any more, if you receive mouse move messages via touch screen or touch pad.
In this situation, especially on a mobile device, a more robust solution is anticipated to be needed.

## Readme startup notes

This file is the first readme file in folder doc.

I do not know yet where I will do the *documentation* of the project,
whether we should prefer the wiki or not.

For now I have decided to add two images for the top README.md to a folder doc/images,
and I think that I will put some readme files in the doc folder, because it exists.

Some documentation is already on the website, and I may add some info here or there.

Perhaps there will be a real book, depending on how the project is going?
Existing unpublished documentation about the RG model itself may find its way into such a book.
The illustrations for that book will be created using the drawing elements in folder Graph2.

> This place is for the code primarily, and for your readme files.

## Legend

There is a legend in RotaForm 1.

You can use button **LG** to see it while *display list* drawing is active.

Button **LC** will change the color scheme used for the individual display items.

( If you select the Super Simple mode of the graph,
then the legend button and line color button change nothing,
since the display list is not used. )
