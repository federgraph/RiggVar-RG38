# RiggVar-RG38

```
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
```


This is a Delphi FMX project which can be built with IDE Rio 10.3.3.

While it is a technically a Delphi cross platform project, it is intended for Windows only.
It should compile for other platforms,
but mainly because of the additional forms,
which have been available previously for the VCL platform and reintroduced as FMX forms,
it is not expected to run on any other platform than Windows.

## RiggVar

The RiggVar project started in 1995.
Special versions of it have been created for several platforms and published via the app store,
but published versions were light versions that did not surface all of the original features.

While it is a good thing to have a lightweight version of the app,
and there will probably be more of them,
the intention with this repository is to rescue some of the original work,
and provide a basis for future development.

<a href="doc/images/RiggVar-RG38-01.png">*RG38 is a full featured Delphi version of the RiggVar project.*<br>
![RG38 screenshot](doc/images/RiggVar-RG38-01.png)</a>

Please read about the RiggVar project ( RG ) on the federgraph website.

## Graphics

As of now, the code behind should be the same for all versions,
but the graphical display of the model can be different.
Currently there are three implementations for the Delphi platform:

1. A *close to original* version of a 2D graph, done by drawing to the canvas.
This one can show multiple overlaid instances of the model; the current situation under load,
the relaxed situation, and a reference situation to compare with.
2. A new 2D graph implementation which is created by defining drawing elements in code, which in turn draw to the canvas.
3. A real 3D graph, not yet included in this repository.

The three implementations are called RotaForm1, RotaForm2, and RotaForm3.
**RotaForm** was the name of the original standalone form where you could rotate the graph around 3 axis with the mouse.
Now you can rotate, pan and zoom on the touch screen as well.
The touchpad should work as well.
When a variation of the drawing is done it should support input.

## Documentation Drawings

This project includes documentation drawings.

In *TFormDrawing* you can select a drawing from a list and manipulate the params of these drawing elements with the mouse wheel.
Every drawing element has a caption attached so that you know what the name of the element is.
Captions of elements can be hidden interactively.

The drawings could be used to produce screen shots,
but I think it is much better to use the **live instance**.

Documentation drawings are new, started only in 2020 because of a need.
To see these drawings in action may be the only reason why you may want to look at the project.
Button FD (see screenshot above) will bring up the form with the documentation drawings.

<a href="doc/images/RiggVar-FD-01.png">*Documentation drawings can serve as a unit test alternative.*<br>
![FormDrawing screenshot](doc/images/RiggVar-FD-01.png)</a>

With the code in folder Graph2 I have determined how the documentation should be done.
More drawings should be added.
It will be easier to explain how the model works once the drawings are in place.

## How to build

Use the Delphi IDE 10.3.3 to build the project.

I have added the dproj file to gitignore so that you can have you own Icons.
Before you open the project in the IDE you need to recreate a **new dproj file** (Delphi Project File):

- Create a new FMX project in the default new project area,
- save project as RG38,
- then copy just the dproj file over to the RG38 working directory.

Once you have added the new dproj file you should be able to open the project in the Delphi IDE and run in the debugger with F9.

It may be necessary to use Shift-F9 to build again after making changes to the project.

## How to use a build of the app

The built executable - RG38.exe - can be used standalone on any current Windows 10 machine.
There is no database, and no files are saved by default.
The app should be very easy to deploy, you can run it from a USB stick.

## Download from the Store

> The name of the Windows Store application is **Trimm420**

The store entry for the app has a German language description,
but much of the text in the application is in English.

## License

I have picked GPL 3.0 as a license, because I mean it,
and because I want the application to have a long life.

The *library* code for the RiggVar computations in folder Core shall always be GPL only,
and a complete application that you build with it should always have the same license.

I plan to publish the documentation drawing base classes in a standalone repository with dual license.

Since this is a complete application, it contains a number of reusable pieces of original code, besides the computation for the RiggVar model.
Next to the documentation drawings, there is also the button frame with the integrated touch bar elements, which could be regarded as a reusable piece.
I use the button frame component in most of my Delphi applications.

How you could use the button frame code in another application needs to be discussed.

## Contributions

Contributions are welcome. Lots of work to do.

- There is currently no localization, and I have not decided how that should be done.
You could fork the project, change the text for the key terms, and let me look at the changes.
- You could provide default data that is closer to reality.
- You could find a use case and describe it.
- You could build a lightweight version in a branch and test on another target.

Please tell me if a branch is needed.

The project is available for VCL and LCL as well too:
- The LCL project can be build in the Lazarus IDE on iMac.
- It should be working on a Raspberry Pi but I have not tried yet.
- The Lazarus project is currently a private repository on GitHub.
- A measuring device may be connected to a version of the application.

> Graphics and high resolution monitor support is currently *more advanced* on the FMX platform!
