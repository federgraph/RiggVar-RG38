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


This is a Delphi FMX project which can be built with IDE Rio 10.3.3 CE.

While it is technically a Delphi cross platform project, it is intended for Windows only.

But it should compile for other targets.

## RiggVar

The RiggVar project started in 1995.
Special versions of it have been created for several platforms and published via the app store.
Published versions before 2020 were light versions that did not surface all of the original features.

The intention with this repository is to rescue some of the original work
and provide a basis for future development.

<a href="doc/images/RiggVar-RG38-01.png">*RG38 is a full featured Delphi version of the RiggVar project.*<br>
![RG38 screenshot](doc/images/RiggVar-RG38-01.png)</a>

> There is help text about the RiggVar project ( RG ) on the federgraph website.

## Graphics

Currently there are three implementations for the Delphi platform:

1. A *close to original* version of a 2D graph which is drawing to the canvas.
This one can show overlaid instances of the model; the current situation under load,
the relaxed situation, and a reference situation to compare with.
2. A new 2D graph implementation which is created by defining drawing elements in code, which in turn will draw to the canvas.
3. A real 3D graph.

These implementations are called RotaForm1, RotaForm2, and RotaForm3.
**RotaForm** was the name of the original standalone form where you could rotate the graph around 3 axis with the mouse.
Now you can rotate, pan and zoom with touch screen or with touch pad as well.

## Documentation Drawings

- The project includes documentation drawings.
- Button **FD** ( see picture above )will bring up the form with the documentation drawings.
- This form can be used as the main form in a standalone application, see RG76.dpr.
- In *TFormDrawing* you can select a drawing from a list and manipulate the parameters of drawing elements with the mouse wheel.

<a href="doc/images/RiggVar-FD-01.png">*Documentation drawings can serve as a unit test alternative.*<br>
![FormDrawing screenshot](doc/images/RiggVar-FD-01.png)</a>

Every drawing element has a caption attached so that you know what the name of the element is.
Captions of elements can be hidden and shown interactively.
The drawings can be used to produce screen shots for the documentation.
To add a new drawing is supposed to be easy.

## How to build

Use the following IDE versions to build the project:
- IDE 10.3.3 Rio CE
- IDE 10.2.3 Tokyo Pro

I have added the dproj file to gitignore so that you can have your own icons.

Before you open the project in the IDE you need to create a **new dproj file** (Delphi Project File):

1. Create a new FMX project in the default new project area.
1. Save the project as RG38
1. Copy the new dproj file to your RG38 working directory.
1. Open the project in the IDE and run in the debugger with **F9**.
1. Use same procedure to create RG76.dproj.

It may be necessary to use **Shift-F9** to *build again*.

## How to use a build of the app

The executable - RG38.exe - can be used standalone on any current Windows 10 machine.

There is no database and no files are saved by default.
The app can be run from a USB stick.

## Download from the Store

> The name of the Windows Store application is **Trimm420**.

The store entry for the app has a German language description.

## License

I have picked GPL 3.0 as a license because I mean it,
and because I want the application to have a long life.

The *library* code for the RiggVar Model (TRigg) shall always be GPL only,
and a complete application that you build with it should always have the same license.

- Since this is a complete application, it contains a number of reusable pieces of original code, besides the computation for the RiggVar model.
Next to the documentation drawings there is the button frame with the integrated touch bar elements, which could be regarded as a reusable piece.
I originally created the button frame feature for the federgraph application and I use it in many projects already.
I understand that it can now be used in your GPL project - since I have published it here - 
but it cannot be reused in your closed source project.
- I may publish the documentation drawing base classes in a dedicated repository.

## Contributions

Contributions are welcome. There is still a lot of work to do, for example:

- Change the text for the key terms and let me look at the changes.
- Provide better default data that is closer to reality.
- Build a lightweight version and test on other targets.
- Find a new use case and describe it.

The standard use case for the application is of educational type.
It shows how the 420 Rigg behaves in principle.
To work with concrete values for *your boat* requires more *work*.

> Do you want to adapt the project for the 470 class?

## About sibling projects at GitHub

Similar projects are available for VCL and LCL:

| Project | Platform | Note |
| :-- | :- | :- |
| RG10 | FMX | minimal UI version of RG |
| [RG19](https://github.com/federgraph/RiggVar-RG19) | VCL | original RiggVar project |
| RG38 | FMX | Trimm 420 full project |
| [RG51](https://github.com/federgraph/RiggVar-RG51) | LCL | FPC / Lazarus project using TBGRABitmap |
| RG76 | FMX | local documentation drawings project |
| [RG79](https://github.com/federgraph/documentation-drawings) | FMX | standalone documentation drawings repo |
