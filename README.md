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

While it is technically a Delphi cross platform project, it is intended for Windows only.

It is not expected to run on any other platform than Windows because of the additional forms,
which have been available previously for the VCL platform, and reintroduced as FMX forms.

But it should compile for other targets.

## RiggVar

The RiggVar project started in 1995.
Special versions of it have been created for several platforms and published via the app store.
Published versions before 2020 were light versions that did not surface all of the original features.

While it is a good thing to have a lightweight version of the app - 
and there will probably be more of them - 
the intention with this repository is to rescue some of the original work,
and provide a basis for future development.

<a href="doc/images/RiggVar-RG38-01.png">*RG38 is a full featured Delphi version of the RiggVar project.*<br>
![RG38 screenshot](doc/images/RiggVar-RG38-01.png)</a>

Please read about the RiggVar project ( RG ) on the federgraph website.

## Graphics

As of now, the code behind should be the same for all versions
while the graphical display of the model can be different.
Currently there are three implementations for the Delphi platform:

1. A *close to original* version of a 2D graph which is drawing to the canvas.
This one can show overlaid instances of the model; the current situation under load,
the relaxed situation, and a reference situation to compare with.
2. A new 2D graph implementation which is created by defining drawing elements in code, which in turn will draw to the canvas.
3. A real 3D graph, not yet included in this repository.

The three implementations are called RotaForm1, RotaForm2, and RotaForm3.

**RotaForm** was the name of the original standalone form where you could rotate the graph around 3 axis with the mouse.
Now you can rotate, pan and zoom with touch screen or with touch pad as well.

## Documentation Drawings

This project includes documentation drawings.

In *TFormDrawing* you can select a drawing from a list and manipulate the parameters of the drawing elements with the mouse wheel.

Every drawing element has a caption attached so that you know what the name of the element is.
Captions of elements can be hidden and shown interactively.

The drawings can be used to produce screen shots for the documentation.
To add a new drawing is supposed to be easy.
More drawings should be added.
It will be easier to explain how the model works once the drawings are in place.

Button **FD** will bring up the form with the documentation drawings.

<a href="doc/images/RiggVar-FD-01.png">*Documentation drawings can serve as a unit test alternative.*<br>
![FormDrawing screenshot](doc/images/RiggVar-FD-01.png)</a>

## How to build

Use IDE 10.3.3 to build the project.

I have added the dproj file to gitignore so that you can have your own icons.

Before you open the project in the IDE you need to create a **new dproj file** (Delphi Project File):

- Create a new FMX project in the default new project area,
- save project as RG38,
- then copy just the dproj file over to the RG38 working directory.

Once you have added the new dproj file you should be able to open the project in the Delphi IDE and run in the debugger with F9.

It may be necessary to use Shift-F9 to build again after making changes to the project.

## How to use a build of the app

The executable - RG38.exe - can be used standalone on any current Windows 10 machine.
There is no database, and no files are saved by default.
The app should be very easy to deploy, it can be run from a USB stick.

## Download from the Store

> The name of the Windows Store application is **Trimm420**.

The store entry for the app has a German language description,
but much of the text in the application is in English.

## License

I have picked GPL 3.0 as a license because I mean it,
and because I want the application to have a long life.

The *library* code for the RiggVar computations in folder Core shall always be GPL only,
and a complete application that you build with it should always have the same license.

( Since this is a complete application, it contains a number of reusable pieces of original code, besides the computation for the RiggVar model.
Next to the documentation drawings there is the button frame with the integrated touch bar elements, which could be regarded as a reusable piece.
I originally created the button frame feature for the federgraph application and I use it in many projects already.
I understand that it can now be used in your GPL project - since I have published it here - 
but it cannot be reused in your closed source project without asking. )

I plan to publish the documentation drawing base classes in a dedicated repository.

## Contributions

Contributions are welcome. Lots of work to do.

There is currently no localization, and I have not decided how that should be done.

- You could fork the project, change the text for the key terms, and let me look at the changes.
- You could provide default data that is closer to reality.
- You could build a lightweight version in a branch and test on another target.
- You could find a (new) use case and describe it.

The standard use case for the applications is of educational type.
It shows how the rig behaves in principle.
To work with concrete values for your boat requires more *work*.
I do not have easy access to a boat any more and have focused on the programming aspect of the project
because I found that interesting in itself.

Adapt the project for the 470 class in Trimm 470?
Please tell me if a branch is needed.

## About the sibling projects

A similar project is available for VCL and LCL:
- The LCL project can be built in the Lazarus IDE on iMac.
- It is currently in a private repository on GitHub.
- There should be a Raspberry Pi version.
- A measuring device may be connected to a version of the application?

> Graphics and high resolution monitor support is currently *more advanced* on the FMX platform.

Projects for other environments, namely for java and C#, exist but are not up to date.
A typescript version exists - of the basics.
A swift version was started but abandoned - I would be interested.

In 2020 I have updated and advanced the Pascal projects, as you can see here.

At some time in the future I may jump to a now very popular platform and do another light weight version using the latest free tools.
If you start such a project and layout a UI with a graph, I may have a jump target.