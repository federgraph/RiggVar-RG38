# Minimal App RG03

The minimal app is minimalistic in terms of the provided UI:
- One Combobox to select the current parameter.
- One Trackbar to change the value of the current parameter.
- One Button to reset the tumb of the trackbar to the center.
- One Text component to show minimal status info.
- One Image component to hold the graphical display.

<a href="images/RiggVar-RG03-01.png">*Minimal RG application RG03*<br>
![Minimal RG app RG03](images/RiggVar-RG03-01.png)</a>

## You need to configure in Project Options

To compile you need to set a *conditional define* (MinimalRG) in project options.

After the dproj file has been recreated, please make sure that the condition (Bedingung) **MinimalRG** is set for all configurations and all platforms:

![screenshot of RG03 project options](images/RiggVar-RG03-Project-Options.png)

Also check that Ausgabeverzeichnis is set to the default of Platform and Config,
because this is typically lost when the dproj file is recreated after cloning from GitHub.

## Use Mouse Wheel

Instead of using the trackbar you can use the mouse wheel to change the value of the current param.

The trackbar may be removed later, when it is no longer needed.

The trackbar or mouse wheel will provide a delta, positive or negative.

Absolute values for the parameter will be needed when loading persisted data,
but not when changing the model.

Constraints will be checked by the model.
The same trackbar is used for all parameters and does not need to be reconfigured when the current parameter changes, same as with the mouse wheel.
We are using the trackbar as it it was a mouse wheel.
This is why you can, at any time, reset the thumb of the trackbar to the center,
to allow further scrolling if needed.

RG38, the full-featured application, does not have a trackbar at all.
In RG38 the trackbar will be replaced by a touch sensitive rectangle which is part of the button frame.

The trackbar is useful to get started with the minimal app.

## View the Graph from any angle 

I am using **Rotaform1** without the DisplayList feature as a graph.

**Rotations** about three axis are built into the graph; just drag the mouse with left and right button down.

**Panning** is also available; just grab the graph inside the green circle when dragging.

## Notes

With RG03 I have shown how to start a new application and reuse of the existing model, controller, textual view (report), and graphical view (display).

Only the most essential use cases will be supported: Selecting a parameter as current and changing the value of the currently selected parameter. Minimal text output and the graphical display will provide enough feedback about the status of the model.

This minimal application does not surface any options for model or view.

- It will start out with default options.
- It can't load persisted data.
- You cannot even reset the model or view.
- None of the available options can be changed for model or view.

Since I am adding this minimal app on top of the existing repository of RG38, I am carrying around ballast that is not needed for the minimal app.

The minimal app can be further reduced, in steps:
- Extract RG03 into its own directory.
- Remove all inactive code (undefined in RG03).
- Further remove functionality not needed.

I have done the steps at home.

If you wanted to port the minimal app to another language and platform, to have a starting point for new round of mobile development or similar, you probably would do this:

- Make sure you have a TStringList replacement on the target platform.
- Find a good TPoint3D replacement which is part of the platform.
- Port all of the model, because this is what you do not want to spend time on.
- Port some part of the controller.
- Redo the graphics, it can be simplified, here I used what is available.
- If something is wrong, you will see it watching the graph.
- And of course you should create proper tests as you go.

Please notify me if you succeed, if you have reproduced the minimal app on another platform.