# TRggData

My implementation is in unit RiggVar.RG.Data.

I am using several instances of the class to keep data in memory.
It is the *data access layer* of the application.
There is no database.

Even if you decide to use a database, it should be possible to keep the code for TRggData.

There is another repository which has just the code for TRggData and a minimal UI.

> see repository trimm-420-data-format-delphi

I have *cloned and modified* the text below (which I wrote) from that repo.

## TRggData instances

Just briefly, from the code, to make it clear:

```pascal
  TRggMain = class
  public
    RggData: TRggData; // temp object for data transfer

    { slot used as reference for diffing }
    Trimm0: TRggData;

    { user data slots }
    Trimm1: TRggData; // selected with button T1
    Trimm2: TRggData; // selected with button T2
    Trimm3: TRggData;
    Trimm4: TRggData;
    Trimm5: TRggData;
    Trimm6: TRggData;

    { example data slots }
    Trimm7: TRggData; // 420
    Trimm8: TRggData; // Logo
  end;
```

- I wanted to allow the user of the app to deal with a limited number of Trimms.
- TRggData is holding a Trimm-Item in memory.
- T1-T6 are the user specified Trimms.
- T0 should hold a copy of the data in T1-T8
- Diffing is an important feature. You are diffing against T0.
- T7 and T8 are read only.
- T7 is the default in Trimm 420 app.
- T7 would be a little different in Trimm 470 app.
- RggData is is temporary object.
- The main model object of the app is hiding in RggMain.Rigg.

## Notes

A TRggData instance is referred to as a slot in this writing.

T1 is the caption of a button used to select Trimm1.

The six named Trimm-Items Trimm1-Trimm6 will be available for user selection, as the current Trimm.
When you select a Trimm with a button or keyboard shortcut, data is taken from the slot,
and the current data of the application model in RggMain.Rigg will be overridden, lost without asking.

There are two more Trimm-Items, Trimm7 and Trimm8, which are very similar to Trimm1-Trimm6,
which are reinitialized to default data when selected.
This is why they appear as read only.

Before you select a Trimm - and load data from a slot -
you may want to save the current model data back to the slot it was loaded from.
You do this by copy and paste,
which is certainly odd, but yes, copy and paste is an existing way to save data back to a slot.
This is valid for any of the slots, Trimm1-Trimm8.
The short caption of the button corresponding to the copy and save action is *cap*.

Trimm0 is special, it is used for comparison only - it cannot be selected as the current Trimm -
but the UI of the real app can show the diff between current and Trimm0.
Trimm0 can be read from a Trimm-File, as the first Trimm-Item.

Using button **MT0** to update Trimm0 with the current Rigg model data is an important action to be familiar with.
When you do this while the diff text report is shown,
you can see how the differences disappear,
you can watch how they become zero.

In the app, the user can move data from Trimm1-Trimm6 to current, and from current to TrimmO.
The application will define actions to do this,
and these actions will be mapped to buttons or keyboard shortcuts.

Copy Trimm-Item ( cti ) will take the current state of Rigg, serialize it into Trimm-Item Text and copy as text to the clipboard.

Pasting from clipboard ( pti ) is similar to loading a Trimm-File or Trimm-Item from a file, it will initialize one or more slots.
It is possible to move data from slot TrimmX to slot TrimmY, when you use the clipboard
and change currently selected slot between copy and paste operations.

> Maybe in the future I will make it possible to copy between slots without going through the clipboard.

After a Trimm-Item was pasted from clipboard, the selected slot is updated, and the data is loaded into Rigg.
If the text that is on the clipboard was just copied there by the application, the state of Rigg will not change.

Using the buttons T1-T6, and T7 (420 default), and T8 (Logo example), should be an intuitive operation.
Data stored in the slot will be loaded into Rigg.

Using button cap (copy and paste) to write back current model data in Rigg to a slot is a thing which I have tried to explain.
We may need a more direct and intuitive way for the same, but it is not strictly needed.

When you have saved back current model changes from Rigg to a slot,
it still needs to be saved to disk in a Trimm-File when the application closes, or even earlier.
Note that this may or may not be a manual operation by default, you probably need to remember to save your work.
What the default mode of operation is for saving data is likely to be platform dependent.
Treat it as a good thing that there is no database, not at this time.
Working with files is a challenge I have taken up.
It is hard but doable. At least this is the assumption.

RggData is the instance of TRggData which is used as a temp object.
This one is used when we want to transfer the actual data to and from the real model object, the TRigg instance.
There are reports which print the state of RggData - Data and Json.
These reports do not change when you change the current model data of TRigg via the UI, with the mouse wheel.
So it appears as if those reports are pretty useless, perhaps only relevant while debugging.
See conditional define *WantUserConfusingReport* in unit RiggVar.RG.Report.

Under normal circumstances, any change to Rigg will immediately produce feedback through the graphics, which will change.
TRggData instances will only be effected when you initiate an IO operation.

The current data in the Rigg model is usually not the same as the data in the slots,
which contain cached data from the time of loading, be it internal hardcoded data, a file from disk, or Text pasted from clipboard.

You can watch the state within Rigg by watching the graphics of course and/or by watching the live reports.
DataText and JsonText are live reports.
- There are buttons to show these reports (**~D** and **~J**)
- and maybe alternative buttons (**~DT** and **~JT**)
- and maybe buttons that toggle on and off (**data**).

There is probably a reason why there are alternative buttons for apparently the same thing.
Perhaps because of the VCL application where text is shown in a Memo,
whereas in the FMX application the report text is shown in a TText control on the main form.

## Actions and Buttons

| Caption | Alt | Hint | Category | Explanation |
| :-- | :- | - | - | - |
| T1  |    | Trimm 1 |Slot| select, read, and load |
| T2  |    | Trimm 2 |Slot| read from Slot 2 |
| T3  |    | Trimm 3 |Slot| read from T3 |
| T4  |    | Trimm 4 |Slot| read from Trimm 4 |
| T5  |    | Trimm 5 |Slot| read from Trimm 5 |
| T6  |    | Trimm 6 |Slot| read from Trimm 6 |
| T7  |420 | Trimm 7 |Slot| init T7 and then read |
| T8  |Logo| Trimm 8 |Slot| init, select, read, load |
| ct0 |MT0 |Memory Btn|IO|Copy from current to Trimm 0 |
| rtf |    |Read Trimm File|IO| Read File |
| stf |    |Save Trimm File|IO| Save File |
| ctf |    |Copy Trimm File|IO| to clipboard as Trimm-File Text |
| cti |    |Copy Trimm Item|IO| to clipboard as Trimm-Item Text |
| pti |    |Paste Trimm Item|IO| Trimm-Item or Trimm-File |
| cap |    |Copy and Paste|IO| from Current to Slot via Clipboard |
| SB  |    |Sandboxed|Option| Toggle IsSandboxed |
| ~LI |DAT |Long Report|Report| Long Trimm-Item Text |
| ~SI |dat |Short Report|Report| Short Trimm-Item Text |
| ~D  |Sta |Data Report|Report| DataText|
| ~J  |Jsn |Json Report| JsonText |
| ~L  |Log |Log Report|Log| reset log and show in LogMemo |

The short captions of actions, shown on buttons, should be unique across the whole range of actions used in the app.

DAT: The long report used when copying data to the clipboard. May be too long to show on screen.

dat: The short report shown on screen. It can be used by humans when providing input.

Sta: Status. The DataText report. Optimized for showing on screen.
It takes the data from the Model via RggData.

Jsn: It will show the current data from the Model, formatted as Json.
Actually, it is a subset of the data, containing mostly the info which is surfaced as Trimm-Item.
The older original application still has features that are not currently surfaced in the published app.
Here we are dealing with the basics only, the kinematics of the abstraction, with no forces and material properties involved.
But at least the report shows actual data from Rigg, the current state of the model used by the app,
not just what happens to be in the temp data transfer object - RggData - from whatever the latest IO operation was.

MT0: It is the button which saves the current state to Trimm slot T0.
This is one of the operations that effect RggData instances.