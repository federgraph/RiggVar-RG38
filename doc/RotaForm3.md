# RotaForm3

RotaForm3 is the 3D graph.

By default it is disabled.
You need to switch on the conditional by removing the dot:

```pascal
{.$define WantRotaForm3}
```

Search in all files belonging to the project for *define WantRotaForm3* and remove or add the dot.
You should find two locations:

- unit App\FrmMain.pas
- unit Graph3\RiggVar.RG.Rota.pas

If you are running without the 3D Graph, then you can remove all units in folder Graph3.
I have added them to the project, event if they are not used by default.
This is so that you can examine them in the IDE.

Now - with RotaForm3 - it is a little more complex then before.

There are 3 things that are note done yet:

1. An improved device check, for the capabilities of the driver to run the application.
1. The orthographic perspective for the RotaForm3.
1. OnResizeEnd on the Windows platform.

This will be done in the future when Embarcadero Community Edition will be ready to support it.

It can be done in IDE version 10.2.3 - Tokyo Pro, if you know how.

When my CE version will run out I will switch back to Tokyo or Berlin to maintain the project.

I said in README.md that RG38 is a full featured version of the app.
It is expected that you will build a more light weight version of the app by removing code.
It is easier to remove code than to add code.

To put slightly different versions of the same app into permanent branches would be an option.
But I think this is going against the idea if git to have permanent branches, which will never be merged with the master branch.

Somehow the orthographic projection would be better for this type of application.
We will see what happens in the future.