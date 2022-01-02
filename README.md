# Timingway

Timingway is designed to seamlessly visualize timelines and mechanics from Google Sheets in timer format.

## How do I use this app?

Simply open http://timingway.app/?url=your-sheet-here, replacing "your-sheet-here" with the sheet-id of your Google Sheets (make sure that sheet sharing permissions are set so that everyone with the url can see it!)

Google sheets urls look like this:  https://docs.google.com/spreadsheets/d/sheet-id/somethingsomething. What you want is the **sheet-id** part.

For example, if I have a sharing link of https://docs.google.com/spreadsheets/d/1WaKmZOOsJudBZGCwdennHxKa2W36i5iqPtwBVfBQtHU/edit#gid=2120318740, my app url would be http://timingway.app/?url=1WaKmZOOsJudBZGCwdennHxKa2W36i5iqPtwBVfBQtHU.

Finally, **make sure** that the sheet you want Timingway to read from is called **App**. By default, Google Sheets names the first sheet you create Sheet1; be sure to rename it if you want Timingway to work!

## I don't have a second monitor!

You can also use this app in overlay mode! Use this url instead: http://timingway.app/?overlay=true&url=your-url-here.

If you want the app to appear as an overlay, you might need something to open a browser window as a transparent overlay. I recommend using the Pennywise browser (https://github.com/kamranahmedse/pennywise).

There's no reset button for the overlay mode; you need to use hotkeys.

## Generating HTML for testing

If you are working on this project, you can use the following command to generate the JS file used in index.html:
```bash
$ make js
```
