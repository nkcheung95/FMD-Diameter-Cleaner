# FMD Diameter Cleaner
 For cleaning diameter data from labchart 8
 **
**Data import**
Data needs to be imported from labchart 8 as a txt.
Select FMD portion of data and export selection.
Export data > select Diameter channel, Time(seconds) and comments.
Downsample 100

**Adjustments**
DO NOT ADJUST THRESHOLD UNLESS NECESSARY
Use window slider to adjust the moving median window size. If downscaled by 100 for 1k/s files, a window of 11 is roughly 1 second of data used for the moving window.
 Red lines indicate the time of cuff occlusion and release.
