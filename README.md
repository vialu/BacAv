---
title: "Welcome to BacAv"
output: html_document
---

# Welcome to BacAv


### Loading data

In order to use BacAV, the data has to be transformed to txt. files in such a way that each channel is a column separated by space. There should be no headers in the txt. file columns. Once the data is in that format, it can be loaded and visualized. The columns corresponding to the EMG and EEG channel to be used, together with the sampling rate at which the data was acquired has to be specified. 

### Time Domain

Once the data is visualized, it is possible to move to the “Time Domain” window. By clicking on “RUN” button, the EMG data will be analyzed by an algorithm that will look for muscle bursts and put markers. To do this, the data is rectified, and its amplitude is re-scaled from 0 to 1 in order to increase the signal to noise ratio to accurately find the muscle bursts. 

The following are the adjustable parameters that the algorithm will use:

![Figure 2](/Figure 2.png)

- Threshold: Value between 0 and 1 over what the muscle activity has to be in order to be considered a candidate burst.
- Time before: Window of time (in seconds) before a muscle activity exceeding the threshold.
- Time after: Window of time (in seconds) after a muscle activity exceeding the threshold.
- Amplitude before: The mean amplitude in the “Time before” window has to be lower than the value specified in this parameter. 
- Amplitude after: The mean amplitude in the “Time after” window has to be higher than the value specified in this parameter. 
- Burst duration: Window of time (in seconds) after which no new muscle burst can be marked.




In the upper right corner of the Time Domain window, the number of markers would be indicated. The parameters can be changed, and the algorithm can be run as many times as needed in order to get a representative number of bursts. The EMG plot can be zoomed in with the mouse and there is a scrolling bar below the plot. 

There are two other parameters in this window that will be used for the segmentation:

- Window: Window in seconds indicating the length of the segments.
- Onset: This will set the position of the time-point “0”, the onset of the muscle burst.

### Average

Once the markers are placed, the next step is to move to the “Average” window. In this window there is one parameter to set, the length in seconds starting from the beginning of the segments that will be used for baseline correction. After hitting “RUN” button, an average of the EEG and EMG in relation to the previously obtained markers will be displayed. 

### Reorder and split
The next step is the “Reorder and split” window. In this window, segments will be randomly re-ordered and splitted in two groups and the EEG average of the two groups will be plotted. This can be run several times and the idea is to look for consistency in the shape of the wave 

### Licence: 

This software was design for academic poupuses only. 
For other uses, contact the author felipevialu@gmail.com
<br> 
Please cite: doi: 10.1016/j.cnp.2019.12.001

