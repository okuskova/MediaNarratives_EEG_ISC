# MediaNarratives_EEG_ISC

The pilot stage of the project “EEG Correlates of the Narrative Processing: The Impact of Media Literacy Skills” has been completed. The aim of the project was to investigate if there was a difference in the brain synchronization during media narrative processing among participants trained in media literacy (‘trained' group), and those who had never been trained in media literacy ('control' group). 
During EEG recording the participants watched three videos: the fragments from the news and documentary films that contained fake facts and manipulations, for instance, various mismatches and inconsistencies in the visual content and the narrator’s speech, fallacious captions, intense, emotionally provocative music, and so on. Besides, all participants watched a fragment from a fiction film as a control video. Before and after each video the participants expressed their attitudes toward social issues brought up in the videos. We then investigated how their attitudes changed after the videos and whether that change was different for the two groups. Then we examined whether there was a correlation between those changes and the ISC during the narratives within the groups. In addition, we examined a correlation of individual behavior and traits with ISC.

EEG was recorded with 64 electrodes with a standard 10-10 placement system. Sampling rate was 500 Hz. Reference electrodes TP9 and TP10 were put on mastoids. Ground electrode was AFz. The electrooculogram (EOG) was recorded with two electrodes FT9, FT10. 

## EEG data analysis

Preprocessing was conducted offline using the MATLAB software (MathWorks) and Brainstorm software (Tadel et al. 2011).

The following EEG data analysis steps have been followed:

Step_1. Segmentation. NBS Presentation triggers were used to detect onset and offset times of the naturalistic stimuli.

Step 2. Filtering. High-passed 0,5 Hz filter and Notch 50, 100, 150 and 200 Hz filters were applied. 

Step 3. Bad channels and outlier samples were manually identified and, to prevent the duration distortion of the signal, they were not deleted but replaced with a flat (zero) signal. 

Step 4. Eye movement artifacts were removed by independent component analysis (ICA).

Step 5. Temporal alignment. The segmented data (see Step 1) were integrated in the same dot.mat file with dimensions TxDxN, where T denotes the number of time samples, D denotes the number of electrodes, and N denotes the number of subjects. 

Step 6. ISC analysis: adjusting previously published scripts (see Parra Lab https://www.parralab.org/isc/)

a. Estimation of the optimal projections of the data

b. Calculation of the overall ISC 

c. Comparison of the overall ISC between groups

d. Calculation of ISC across time for each video

e. Comparison of the ISC across time between videos

Step 5. Statistics based on the ISC analysis output 

In this repository I provide scripts only for this step since all the previous steps were either applied manually (e.g., in Brainstorm) or after adjusting previously published scripts (Parra Lab https://www.parralab.org/isc/) 
