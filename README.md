# ameriflux_visualization
An RShiny app to visualize AmeriFlux data

## About the data
Data were downloaded from the AmeriFlux data portal on August 1, 2022. Variables in timeseries, density and histogram plots were selected based on general interest from the community and represent the first sensor and first position of their kind appearing within the dataset. For example, if two relative humidity (RH) measurements were recorded at a flux tower, RH_1_1_1 (the first position and first sensor) will be chosen for the plots (instead of RH_1_2_1). All data were averaged to daily scales. Detailed summaries of all variables at each site are available under the "Summary statistics" tab.
Find variable information (including naming conventions and units) on the AmeriFlux data variables page.

## About this visualization tool
This app was developed to celebrate 3,000 site years of AmeriFlux data. It is intended to be used only for initial data visualization and exploration to give users a better understanding of data availability before downloading. It is not intended for detailed analysis or network synthesis.

To run and edith the app yourself, fork this repository and open the `prep_data.R` file. Change the `datawd` and `savewd` to directories where you house raw AmeriFlux data and where to save output, respectively. Install any packages not already on your local device. Run the `prep_data.R` file to prepare data (taking daily averages, summary statistics, and DOI information) as inputs into the app. Smaller data files are useful for a fast app that priortizies plotting speed over data detail. After installing any necessary packages, run the `app.R` file.

If you would like to submit improvements upon the app, contact Sophie Ruehr (sophie.ruehr (at) berkeley.edu) or create a well-commented push request on GitHub.

## Citation
If using any of these tools in publication or presentations, please acknowledge as "AmeriFlux Data Visualization Tool, Sophie Ruehr (2022)."

## Acknowledgements
This application was developed by Sophie Ruehr with support from members of the AmeriFlux community and management team: Rachel Hollowgrass, Karla Leibowitz, Christin Buechner, Housen Chu, Trevor Keenan and Margaret Torn.
