# UBC BioMet Visualization
An RShiny app to visualize Eddy Flux Data similar to that of AmeriFlux.

## About this visualization tool
This app was developed following the code by Sophie Ruehr to visualize AmeriFlux data. Like that app, it is intended to be used for quick data visualization and exploration for both second and third-stage flux datasets.

To run and edit the app yourself, fork this repository and open the `prep_data.R` file. Change the `datawd` and `savewd` to directories where you house raw AmeriFlux data and where to save output, respectively. Install any packages not already on your local device. Run the `prep_data.R` file to prepare data (taking daily averages, summary statistics, and DOI information) as inputs into the app. Smaller data files are useful for a fast app that priortizies plotting speed over data detail. After installing any necessary packages, run the `app.R` file.

## Acknowledgements
AmeriFlux Data Visualization Tool, Sophie Ruehr (2022), 10.5281/zenodo.7023749.
