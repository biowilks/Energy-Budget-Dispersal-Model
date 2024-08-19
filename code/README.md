# Code

Here is all the code needed for analyses in the paper (doi:XXXX): 

- [0-disp-workflowr-script](https://github.com/biowilks/Energy-Budget-Model/blob/master/code/0-disp-workflowr-script.R) provides the code to create a workflowr project and organised subdirectories in GitHub (Blischak et al. 2023). 

- [1-fish-energy-reserve-and-trait-data](https://github.com/biowilks/Energy-Budget-Model/blob/master/code/1-fish-energy-reserve-and-trait-data.R) provides the code to: 
<div class="indent-list"> 1) Extract body mass data from Fishbase and SeaLifeBase (Boettiger et al. 2023) </div>
<div class="indent-list"> 2) Refit the energy storage data provided in (Martin et al. 2017) to convert length (mm) to mass (g). This was to obtain the intercept and slope for the fish energy storage allometry used in our model </div>

- [2-disp-raw-data-transformation](https://github.com/biowilks/Energy-Budget-Model/blob/master/code/2-disp-raw-data-transformation.R) provides the code to transform the raw maximum dispersal distance data to: 
<div class="indent-list"> 1) Import trait datasets and extract and harmonise names using rgbif</div>
<div class="indent-list"> 2) Add in missing body mass data </div>
<div class="indent-list"> 3) Add in missing movement mode </div>
<div class="indent-list"> 4) Filtering data to include flying birds, running mammals, swimming fish only </div>
<div class="indent-list"> 5) Harmonising all units for distance (m) and body mass (g) </div>

- [3-parameter-conversions](https://github.com/biowilks/Energy-Budget-Model/blob/master/code/3-parameter-conversions.R) provides the code to convert the units from the original parameter allometries to the ones used in the energy budget model. 

- [4-energy-function](https://github.com/biowilks/Energy-Budget-Model/blob/master/code/4-energy-function.R) provides the energy-budget model function needed to obtain the energetic costs of dispersal in J.

- [5-energy-data-visualisation](https://github.com/biowilks/Energy-Budget-Model/blob/master/code/5-energy-data-visualisation.R) provides the code needed to produce figures 3a-d in the paper mentioned above. 

- [6-disp-function](https://github.com/biowilks/Energy-Budget-Model/blob/master/code/6-disp-function.R) provides the energy-budget model function needed to obtain the maximum dispersal distance of animals in m. 

- [7-disp-data-visualisation](https://github.com/biowilks/Energy-Budget-Model/blob/master/code/7-disp-data-visualisation.R) provides the code needed to: 
<div class="indent-list"> 1) Make model predictions for maximum dispersal distance for each movement mode and related taxonomic group to compare to the empirical data</div>
<div class="indent-list"> 2) Calculate the percentage of data which lies above model predictions </div>
<div class="indent-list"> 3) Produce figures 2a-d and 4a-c in the paper mentioned above.</div> 
