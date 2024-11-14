# Long-term abundance time-series of the High Arctic terrestrial vertebrate community of Bylot Island, Nunavut

<style>
body {
text-align: justify}
</style>

## Abstract

Arctic ecosystems present unique opportunities for community-wide monitoring, in part due to their relatively low species richness. However, conducting research in these remote environments poses significant logistical challenges, resulting in long-term monitoring being exceedingly rare. Here, we focus on the long-term, intensive ecological monitoring efforts conducted on the south plain of Bylot Island (almost 400 km2 , Nunavut, Canada), which has generated a remarkable dataset spanning up to 30 years, a rarity in tundra ecosystems. Our goal is to synthesize this dataset and upscale vertebrate abundance data at the landscape level, a prerequisite to conduct community-level analyses. We have standardized data obtained with different field methods to provide readily usable long-term time series of abundance for 35 vertebrate species (30 birds and 5 mammals) present in the study system. Monitoring data includes intensive capture-mark-recapture density estimates of lemmings on trapping grids, systematic or opportunistic nest monitoring conducted across the entire study area or within specific plots for all bird species, transects of vertebrate counts distributed throughout the study area, daily incidental observations of vertebrates and satellite tracking of fox movements. Annual abundance of species was estimated at the landscape level, accounting for spatial variations. Furthermore, we provide body masses for each species, derived from empirical onsite measurements for 18 species and from the literature for the remaining species. Body mass is essential to convert species abundance into biomass for studies of trophic fluxes and ecosystem processes. Our dataset provides a unique opportunity for holistic empirical studies of ecological communities, allowing a deeper understanding of community structure and dynamics. Considering that the study site is a pristine and protected area that has experienced minimal anthropogenic impact, it can also provide an ideal baseline for investigating the impacts of global changes on high-latitude terrestrial ecosystems.

## Objective

Our main objective is to provide readily accessible, long-term time series of annual abundances of all vertebrate species within the Arctic terrestrial community of Bylot Island during the breeding season (May to August). This includes both breeding and non-breeding individuals that stay in the study area for a significant period of time, and excludes non-breeding individuals that stop for only a few days during their migration. We focus on adults, except for lemmings for which we have not distinguished between juveniles and adults. Our focus extends to estimating abundances at the landscape scale, enabling the study of community and ecosystem dynamics, trophic interactions and the impacts of global changes on high-latitude environments. Additionally, we aim to provide the average body mass for each species in the community, enabling the conversion of abundances into biomasses.

## Structure of the project

**DataS1.zip**: Ready-to-use data set on the Bylot Island vertebrate community; includes the following files (see **MetadataS1.pdf** for more details):

* **BYLOT-species_taxonomy.csv**: Species taxonomy.
  * ***class***: Taxonomic class  for birds (Gill et al., 2024) and mammals species (Upham et al., 2024).
  * ***order***: Taxonomic order  for birds (Gill et al., 2024) and mammals species (Upham et al., 2024).
  * ***family***: Taxonomic family  for birds (Gill et al., 2024) and mammals species (Upham et al., 2024).
  * ***genus***: Taxonomic genus  for birds (Gill et al., 2024) and mammals species (Upham et al., 2024).
  * ***species_scientific***: Taxonomic species  for birds (Gill et al., 2024) and mammals species (Upham et al., 2024).
  * ***species_en***: Common names of species in English.
  * ***species_fr***: Common names of species in French.
  * ***functional_group***: Functional group for each species. The classification of species into functional groups is based on Moisan et al. (2023).
  * ***migratory_status***: Migratory status of each species. The classification of species migratory status is based on Gauthier et al., (2011)  and Moisan et al. (2023).
* **BYLOT-species_abundance.csv**: Long-term series or mean species abundance.
  * ***species_en***: Common names of species in English.
  * ***year***: Year corresponding to the estimate of annual abundance. If abundance has not been calculated for a given series of years, but rather as a general average, then NA has been assigned.
  * ***breeding_status***: Reproductive status of the individuals.
  * ***abundance***: Estimate of the annual number of individuals found within the 389 km2 study area located on the southern part of Bylot Island during the breeding season (May to August). This includes both breeding and non-breeding individuals that stay in the study area for a significant period of time, and excludes non-breeding individuals that stop for only a few days during their migration. The estimates only consider adults, with the exception of lemmings, for which no distinction has been made between juveniles and adults.
  * ***method_description***: Brief overview of the method used to estimate the species abundance.
  * ***method_quality***: Qualitative measure of the method quality based on data available, method used for extrapolation (if necessary), and in some cases, from the fit of statistical models to estimate density.
* **BYLOT-species_body_mass.csv**: Mean individual body mass.
  * ***species_en***:Common names of species in English.
  * ***site***: Site where individual body mass measurements were taken.
  * ***mean_body_mass_g***: Mean individual body mass.
  * ***sample_size***: Number of individuals measured.
  * ***reference***: Reference from which estimate of mean body mass were derived.
* **BYLOT-interannual_variation_nest_density.csv**: Inter-annual variation in nest density for species for which nest monitoring has been performed.
  * ***species_en***: Common names of species in English.
  * ***zone***: Sampled zone of the study area.
  * ***mean_nest_density_km2***: Estimate of the mean annual nest density measured within the corresponding zone of the study area.
  * ***sd_nest_density_km2***: Standard deviation of the annual nest density measured within the corresponding zone of the study area.
  * ***number_years***: Number of years consider in the calculation of the nest density.

**MetadataS1.pdf**: The detailed description of the methods and the complete metadata documentation.

**BYLOT_species_abundance_dataset.zip**:  A Ready-to use R project, including raw data (*data*) and codes (*script*) used to extract the ready-to-use data set (*DataS1.zip*) and the metadata document (*MetadataS1.pdf*).

## Sharing/access information

The data  set will also be available as supplementary information by the journal upon publication of the related manuscript.

## Citation

Please use the following citation when referencing this data set, even for partial use:

Moisan, L., Bideault, A., Gauthier, G., Duchesne, É., Fauteux, D., Berteaux, D., Legagneux, P., Cadieux, M.-C. and Bêty, J. (2024). Long-term abundance time-series of the High Arctic terrestrial vertebrate community of Bylot Island, Nunavut. Dryad. [https://doi.org/10.5061/dryad.44j0zpcnt](https://doi.org/10.5061/dryad.44j0zpcnt)

## Code/Software

*Operating system*: Data preparation was performed on x86_64-pc-linux-gnu (64-bit) with Ubuntu 22.04.3 LTS.
*Program*: R version 4.3.2 (2023-10-31)
*Packages*: dplyr (Wickham et al., 2023a), tidyr (Wickham et al., 2024), sf (Pebesma et al., 2018), stringr (Wickham, 2023), xtable (Dahl et al., 2019), Distance (Miller et al., 2019), ggplot2 (Wickham, 2016), lme4 (Bates et al., 2015), AICcmodavg (Mazerolle, 2023), scales(Wickham et al., 2023b), ggmap (Kahle and Wickham, 2013)

## Contact persons

*Overall project*: Joël Bêty([joel_bety@uqar.ca](mailto:joel_bety@uqar.ca))
*Data set and codes*: Louis Moisan ([louis.moisan.bio@gmail.com](mailto:louis.moisan.bio@gmail.com))

## References

D. Bates, M. Mächler, B. Bolker, and S. Walker. Fitting linear mixed-effects models using lme4. Journal of Statistical Software, 67(1):1–48, 2015.

D. B. Dahl, D. Scott, C. Roosen, A. Magnusson, and J. Swinton. xtable: Export Tables to LaTeX or HTML, 2019. URL [https://CRAN.R](https://CRAN.R) project.org/package=xtable. R package version 1.8-4.

G. Gauthier, D. Berteaux, J. Bêty, A. Tarroux, J.-F. Therrien, L. McKinnon, P. Legagneux, and M.-C. Cadieux. The tundra food web of bylot island in a changing climate and the role of exchanges between ecosystems. Ecoscience, 18(3):223–235, 2011.

F. Gill, D. Donsker, and P. Rasmussen. Ioc world bird list (v14. 2), 2024.

D. Kahle and H. Wickham. ggmap: Spatial visualization with ggplot2. The R Journal, 5 (1):144–161, 2013.

M. J. Mazerolle. AICcmodavg: Model selection and multimodel inference based on (Q)AIC(c), 2023. URL [https://cran.r-project.org/package=AICcmodavg](https://cran.r-project.org/package=AICcmodavg). R package version 2.3.3.

D. L. Miller, E. Rexstad, L. Thomas, L. Marshall, and J. L. Laake. Distance sampling in r. Journal of Statistical Software, 89(1):1–28, 2019.

L. Moisan, D. Gravel, P. Legagneux, G. Gauthier, D.-J. Léandri-Breton, M. Somveille, J.-F. Therrien, J.-F. Lamarre, and J. Bêty. Scaling migrations to communities: An empirical case of migration network in the arctic. Frontiers in Ecology and Evolution, 10:1077260, 2023.

E. J. Pebesma et al. Simple features for r: standardized support for spatial vector data. The R Journal, 10(1):439–446, 2018.

N. Upham, C. Burgin, J. Widness, S. Liphardt, C. Parker, M. Becker, I. Rochon, D. Huckaby, and J. Zijlstra. Mammal diversity database, 2024.

H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016. ISBN 978-3-319-24277-4. URL [https://ggplot2.tidyverse.org](https://ggplot2.tidyverse.org).

H. Wickham, R. François, L. Henry, K. Müller, and D. Vaughan. dplyr: A Grammar of Data Manipulation, 2023a. URL [https://CRAN.R-project.org/package=dplyr](https://CRAN.R-project.org/package=dplyr). R package version 1.1.4.

H. Wickham, T. L. Pedersen, and D. Seidel. scales: Scale Functions for Visualization, 2023b. URL [https://CRAN.R-project.org/package=scales](https://CRAN.R-project.org/package=scales). R package version 1.3.0.

H. Wickham. stringr: Simple, Consistent Wrappers for Common String Operations, 2023. URL [https://CRAN.R-project.org/package=stringr](https://CRAN.R-project.org/package=stringr). R package version 1.5.1.

H. Wickham, D. Vaughan, and M. Girlich. tidyr: Tidy Messy Data, 2024. URL [https://CRAN.R-project.org/package=tidyr](https://CRAN.R-project.org/package=tidyr). R package version 1.3.1.
