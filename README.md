# dhshealthrisks

This repository contains the replication files for my master's thesis at the Department of Statistics, University of Munich, Germany:

*Identification of Health Risk Factors in Developing Countries using Intrinsic Model Selection Approaches*

**Abstract**

In low- and middle-income countries, nationally representative household surveys such as the Demographic and Health Surveys provide a wealth of primary data on health, nutrition, and socio-economic outcomes. Researchers in epidemiology, public health and social sciences routinely rely on this data for statistical analyses, commonly collected through complex survey mechanisms. In this thesis, I employ statistical boosting to identify relevant risk factors of two prevalent health conditions in sub-Saharan Africa. This allows for flexible estimation of linear, smooth and spatial effects, interactions thereof, and automatic variable selection. Furthermore, I investigate how common aspects of such data can be accommodated in a statistical learning framework. Household surveys often employ multi-stage cluster sampling, which may induce a dependence structure, and data aggregated on cluster-level often exhibit overdispersion. In two case studies, I study risk factors of individual-level outcomes of childhood malnutrition and environmental correlates of geographic prevalence of malaria. In the former, the survey design can be included in the resampling strategy used to select hyper-parameters. Yet, simulation studies to assess the effect of such modifications do not show major improvements. In the latter application case, employing more flexible response distributions that accommodate the variability in the data and grants honest prediction intervals.


**Replication**

To replicate this work, access to the microdata from the Demographic and Health Surveys (DHS) has to be requested [here](https://dhsprogram.com/). Then set the following environment variables: 

```
email="yourmail@mail.com"
project="yourprojectname"
```
Then execute `src/configs/rdhs.R` to set up the API of the `rdhs` package. The files to download all required data are in the folder `src/data/`. For the analysis of Mali, Data from the [Google Earth Engine](https://earthengine.google.com/) has to be collected. See also [here](https://developers.google.com/earth-engine/tutorials/community/intro-to-python-api) for an introduction. The Python environment is documented in `requirements.txt` and can be used to set up a local virtual environment. Finally, the data exported from the Google Earth Engine has to be manually copied into the folder `data/raw/earthengine`. 

Each analysis has separate files to prepare the microdata and remote sensing data, these have to be executed first. 


**References**

(Only main references, see also report.)

J. H. Friedman, ‘Greedy function approximation: A gradient boosting machine.’, Ann. Statist., vol. 29, no. 5, Oct. 2001, doi: 10.1214/aos/1013203451.

P. Bühlmann and B. Yu, ‘Boosting With the L 2 Loss: Regression and Classification’, Journal of the American Statistical Association, vol. 98, no. 462, pp. 324–339, Jun. 2003, doi: 10.1198/016214503000125.

P. Bühlmann and T. Hothorn, ‘Boosting Algorithms: Regularization, Prediction and Model Fitting’, Statist. Sci., vol. 22, no. 4, Nov. 2007, doi: 10.1214/07-STS242.

N. Fenske, T. Kneib, and T. Hothorn, ‘Identifying Risk Factors for Severe Childhood Malnutrition by Boosting Additive Quantile Regression’, Journal of the American Statistical Association, vol. 106, no. 494, pp. 494–510, Jun. 2011, doi: 10.1198/jasa.2011.ap09272.

T. Kneib, T. Hothorn, and G. Tutz, ‘Variable Selection and Model Choice in Geoadditive Regression Models’, Biometrics, vol. 65, no. 2, pp. 626–634, Jun. 2009, doi: 10.1111/j.1541-0420.2008.01112.x.

J. Thomas, A. Mayr, B. Bischl, M. Schmid, A. Smith, and B. Hofner, ‘Gradient boosting for distributional regression: faster tuning and improved variable selection via noncyclical updates’, Stat Comput, vol. 28, no. 3, pp. 673–687, May 2018, doi: 10.1007/s11222-017-9754-6.

T. Q. Dong and J. Wakefield, ‘Modeling and presentation of vaccination coverage estimates using data from household surveys’, Vaccine, vol. 39, no. 18, pp. 2584–2594, Apr. 2021, doi: 10.1016/j.vaccine.2021.03.007.

E. Giorgi and P. J. Diggle, Model-based Geostatistics for Global Public Health: Methods and Applications. Routledge, 2021.

Institut National de la Statistique (INSTAT) and ICF, ‘Enquête Démographique et de Santé à Madagascar (EDSMD-V) 2021’, Antananarivo, Madagascar et Rockville, Maryland, USA, 2022. [Online]. Available: https://www.dhsprogram.com/pubs/pdf/FR376/FR376.pdf

Institut National de la Statistique (INSTAT), Programme National de Lutte contre le Paludisme (PNLP), and The DHS Program, ‘Enquête sur les Indicateurs du Paludisme au Mali 2021’, Bamako, Mali et Rockville, Maryland, USA, 2022. [Online]. Available: https://www.dhsprogram.com/pubs/pdf/FR376/FR376.pdf

Ethiopian Public Health Institute - EPHI, Federal Ministry of Health - FMoH, and ICF, ‘Ethiopia Mini Demographic and Health Survey 2019’, EPHI/FMoH/ICF, Addis Ababa, Ethiopia, 2021. [Online]. Available: https://www.dhsprogram.com/pubs/pdf/FR363/FR363.pdf
