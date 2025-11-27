# Growth estimates for the CA CORE districts

## Code index
- 00_data.R preparing the data for analysis
- 01_descriptives.R descriptive statistics
- 02_model.R running basic growth model
- 03_analysis.R analysis of the growth estimates
- 06_oos.R

## issues

### prep
- triple checks scores/years/grades/lags
- double check lag mean computation

### analysis
- get info on the 5' and 5''' regressions (eg how many observations are being used for that? obs/grade?)
- check that shrinkage looks ok for grade 11 schools
- save 5''' residuals
- can i get (even a sample) of prior year growth results to make sure mine look coherent?
- Weight correlations with prior year scores and delta


### reporting
- need to aggregate schoolXgrade into a school-level result and compare that with the direct estimate
- update _ela.Rdata and _math.Rdata files on nero for bel  [for qc]

