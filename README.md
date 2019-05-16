MHMMR

R code for the segmentation of multivariate time series with a Multiple Hidden Markov Model Regression (MHMMR).


 Multiple Hidden Markov Model Regression (HMMR) for the segmentation of multivariate time series with regime changes. The model assumes that the time series is governed by a sequence of hidden discrete regimes/states, where each regime/state has multivariate Gaussian regressors emission densities.
The model parameters are estimated by MLE via the EM algorithm


 *Please cite the following papers for this code:*

```
 @article{Chamroukhi-MHMMR-2013,
 	Author = {Trabelsi, D. and Mohammed, S. and Chamroukhi, F. and Oukhellou, L. and Amirat, Y.},
 	Journal = {IEEE Transactions on Automation Science and Engineering},
 	Number = {10},
 	Pages = {829--335},
 	Title = {An unsupervised approach for automatic activity recognition based on Hidden Markov Model Regression},
 	Volume = {3},
 	Year = {2013},
 	url  = {https://chamroukhi.com/papers/Chamroukhi-MHMMR-IeeeTase.pdf}
 	}


 @article{Chamroukhi-FDA-2018,
  	Journal = {Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery},
  	Author = {Faicel Chamroukhi and Hien D. Nguyen},
  	Note = {DOI: 10.1002/widm.1298.},
  	Volume = {},
  	Title = {Model-Based Clustering and Classification of Functional Data},
  	Year = {2019},
  	Month = {to appear},
  	url =  {https://chamroukhi.com/papers/MBCC-FDA.pdf}
 }
```
