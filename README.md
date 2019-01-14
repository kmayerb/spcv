# spcv
a minimalist package for anticipating prediction errors in spatial interpolation through cross validation



### In Search of a Goldilocks Interpolator

#### What is spcv?

*spcv*, short for spatial cross validation, is a minimalist R package for anticipating prediction errors in spatially interpolated data. For now, it’s based on hold one out cross validation, and its meant to work with the R spatial statistics package gstat.

*spcv* currently can be used only to examine the magnitude and distribution errors generated by inverse distance weighted (IDW) interpolations. With minor modification it could be adapted to cross-validate other gstat interpolators (i.e. simple, ordinary, block, and universal kriging)

#### What is inverse distance weighting?

Inverse distance weighting (or IDW for short) is an simple but flexible interpolation method. An estimate at an unknown point is calculated from a weighted average of the values available at the known points, but the relative contribution of each point is an inverse function of distance ( usually 1 / dist(x[i]-x[j] )^p ). The power parameter (p) can be increased to lend greater weight to near-field points.

IDW differs from kriging methods in that it does not rely on distributional assumptions about the underlying data. IDW is easy to tune and implement, but it offers no upfront prediction about the bias of its estimates. With IDW, accuracy is highly site and dataset specific. Therefore it would be nice to have an easy to use tool to scrutinize IDW interpolation accuracy.

#### How to use spcv


spcv::spcv() is the main function of the spcv package. It has three inputs:
  <li> df.sp    - a SpatialPointsDataFrame object, which is a data class created by the sp package </li>
  <li> my_idp   - a numeric bandwidth parameter </li>
  <li> var_name - a string specifying the column of data to be interpolated </li>

Calling spcv() on a SpatialPointsDataFrame will impliment IDW interpolation with hold-one-out cross validation.

It will return a list object containing the following:
  <li> cv.input - the input SpatialPointsDataFrame object </li>
  <li> cv.pred  - the predictions at each held-out point </li>
  <li> cv.error - differences between the held-out test points and the interpolated estimate at that location </li>
  <li> cv.rmse  - overall root mean square error between test points and predictions </li>

#### Convenience functions in spcv

*spcv:::convert_spdf_for_ggplot()* is a useful function for converting SpatialPointsDataFrame for plotting in ggplot


#### A toy example 

Consider **m.sample**, a SpatialPointsDataFrame object consisting of 100 points in {x,y} coordinate space, containing chemical results at various concentrations (“conc”). It is easily plot using spcv::convert_spdf_for_ggplot2() (also see spcv::theme_basic() for reproducing the aesthetic below).


```{r load_example}
m.sample <- readRDS( "../data-raw/spdf_sample1.rds")
head(m.sample,3)
```

```{r, echo = T, fig.height= 3,fig.width = 3, fig.align= "left"}
ggplot(spcv::convert_spdf_for_ggplot2(m.sample, "conc"), 
       aes(x,y, col =z)) + 
  geom_point() + 
  spcv::theme_basic() 
```

![f1](https://raw.githubusercontent.com/kmayerb/spcv/master/spcv/img/f1.png =250x250)

![f2](https://raw.githubusercontent.com/kmayerb/spcv/master/spcv/img/f1.png =1000x250)

![f3](https://raw.githubusercontent.com/kmayerb/spcv/master/spcv/img/f1.png =1000x250)

![f4](https://raw.githubusercontent.com/kmayerb/spcv/master/spcv/img/f1.png =1000x200)

![f5](https://raw.githubusercontent.com/kmayerb/spcv/master/spcv/img/f1.png =1000x200)

![f6](https://raw.githubusercontent.com/kmayerb/spcv/master/spcv/img/f1.png =1000x333)

![f7](https://raw.githubusercontent.com/kmayerb/spcv/master/spcv/img/f1.png =250x250)

![f8](https://raw.githubusercontent.com/kmayerb/spcv/master/spcv/img/f1.png =750x250)
