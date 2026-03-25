# Create a DALEX explainer for h3sdm workflows

Creates a DALEX explainer for a species distribution model fitted with
[`h3sdm_fit_model()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_fit_model.md).
Prepares response and predictor variables, ensuring that all columns
used during model training (including `h3_address` and coordinates) are
included. The explainer can be used for feature importance, model
residuals, and other DALEX diagnostics.

## Usage

``` r
h3sdm_explain(model, data, response = "presence", label = "h3sdm workflow")
```

## Arguments

- model:

  A fitted workflow returned by
  [`h3sdm_fit_model()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_fit_model.md).

- data:

  A `data.frame` or `sf` object containing the original predictors and
  response variable. If an `sf` object, geometry is dropped
  automatically.

- response:

  Character string specifying the name of the response column. Must be a
  binary factor or numeric vector (0/1). Defaults to `"presence"`.

- label:

  Character string specifying a label for the explainer. Defaults to
  `"h3sdm workflow"`.

## Value

An object of class `explainer` from the **DALEX** package, ready to be
used with
[`feature_importance()`](https://modeloriented.github.io/DALEX/reference/model_parts.html),
[`model_performance()`](https://modeloriented.github.io/DALEX/reference/model_performance.html),
[`predict_parts()`](https://modeloriented.github.io/DALEX/reference/predict_parts.html),
and other DALEX functions.

## Examples

``` r
# \donttest{
library(h3sdm)
library(DALEX)
#> Welcome to DALEX (version: 2.5.3).
#> Find examples and detailed introduction at: http://ema.drwhy.ai/
#> Additional features will be available after installation of: ggpubr.
#> Use 'install_dependencies()' to get all suggested dependencies
library(parsnip)

dat <- data.frame(
  x1 = rnorm(20),
  x2 = rnorm(20),
  presence = factor(sample(0:1, 20, replace = TRUE))
)

model <- logistic_reg() |>
  fit(presence ~ x1 + x2, data = dat)

explainer <- h3sdm_explain(model, data = dat, response = "presence")
#> Preparation of a new explainer is initiated
#>   -> model label       :  h3sdm workflow 
#>   -> data              :  20  rows  2  cols 
#>   -> target variable   :  20  values 
#>   -> predict function  :  custom_predict 
#>   -> predicted values  :  No value for predict function target column. (  default  )
#>   -> model_info        :  package parsnip , ver. 1.4.1 , task classification (  default  ) 
#>   -> predicted values  :  numerical, min =  0.2073021 , mean =  0.6 , max =  0.9190452  
#>   -> residual function :  difference between y and yhat (  default  )
#>   -> residuals         :  numerical, min =  -0.8424846 , mean =  1.239972e-10 , max =  0.6379214  
#>   A new explainer has been created!  
feature_importance(explainer)
#>       variable mean_dropout_loss          label
#> 1 _full_model_         0.2916667 h3sdm workflow
#> 2           x2         0.3104167 h3sdm workflow
#> 3           x1         0.5177083 h3sdm workflow
#> 4   _baseline_         0.4510417 h3sdm workflow
# }
```
