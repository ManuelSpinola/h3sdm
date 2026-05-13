# Creates a tidymodels workflow for Generalized Additive Models (GAM).

This function constructs a `workflow` object by combining a GAM model
specification (`gen_additive_mod` with the `mgcv` engine) with either a
`recipe` object or an explicit model formula.

It is optimized for Species Distribution Models (SDM) that use smooth
splines, ensuring that the specialized GAM formula (containing `s()`
terms) is correctly passed to the model, even when a recipe is provided
for general data preprocessing.

## Usage

``` r
h3sdm_workflow_gam(gam_spec, recipe = NULL, formula = NULL)
```

## Arguments

- gam_spec:

  A `parsnip` model specification of type
  [`gen_additive_mod()`](https://parsnip.tidymodels.org/reference/gen_additive_mod.html),
  configured with `set_engine("mgcv")`. Use `set_mode("classification")`
  for presence/absence models and `set_mode("regression")` for
  count-based models.

- recipe:

  (Optional) A `recipes` package `recipe` object (e.g., the output of
  [`h3sdm_recipe_gam`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_recipe_gam.md)).
  Used for general data preprocessing like normalization or dummy
  variable creation.

- formula:

  (Optional) A `formula` object that defines the structure of the GAM,
  including smooth terms (e.g., `y ~ s(x1) + s(x, y)`). If provided
  alongside `recipe`, this formula overrides the recipe's implicit
  formula for the final model fit.

## Value

A `workflow` object, ready for fitting with
[`fit()`](https://generics.r-lib.org/reference/fit.html) or resampling
with `fit_resamples()` or `tune_grid()`.

## Details

**Formula Priority:**

- If **only `recipe`** is provided, the workflow uses the recipe's
  implicit formula (e.g., `outcome ~ .`).

- If **`recipe` and `formula`** are provided, the workflow uses the
  `recipe` for data preprocessing but explicitly passes the `formula` to
  the `mgcv` engine for fitting, enabling the use of specialized terms
  like `s(x, y)`.

**Choosing the model mode and family:**

- For **presence/absence** data: use `set_mode("classification")`. The
  `mgcv` engine uses [`binomial()`](https://rdrr.io/r/stats/family.html)
  family by default.

- For **count** data (species richness, detections, individuals): use
  `set_mode("regression")` and specify
  `set_engine("mgcv", family = poisson())`.

## See also

Other h3sdm_tools:
[`h3sdm_recipe_gam()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_recipe_gam.md),
[`h3sdm_stack_fit()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_stack_fit.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(parsnip)

# --- Presence/absence model (binomial) ---
gam_spec_pa <- gen_additive_mod() %>%
  set_engine("mgcv") %>%
  set_mode("classification")

gam_formula_pa <- presence ~ s(bio1) + s(bio12) + s(x, y, bs = "tp")

base_rec_pa <- h3sdm_recipe_gam(data)

h3sdm_wf_pa <- h3sdm_workflow_gam(
  gam_spec = gam_spec_pa,
  recipe   = base_rec_pa,
  formula  = gam_formula_pa
)

# --- Count-based model (Poisson) ---
gam_spec_count <- gen_additive_mod() %>%
  set_engine("mgcv", family = poisson()) %>%
  set_mode("regression")

gam_formula_count <- count ~ s(bio1) + s(bio12) + s(x, y, bs = "tp")

base_rec_count <- h3sdm_recipe_gam(data, response_col = "count")

h3sdm_wf_count <- h3sdm_workflow_gam(
  gam_spec = gam_spec_count,
  recipe   = base_rec_count,
  formula  = gam_formula_count
)
} # }
```
