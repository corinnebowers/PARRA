# 5e) Fit LISFLOOD surrogate model

The last step to fitting the inundation component model is to fit a
computationally efficient surrogate to predict the LISFLOOD inundation
map based only on the inputs *Q*<sub>*p*</sub> (peak flow, m^3/s) and
*t*<sub>*p*</sub> (time to peak flow, hrs).

# Define the surrogate model

<!-- We have chosen to use the inverse distance weighted (IDW) spatial interpolation method.  -->

We first define the surrogate model. We used the inverse distance
weighting (IDW) spatial interpolation method to generate inundation maps
within the Monte Carlo process and reduce the computational demand of
the PARRA framework. The IDW method has three hyperparameters to tune,
which are as follows:

-   *n* defines the size of the search neighborhood;
-   *p* is the power function coefficient, which defines the decay rate
    of the distance weighting; and
-   *α* defines the anisotropy (asymmetry of information content)
    between *Q*<sub>*p*</sub> and *t*<sub>*p*</sub>.

The equations defining the IDW surrogate model can be found in the
script `fit_surrogate.Rmd`. The values we considered for each
hyperparameter are listed below.

    ## n = 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20

    ## p = 0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 4, 5

    ## alpha = 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9

## Define IDW hyperparameter values

We find the best-fit hyperparameters by performing 10-fold
cross-validation on Sherlock, Stanford’s high-performance computing
cluster. An outline of that process is enumerated below.

1.  Generate the data files `nonzero.Rdata` and `buildings.Rdata` in the
    `_data/` folder.
2.  Define the values to consider for each hyperparameter in the
    user-defined section of `calc_surrogate_errors.R`.
3.  Run `calc_surrogate_errors.sbatch` to calculate error for each
    sample index and hyperparameter value.
4.  Run `fit_surrogate.Rmd` to find the best-fit hyperparameter values.

The final best-fit values chosen for the surrogate model hyperparameters
are shown in the table below. These lead to an overall model error (MAE)
of 5.38 centimeters among the grid cells that experience nonzero
inundation in at least 1% of simulations.

<div id="xzjenwebid" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#xzjenwebid .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#xzjenwebid .gt_heading {
  background-color: #d9d9d9;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xzjenwebid .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#xzjenwebid .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#xzjenwebid .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xzjenwebid .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xzjenwebid .gt_col_heading {
  color: #333333;
  background-color: #f2f2f2;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#xzjenwebid .gt_column_spanner_outer {
  color: #333333;
  background-color: #f2f2f2;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#xzjenwebid .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xzjenwebid .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xzjenwebid .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#xzjenwebid .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#xzjenwebid .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#xzjenwebid .gt_from_md > :first-child {
  margin-top: 0;
}

#xzjenwebid .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xzjenwebid .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#xzjenwebid .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#xzjenwebid .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xzjenwebid .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#xzjenwebid .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xzjenwebid .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xzjenwebid .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xzjenwebid .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xzjenwebid .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xzjenwebid .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#xzjenwebid .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xzjenwebid .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#xzjenwebid .gt_left {
  text-align: left;
}

#xzjenwebid .gt_center {
  text-align: center;
}

#xzjenwebid .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xzjenwebid .gt_font_normal {
  font-weight: normal;
}

#xzjenwebid .gt_font_bold {
  font-weight: bold;
}

#xzjenwebid .gt_font_italic {
  font-style: italic;
}

#xzjenwebid .gt_super {
  font-size: 65%;
}

#xzjenwebid .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Surrogate Model Parameter Values</th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Parameter</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Best-Fit Value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left"><div class='gt_from_md'><p>n</p>
</div></td>
<td class="gt_row gt_right">6.00</td></tr>
    <tr><td class="gt_row gt_left"><div class='gt_from_md'><p>p</p>
</div></td>
<td class="gt_row gt_right">2.00</td></tr>
    <tr><td class="gt_row gt_left"><div class='gt_from_md'><p>alpha</p>
</div></td>
<td class="gt_row gt_right">0.70</td></tr>
  </tbody>
  
  
</table>
</div>
