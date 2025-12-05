html1 <- 
  "<p>Heat is a recognized public health hazard. More people die of heat than of any other weather-related disaster. We used Massachusetts data to evaluate the health impacts of heat in the Commonwealth. </p>
    <ul>
      <li><b>Time period:</b> Summers (May – Sept) of 2010 through 2023</li>
      <li><b>Population:</b> Massachusetts (MA) residents of all ages</li>
      <li><b>Health outcome:</b> All-cause Emergency Department (ED) visits in MA hospitals</li>
      <li><b>Exposure:</b> Daily maximum temperature for each MA Zipcode</li>
      Health impacts are calculated for summertime days where the daily maximum temperature > 75F.
    </ul>"

authors <-
  "<p>
  Chad Milando, PhD (cmilando[at]bu.edu), 
  Alexis Arlak (aarlak[at]bu.edu), 
  Gregory Wellenius, ScD (wellenius[at]bu.edu)
The Center for Climate and Health, Boston University School of Public Health
</p>
  <br>"

html2 <-
  "
      <p>To estimate the association between high ambient temperature and emergency department visits,
      we used a <b>time-stratified case-crossover study design</b>, specifically a
      <b>single-stage conditional quasi-Poisson model</b> with strata for ZCTA, year, month, and day of week.</p>

      <p>A single-stage model pools statistical coefficients across all investigated strata, and
      a quasi-Poisson model adjusts the variance of these coefficients to account for
      <b>over-dispersion</b> in the observed outcome.
      A conditional Poisson model has the added benefit of efficiently calculating desired
      model coefficients without needing to estimate values for strata-specific intercepts.</p>

      <p>We used a <b>distributed lag non-linear modeling (DLNM)</b> framework to capture
      the non-linear and lagged impact of exposure on the outcome.
      This models the exposure (temperature or heat index in each ZCTA) as a
      <i>crossbasis</i> matrix with separate components for the exposure magnitude and the lag:</p>

      <ul>
        <li>For the exposure magnitude, we used a natural spline with knots at the 50th and 90th percentile of the exposure variable.</li>
        <li>For the lag, we used a natural spline with two evenly spaced log-knots between 0 and a maximum lag of 8 days.</li>
      </ul>

      <p>At the strata level, the model has the following format:</p>

      <pre style='background-color: #ffffff; border: 1px solid #ccc; padding: 8px; border-radius: 6px;'>
        log(yₛ,ᵢ) = αₛ + βwₛ,ᵢ + holiday
      </pre>

      <p>Here, the daily count of emergency department visits <i>yₛ,ᵢ</i> is a function of a strata-specific intercept
      <i>αₛ</i> (calculated in post-processing due to the conditional Poisson formulation),
      daily crossbasis weights <i>wₛ,ᵢ</i>, and an indicator for federal holiday.
      We removed strata without recorded emergency department visits.</p>
      "

html3 <-
  "
      <p>Following the model fit, we calculated the number of emergency department visits
      <b>attributable to high ambient summertime temperature</b> each year.</p>

      <p>The first step is to define the <b>reference temperature</b>, which provides the baseline against which
      the impacts of higher temperature are compared.
      We chose a reference temperature of <b>75 °F</b>, the average daily summertime maximum temperature.</p>

      <p>The attributable number requires a reference temperature—it describes the additional health impacts that occur
      when the temperature exceeds that baseline. In other words, we quantify the
      <b>potentially avoidable emergency department visits</b> due to high summertime temperature if risk on extremely hot
      days were reduced to the risk on an “average” hot day.</p>

      <p><i>Example:</i> With 75 °F as the reference, if the daily maximum temperature is 100 °F
      and we estimate 1,000 additional ED visits, the interpretation is: 
      “Compared to days with a daily maximum temperature of 75 °F, there were 1,000 additional emergency department visits
      on days with a daily maximum temperature of 100 °F.”</p>
  
      <p>If the reference temperature is changed (e.g., to 80 °F or 70 °F), the estimate of the attributable number will also change.</p>
      "