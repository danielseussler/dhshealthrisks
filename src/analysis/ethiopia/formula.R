# 
#
#
#

frml.1 = hazf ~
  bols(denom, intercept = FALSE) +
  bols(urban) +
  bols(csex) +
  bols(ctwin) +
  bols(cbord) +
  bols(hmembers) +
  bols(mreligion) +

  bols(watersource) +
  bols(sanitation) +
  bols(wealth) +
  bols(electricity) +
  bols(radio) +
  bols(television) +
  bols(refrigerator) +
  bols(bicycle) +
  bols(motorcycle) +
  bols(car) +
  bols(fews) +

  bols(cage) + bbs(cage, center = TRUE, df = 1, knots = 20) +
  bols(mage) + bbs(mage, center = TRUE, df = 1, knots = 20) +
  bols(medu) + bbs(medu, center = TRUE, df = 1, knots = 20) +
  bols(healthaccess) + bbs(healthaccess, center = TRUE, df = 1, knots = 20) +
  bols(cityaccess) + bbs(cityaccess, center = TRUE, df = 1, knots = 20) +

  bmrf(dhsregion, bnd = nb)


frml.2 = hazf ~
  bols(denom, intercept = FALSE) +
  bols(urban) + bols(urban, by = csex) +
  bols(csex) + bols(csex, by = urban) +
  bols(ctwin) + bols(ctwin, by = csex) +
  bols(cbord) + bols(cbord, by = csex) +
  bols(hmembers) + bols(hmembers, by = csex) +
  bols(mreligion) + bols(mreligion, by = csex) +

  bols(watersource) + bols(watersource, by = csex) +
  bols(sanitation) + bols(sanitation, by = csex) +
  bols(wealth) + bols(wealth, by = csex) +
  bols(electricity) + bols(electricity, by = csex) +
  bols(radio) + bols(radio, by = csex) +
  bols(television) + bols(television, by = csex) +
  bols(refrigerator) + bols(refrigerator, by = csex) +
  bols(bicycle) + bols(bicycle, by = csex) +
  bols(motorcycle) + bols(motorcycle, by = csex) +
  bols(car) + bols(car, by = csex) +
  bols(fews) + bols(fews, by = csex) +

  bols(cage) + bols(cage, by = csex) + bbs(cage, center = TRUE, df = 1, knots = 20) +
  bols(mage) + bols(mage, by = csex) + bbs(mage, center = TRUE, df = 1, knots = 20) +
  bols(medu) + bols(medu, by = csex) + bbs(medu, center = TRUE, df = 1, knots = 20) +
  bols(healthaccess) + bols(healthaccess, by = csex) + bbs(healthaccess, center = TRUE, df = 1, knots = 20) +
  bols(cityaccess) + bols(cityaccess, by = csex) + bbs(cityaccess, center = TRUE, df = 1, knots = 20) +

  bmrf(dhsregion, bnd = nb)


frml.3 = hazf ~
  bols(denom, intercept = FALSE) +

  bols(urban) + bols(urban, by = csex) +
  bols(csex) + bols(csex, by = urban) +
  bols(ctwin) + bols(ctwin, by = csex) + bols(ctwin, by = urban) +
  bols(cbord) + bols(cbord, by = csex) + bols(cbord, by = urban) +
  bols(hmembers) + bols(hmembers, by = csex) + bols(hmembers, by = urban) +
  bols(mreligion) + bols(mreligion, by = csex) + bols(mreligion, by = urban) +
  bols(watersource) + bols(watersource, by = csex) + bols(watersource, by = urban) +

  bols(sanitation) + bols(sanitation, by = csex) + bols(sanitation, by = urban) +
  bols(wealth) + bols(wealth, by = csex) + bols(wealth, by = urban) +
  bols(electricity) + bols(electricity, by = csex) + bols(electricity, by = urban) +
  bols(radio) + bols(radio, by = csex) + bols(radio, by = urban) +
  bols(television) + bols(television, by = csex) + bols(television, by = urban) +
  bols(refrigerator) + bols(refrigerator, by = csex) + bols(refrigerator, by = urban) +
  bols(bicycle) + bols(bicycle, by = csex) + bols(bicycle, by = urban) +
  bols(motorcycle) + bols(motorcycle, by = csex) + bols(motorcycle, by = urban) +
  bols(car) + bols(car, by = csex) + bols(car, by = urban) +
  bols(fews) + bols(fews, by = csex) + bols(fews, by = urban) +

  bols(cage) + bols(cage, by = csex) + bols(cage, by = urban) + bbs(cage, center = TRUE, df = 1, knots = 20) +
  bols(mage) + bols(mage, by = csex) + bols(mage, by = urban) + bbs(mage, center = TRUE, df = 1, knots = 20) +
  bols(medu) + bols(medu, by = csex) + bols(mage, by = urban) + bbs(medu, center = TRUE, df = 1, knots = 20) +
  bols(healthaccess) + bols(healthaccess, by = csex) + bols(healthaccess, by = urban) + bbs(healthaccess, center = TRUE, df = 1, knots = 20) +
  bols(cityaccess) + bols(cityaccess, by = csex) + bols(cityaccess, by = urban) + bbs(cityaccess, center = TRUE, df = 1, knots = 20) +

  bmrf(dhsregion, bnd = nb)
