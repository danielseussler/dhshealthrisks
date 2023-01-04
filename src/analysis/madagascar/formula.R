# formulas
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
  bols(memployed) +
  bols(nodead) +

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

  bols(cage) + bbs(cage, center = TRUE, df = 1L, knots = 20L) +
  bols(mage) + bbs(mage, center = TRUE, df = 1L, knots = 20L) +
  bols(mbmi) + bbs(mbmi, center = TRUE, df = 1L, knots = 20L) +
  bols(medu) + bbs(medu, center = TRUE, df = 1L, knots = 20L) +
  bols(healthaccess) + bbs(healthaccess, center = TRUE, df = 1L, knots = 20L) +
  bols(cityaccess) + bbs(cityaccess, center = TRUE, df = 1L, knots = 20L) +

  bmrf(dhsregion, bnd = nb)


frml.2 = hazf ~
  bols(denom, intercept = FALSE) +
  bols(urban) + bols(urban, by = csex) +
  bols(csex) +
  bols(ctwin) + bols(ctwin, by = csex) +
  bols(cbord) + bols(cbord, by = csex) +
  bols(hmembers) + bols(hmembers, by = csex) +
  bols(mreligion) + bols(mreligion, by = csex) +
  bols(memployed) + bols(memployed, by = csex) +
  bols(nodead) + bols(nodead, by = csex) +

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

  bols(cage) + bols(cage, by = csex) + bbs(cage, center = TRUE, df = 1L, knots = 20L) + bbs(cage, by = csex, center = TRUE, df = 1L, knots = 20L) +
  bols(mage) + bols(mage, by = csex) + bbs(mage, center = TRUE, df = 1L, knots = 20L) +  bbs(mage, by = csex, center = TRUE, df = 1L, knots = 20L) +
  bols(mbmi) + bols(mbmi, by = csex) + bbs(mbmi, center = TRUE, df = 1L, knots = 20L) + bbs(mbmi, by = csex, center = TRUE, df = 1L, knots = 20L) +
  bols(medu) + bols(medu, by = csex) + bbs(medu, center = TRUE, df = 1L, knots = 20L) + bbs(medu, by = csex, center = TRUE, df = 1L, knots = 20L) +
  bols(healthaccess) + bols(healthaccess, by = csex) + bbs(healthaccess, center = TRUE, df = 1L, knots = 20L) + bbs(healthaccess, by = csex, center = TRUE, df = 1L, knots = 20L) +
  bols(cityaccess) + bols(cityaccess, by = csex) + bbs(cityaccess, center = TRUE, df = 1L, knots = 20L) + bbs(cityaccess, by = csex, center = TRUE, df = 1L, knots = 20L) +

  bmrf(dhsregion, bnd = nb) + bmrf(dhsregion, bnd = nb, by = csex)


frml.3 = hazf ~
  bols(denom, intercept = FALSE) +

  bols(urban) +
  bols(csex) + bols(csex, by = urban) +
  bols(ctwin) + bols(ctwin, by = urban) +
  bols(cbord) +  bols(cbord, by = urban) +
  bols(hmembers) + bols(hmembers, by = urban) +
  bols(mreligion) + bols(mreligion, by = urban) +
  bols(memployed) +  bols(memployed, by = urban) +
  bols(nodead) + bols(nodead, by = urban) +
  bols(watersource) + bols(watersource, by = urban) +

  bols(sanitation) + bols(sanitation, by = urban) +
  bols(wealth) +  bols(wealth, by = urban) +
  bols(electricity) +  bols(electricity, by = urban) +
  bols(radio) +  bols(radio, by = urban) +
  bols(television) +  bols(television, by = urban) +
  bols(refrigerator) +  bols(refrigerator, by = urban) +
  bols(bicycle) +  bols(bicycle, by = urban) +
  bols(motorcycle) + bols(motorcycle, by = urban) +
  bols(car) + bols(car, by = urban) +
  bols(fews) +  bols(fews, by = urban) +

  bols(cage) + bols(cage, by = urban) + bbs(cage, center = TRUE, df = 1L, knots = 20L) + bbs(cage, by = urban, center = TRUE, df = 1L, knots = 20L) +
  bols(mage) + bols(mage, by = urban) + bbs(mage, center = TRUE, df = 1L, knots = 20L) + bbs(mage, by = urban, center = TRUE, df = 1L, knots = 20L) +
  bols(mbmi) + bols(mbmi, by = urban) + bbs(mbmi, center = TRUE, df = 1L, knots = 20L) + bbs(mbmi, by = urban, center = TRUE, df = 1L, knots = 20L) +
  bols(medu) + bols(mage, by = urban) + bbs(medu, center = TRUE, df = 1L, knots = 20L) + bbs(medu, by = urban, center = TRUE, df = 1L, knots = 20L) +
  bols(healthaccess) + bols(healthaccess, by = urban) + bbs(healthaccess, center = TRUE, df = 1L, knots = 20L) + bbs(healthaccess, by = urban, center = TRUE, df = 1L, knots = 20L) +
  bols(cityaccess) + bols(cityaccess, by = urban) + bbs(cityaccess, center = TRUE, df = 1L, knots = 20L) + bbs(cityaccess, by = urban, center = TRUE, df = 1L, knots = 20L) +

  bmrf(dhsregion, bnd = nb) + bmrf(dhsregion, bnd = nb, by = urban)


frml.tree = hazf ~ denom + urban + csex + ctwin + cbord + hmembers + mreligion +
  memployed + nodead + watersource + sanitation + wealth + electricity + radio +
  television + refrigerator + bicycle + motorcycle + car + fews + cage + mage +
  mbmi + medu + healthaccess + cityaccess + dhsregion
