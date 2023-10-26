



event_mods = qread(here('data/results/2023-10-25/rural/cnty-alt/event-mods.qs'))
coefplot(event_mods[lhs = 'km2'])

# Need to rescale the coefs by their std deviation 
coeftable(event_mods)

coefplot(event_mods[lhs = 'median_birth_wt_m'])