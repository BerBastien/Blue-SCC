library(arrow)
library(here)
library(dplyr)
library(gsaot)

df = arrow::read_parquet(paste0(here::here(), '/Data/SCC/out/mc_intermediate.parquet')) %>% select(-'__index_level_0__')
df = df %>%
  group_by(id) %>%
  mutate(sccmax = max(scc)) %>%
  ungroup()
df = df %>%
  select(-sccmax, -ocean_theta_2)

X = df %>% select(-t, -scc, -oc_capital, -valuation, -id, -baseline)
Y = pull(df %>% select(scc))
ind = gsaot::ot_indices_1d(X, Y, 25, boot = TRUE, R = 100)


arrow::write_parquet(data.frame(names = names(ind$indices), ot = ind$indices, ot_lb = ind$indices_ci$low.ci, ot_ub = ind$indices_ci$high.ci)
  , paste0(here::here(), '/Data/SCC/out/OT.parquet'))

library(ggplot2)
plot(ind) +
  theme(axis.text.x = element_text(angle = -90))
plot_inner_stats(ind, ranking = 3)

dummy = gsaot::lower_bound(as.matrix(Y), 25, solver = 'transport')
plot(ind, dummy = dummy)

plot(X$vsl_start, Y)