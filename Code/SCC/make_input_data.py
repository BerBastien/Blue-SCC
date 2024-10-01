# %%
from functools import reduce
import gdxpds
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from patsy.highlevel import dmatrices
import statsmodels.api as sm
import statsmodels.formula.api as smf
from tqdm import tqdm
from pathlib import Path

import context

context.pdsettings()

# %% Deflator 2020 to 2005
deflator = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/gdp_deflator_usa_2022.csv')
deflator20_05 = deflator.loc[deflator.year==2005, 'value'].iloc[0] / deflator.loc[deflator.year==2020, 'value'].iloc[0]
deflator07_05 = deflator.loc[deflator.year==2005, 'value'].iloc[0] / deflator.loc[deflator.year==2007, 'value'].iloc[0]
# %% GDP per capita
gdx_dict = gdxpds.read_gdx.to_dataframes(context.projectpath().parent / 'RICE50x\input\data\ssp_navigate-ssp_base.gdx')
gdp = gdx_dict['gdp_base_navigate'].rename(columns={'Value': 'gdp'})
popu = gdx_dict['pop_base_navigate'].rename(columns={'Value': 'popu'})
gdppc = pd.merge(gdp, popu).query("ssp=='SSP2'").drop(columns=['ssp'])\
    .assign(gdppc = lambda x: x.gdp/x.popu*1e6, year = lambda x: x.year.astype(int))\
    .assign(log_gdppc = lambda x: np.log(x.gdppc))\
    .filter(['iso3', 'year', 'gdppc', 'log_gdppc'])
# %% Coral
# Values per km2
coral_value_km2 = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/corals_areaDam_Value.csv')\
    .filter(['countrycode', 'muV_value_perkm2year',  'nuV_value_perkm2year', 'nV_value_perkm2year', 
             'muV_value_perkm2year_se', 'nuV_value_perkm2year_se', 'nV_value_perkm2year_se'])\
    .rename(columns={'countrycode': 'iso3', 'muV_value_perkm2year': 'um',  'nuV_value_perkm2year': 'unm', 'nV_value_perkm2year': 'nu',
                     'muV_value_perkm2year_se': 'um_se', 'nuV_value_perkm2year_se': 'unm_se', 'nV_value_perkm2year_se': 'nu_se'})
# convert to trillion 2005 USD
coral_value_km2['um'] *= deflator20_05 / 1e12
coral_value_km2['unm'] *= deflator20_05 / 1e12
coral_value_km2['nu'] *= deflator20_05 / 1e12
coral_value_km2['um_se'] *= deflator20_05 / 1e12
coral_value_km2['unm_se'] *= deflator20_05 / 1e12
coral_value_km2['nu_se'] *= deflator20_05 / 1e12
# Area
coral_area = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/corals_areaDam_Value.csv')\
    .filter(['countrycode', 'CoralArea_2020_km2'])\
    .rename(columns={'countrycode': 'iso3', 'CoralArea_2020_km2': 'area'})
# Area damage coefficients
coral_area_damcoef = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/corals_areaDam_Value.csv')\
    .filter(['countrycode', 'DamCoef_changeperC', 'DamCoef_changeperC_se'])\
    .rename(columns={'countrycode': 'iso3', 'DamCoef_changeperC': 'area_damcoef', 'DamCoef_changeperC_se': 'area_damcoef_se'})
# Consumption damage coefficients
coral_consump_damcoef = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/coral_GDPdam_coefficients.csv')\
    .filter(['countrycode', 'FractionChangeGDP_perC', 'FractionChangeGDP_perC_se_adj'])\
    .rename(columns={'countrycode': 'iso3', 'FractionChangeGDP_perC': 'consump_damcoef', 'FractionChangeGDP_perC_se_adj': 'consump_damcoef_se'})
coral = reduce(lambda l, r: pd.merge(l, r, on=['iso3'], how='outer'),
               [coral_value_km2, coral_area, coral_area_damcoef, coral_consump_damcoef])
coral.insert(0, 'oc_capital', 'coral')
# %% Mangroves
# Values per km2
mangrove_value_km2 = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/mangrove_benefits_per_km2.csv')\
    .filter(['countrycode', 'year', 'use_market_perkm2', 'use_nonmarket_perkm2', 'nonuse_perkm2'])\
    .rename(columns={'countrycode': 'iso3', 'use_market_perkm2': 'um', 'use_nonmarket_perkm2': 'unm', 'nonuse_perkm2': 'nu'})
# Convert to trillion 2005 USD
mangrove_value_km2['um'] *= deflator20_05 / 1e12
mangrove_value_km2['unm'] *= deflator20_05 / 1e12
mangrove_value_km2['nu'] *= deflator20_05 / 1e12
# Logs
mangrove_value_km2 = mangrove_value_km2.assign(log_um = lambda x: np.log(x.um),
                                               log_unm = lambda x: np.log(x.unm),
                                               log_nu = lambda x: np.log(x.nu))\
    .replace({-np.inf: np.nan})
# Merge with GDPpc
mangrove_value_km2 = pd.merge(mangrove_value_km2, gdppc, on=['iso3', 'year'], how='left')
# Log-log regression for each country
l = []
for iso3, g in mangrove_value_km2.groupby('iso3'):
    if len(g.dropna()) > 0:
        mod_um = smf.ols('log_um ~ log_gdppc', data=g, missing='drop').fit()
        mod_unm = smf.ols('log_unm ~ log_gdppc', data=g, missing='drop').fit()
        mod_nu = smf.ols('log_nu ~ log_gdppc', data=g, missing='drop').fit()
        l.append([iso3,
                  mod_um.params['Intercept'], mod_um.params['log_gdppc'], mod_um.bse['Intercept'], mod_um.bse['log_gdppc'],
                  mod_unm.params['Intercept'], mod_unm.params['log_gdppc'], mod_unm.bse['Intercept'], mod_unm.bse['log_gdppc'],
                  mod_nu.params['Intercept'], mod_nu.params['log_gdppc'], mod_nu.bse['Intercept'], mod_nu.bse['log_gdppc']])
# Value coefficients
mangrove_value_coef = pd.DataFrame(l, columns=[ 'iso3',
                                                'intercept_um', 'exp_um', 'intercept_um_se', 'exp_um_se',
                                                'intercept_unm', 'exp_unm', 'intercept_unm_se', 'exp_unm_se',
                                                'intercept_nu', 'exp_nu', 'intercept_nu_se', 'exp_nu_se',
                                                ])
mangrove_value_km2 = mangrove_value_km2.loc[
    mangrove_value_km2.year == mangrove_value_km2.year.min(), ['iso3', 'um', 'unm', 'nu']]
# Area
mangrove_area = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/mangrove_area_coefficients_sq.csv')\
    .filter(['countrycode', 'MangroveArea_2020_km2'])\
    .rename(columns={'countrycode': 'iso3', 'MangroveArea_2020_km2': 'area'})
# Area damage coefficients
mangrove_area_damcoef = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/mangrove_area_coefficients_sq.csv')\
    .filter(['countrycode', 'FractionChange_perC', 'FractionChange_perC_se', 'FractionChange_perC_sq', 'FractionChange_perC_sq_se'])\
    .rename(columns={'countrycode': 'iso3', 'FractionChange_perC': 'area_damcoef', 'FractionChange_perC_se': 'area_damcoef_se', 'FractionChange_perC_sq': 'area_damcoef_sq', 'FractionChange_perC_sq_se': 'area_damcoef_sq_se'})
# Consumption damage coefficients
mangrove_consump_damcoef = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/mangrove_GDPdam_coefficients.csv')\
    .filter(['countrycode', 'GDPDam_perC', 'GDPDam_perC_se_adj', 'GDPDam_perC_sq', 'GDPDam_perC_sq_se_adj'])\
    .rename(columns={'countrycode': 'iso3', 'GDPDam_perC': 'consump_damcoef', 'GDPDam_perC_se_adj': 'consump_damcoef_se', 'GDPDam_perC_sq': 'consump_damcoef_sq', 'GDPDam_perC_sq_se_adj': 'consump_damcoef_sq_se'})
mangrove_consump_damcoef_cov = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/mangrove_GDPdam_coefficients.csv')\
    .filter(['countrycode', 'cov_t_t2_adj'])\
    .rename(columns={'countrycode': 'iso3', 'cov_t_t2_adj': 'consump_damcoef_cov'})
mangrove = reduce(lambda l, r: pd.merge(l, r, on=['iso3'], how='outer'),
               [mangrove_value_km2, mangrove_area, mangrove_value_coef, mangrove_area_damcoef, mangrove_consump_damcoef, mangrove_consump_damcoef_cov])
mangrove.insert(0, 'oc_capital', 'mangrove')
# %% Ports
ports = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/ports_tcoeff.csv') \
    .filter(['iso3', 'GDP_FractionChange_perC', 'GDP_FractionChange_perC_se']) \
    .rename(columns={'GDP_FractionChange_perC': 'consump_damcoef', 'GDP_FractionChange_perC_se': 'consump_damcoef_se'})
ports.insert(0, 'oc_capital', 'ports')
# %% Fisheries
fisheries = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/fish_tcoeff.csv') \
    .filter(['country_iso3', 'GDP_FractionChange_perC', 'GDP_FractionChange_perC_se']) \
    .rename(columns={'country_iso3': 'iso3', 'GDP_FractionChange_perC': 'consump_damcoef', 'GDP_FractionChange_perC_se': 'consump_damcoef_se'})
# Health coefficients
fisheries_health_coef = pd.read_csv(context.projectpath() / 'Data/output_modules_input_rice50x/input_rice50x/mortality_seafood_nutrition.csv') \
    .filter(['countrycode', 'beta_nutrient_percChange_perDegreeC', 'beta_nutrient_percChange_perDegreeC_se',
             'TAME_nutrients_MortalityEffect', 'TAME_nutrients_MortalityEffect_se','Nutritional_D'])\
    .rename(columns={'countrycode': 'iso3', 'beta_nutrient_percChange_perDegreeC': 'health_beta',
                     'beta_nutrient_percChange_perDegreeC_se': 'health_beta_se',
                     'TAME_nutrients_MortalityEffect': 'health_tame',
                     'TAME_nutrients_MortalityEffect_se': 'health_tame_se',
                     'Nutritional_D': 'health_mu'
                     })
for col in fisheries_health_coef.filter(like='health_beta').columns:
    fisheries_health_coef[col] /= 100
fisheries = pd.merge(fisheries, fisheries_health_coef, on=['iso3'], how='outer').fillna(0)
fisheries.insert(0, 'oc_capital', 'fisheries')
# %% Combine and export
df = pd.concat([coral, mangrove, fisheries, ports]).dropna(subset=['iso3']).fillna(0)
df = df.rename(columns={
    'um': 'ocean_um_start',
    'unm': 'ocean_unm_start',
    'nu': 'ocean_nu_start',
    'um_se': 'ocean_um_start_se',
    'unm_se': 'ocean_unm_start_se',
    'nu_se': 'ocean_nu_start_se',
    'area': 'ocean_area_start',
    'area_damcoef': 'ocean_area_damage_coef',
    'area_damcoef_se': 'ocean_area_damage_se',
    'area_damcoef_sq': 'ocean_area_damage_coef_sq',
    'area_damcoef_sq_se': 'ocean_area_damage_sq_se',
    'consump_damcoef': 'ocean_consump_damage_coef',
    'consump_damcoef_se': 'ocean_consump_damage_se',
    'consump_damcoef_sq': 'ocean_consump_damage_coef_sq',
    'consump_damcoef_sq_se': 'ocean_consump_damage_sq_se',
    'consump_damcoef_cov': 'ocean_consump_damage_coef_cov',
    'health_mu': 'ocean_health_mu',
    'health_beta': 'ocean_health_beta',
    'health_beta_se': 'ocean_health_beta_se',
    'health_tame': 'ocean_health_tame',
    'health_tame_se': 'ocean_health_tame_se',
    'intercept_um': 'ocean_value_intercept_um',
    'intercept_um_se': 'ocean_value_intercept_um_se',
    'exp_um': 'ocean_value_exp_um',
    'exp_um_se': 'ocean_value_exp_um_se',
    'intercept_unm': 'ocean_value_intercept_unm',
    'intercept_unm_se': 'ocean_value_intercept_unm_se',
    'exp_unm': 'ocean_value_exp_unm',
    'exp_unm_se': 'ocean_value_exp_unm_se',
    'intercept_nu': 'ocean_value_intercept_nu',
    'intercept_nu_se': 'ocean_value_intercept_nu_se',
    'exp_nu': 'ocean_value_exp_nu',
    'exp_nu_se': 'ocean_value_exp_nu_se',
})
try:
    df.to_parquet(context.projectpath().parent / "RICE50x/input/data/ocean_data.parquet")
except OSError:
    df.to_parquet(r"C:\Users\basti\Documents\GitHub\RICE50x\input\data\ocean_data.parquet")
except:
    raise OSError('File could not be saved.')
