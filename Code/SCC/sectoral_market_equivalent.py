# %% Imports
from sympy import symbols
import functools
import pathlib
from pathlib import Path
import numpy as np
import gdxpds
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import pandas as pd

import context

context.pdsettings()


def var_from_gdx(gdx_dict, var, vars=[]):
    df = gdx_dict[var]
    n = ['n'] if 'n' in df.columns else []
    if 'Level' in df.columns:
        df = df.filter(n + ['t', 'Level'] + vars).rename(columns={'Level': var})
    else:
        df = df.filter(n + ['t', 'Value'] + vars).rename(columns={'Value': var})
    return df

   
def sectoral_market_equivalent(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, target_oc_capital=None, target_valuation=None):
    """
    Main idea: Replace with the baseline (no pulse) values all the values of consumption, usenm and nonuse except for
    the target oc_capital and target_valuation.
    targets = [('coral', 'consumption'), ('coral', 'usenm'), ('coral', 'nonuse'), ('mangrove', 'consumption'),
          ('mangrove', 'usenm'), ('mangrove', 'nonuse'), ('ports', 'consumption'), ('fisheries', 'consumption'),
          ('fisheries', 'usenm'), (None, None)]


    :param ocean_today_gdx: gams run_rice50x.gms --mod_ocean=1 --nameout=ocean_today  --policy=simulation_tatm_exogen --climate_of_today=1
    :param ocean_damage_gdx: gams run_rice50x.gms --mod_ocean=1 --nameout=ocean_damage
    :param ocean_damage_pulse_gdx: gams run_rice50x.gms --mod_ocean=1 --nameout=ocean_damage_pulse  --mod_emission_pulse=ocean_damage
    :param target_oc_capital: one of 'coral', 'mangrove', 'fisheries', 'ports', None. None for overall SCC
    :param target_valuation: one of 'consumption', 'usenm', 'nonuse', None. None for overall SCC
    :return: pd.DataFrame of SCC
    """
    # Variables
    UTARG_today = var_from_gdx(ocean_today_gdx, 'UTARG').rename(columns={'UTARG': 'UTARG_today'})
    UTARG_base = var_from_gdx(ocean_damage_gdx, 'UTARG').rename(columns={'UTARG': 'UTARG_base'})
    UTARG_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'UTARG').rename(columns={'UTARG': 'UTARG_pulse'})

    OCEAN_USENM_VALUE_today = var_from_gdx(ocean_today_gdx, 'OCEAN_USENM_VALUE', vars=['oc_capital']).rename(columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_today'})
    OCEAN_USENM_VALUE_base = var_from_gdx(ocean_damage_gdx, 'OCEAN_USENM_VALUE', vars=['oc_capital']).rename(columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_base'})
    OCEAN_USENM_VALUE_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'OCEAN_USENM_VALUE', vars=['oc_capital']).rename(columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_pulse'})

    OCEAN_NONUSE_VALUE_today = var_from_gdx(ocean_today_gdx, 'OCEAN_NONUSE_VALUE', vars=['oc_capital']).rename(columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_today'})
    OCEAN_NONUSE_VALUE_base = var_from_gdx(ocean_damage_gdx, 'OCEAN_NONUSE_VALUE', vars=['oc_capital']).rename(columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_base'})
    OCEAN_NONUSE_VALUE_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'OCEAN_NONUSE_VALUE', vars=['oc_capital']).rename(columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_pulse'})

    CPC_today = var_from_gdx(ocean_today_gdx, 'CPC').rename(columns={'CPC': 'CPC_today'})
    CPC_base = var_from_gdx(ocean_damage_gdx, 'CPC').rename(columns={'CPC': 'CPC_base'})
    CPC_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'CPC').rename(columns={'CPC': 'CPC_pulse'})

    CPC_OCEAN_DAM_today = var_from_gdx(ocean_today_gdx, 'CPC_OCEAN_DAM').rename(columns={'CPC_OCEAN_DAM': 'CPC_OCEAN_DAM_today'})
    CPC_OCEAN_DAM_base = var_from_gdx(ocean_damage_gdx, 'CPC_OCEAN_DAM').rename(columns={'CPC_OCEAN_DAM': 'CPC_OCEAN_DAM_base'})
    CPC_OCEAN_DAM_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'CPC_OCEAN_DAM').rename(columns={'CPC_OCEAN_DAM': 'CPC_OCEAN_DAM_pulse'})

    TATM_base = var_from_gdx(ocean_damage_gdx, 'TATM').rename(columns={'TATM': 'TATM_base'})
    TATM_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'TATM').rename(columns={'TATM': 'TATM_pulse'})
    TATM_today = var_from_gdx(ocean_today_gdx, 'TATM').rename(columns={'TATM': 'TATM_today'})

    # Parameters
    popu = var_from_gdx(ocean_damage_gdx, 'pop').rename(columns={'pop': 'popu'})
    rr = var_from_gdx(ocean_damage_gdx, 'rr').assign(t=lambda x: x.t.astype(int)).set_index('t')
    ocean_theta_1 = var_from_gdx(ocean_damage_gdx, 'ocean_theta_1').values[0][0]
    ocean_theta_2 = var_from_gdx(ocean_damage_gdx, 'ocean_theta_2').values[0][0]
    ocean_s1_1 = var_from_gdx(ocean_damage_gdx, 'ocean_s1_1').values[0][0]
    ocean_s1_2 = var_from_gdx(ocean_damage_gdx, 'ocean_s1_2').values[0][0]
    ocean_s2_1 = var_from_gdx(ocean_damage_gdx, 'ocean_s2_1').values[0][0]
    ocean_s2_2 = var_from_gdx(ocean_damage_gdx, 'ocean_s2_2').values[0][0]
    ocean_consump_damage_coef = var_from_gdx(ocean_damage_gdx, 'ocean_consump_damage_coef', vars=['oc_capital'])
    ocean_consump_damage_coef_sq = var_from_gdx(ocean_damage_gdx, 'ocean_consump_damage_coef_sq', vars=['oc_capital'])
    elasmu = var_from_gdx(ocean_damage_gdx, 'elasmu', vars=['oc_capital']).values[0][0]

    if target_valuation == 'consumption':
        # CPC_OCEAN_DAM_pulse
        # consumption damage coefficients
        ocean_consump_damage_coefs = ocean_consump_damage_coef\
            .merge(ocean_consump_damage_coef_sq, on=['n', 'oc_capital'],how='outer').fillna(0)
        # Consumption & consumption damages & delta TATM base & delta TATM pulse
        _CPC = CPC_today \
            .merge(ocean_consump_damage_coefs, on=['n'], how='outer') \
            .merge(TATM_today.assign(delta_TATM_today=lambda x: x.TATM_today - x.at[0, 'TATM_today']), on=['t'], how='outer') \
            .merge(TATM_base.assign(delta_TATM_base=lambda x: x.TATM_base - x.at[0, 'TATM_base']), how='outer', on='t').fillna(0)

        # delta_TATM set to delta_TATM_base (i.e. no pulse) where oc_capital is not the target on
        _CPC['delta_TATM'] = np.where(_CPC.oc_capital == target_oc_capital, _CPC.delta_TATM_base, _CPC.delta_TATM_today)
        _CPC['ocean_consump_damage_coefs_TATM'] = _CPC.ocean_consump_damage_coef * _CPC.delta_TATM + _CPC.ocean_consump_damage_coef_sq * _CPC.delta_TATM ** 2
        # Sum coefficients
        _CPC = _CPC.groupby(['t', 'n']).agg(CPC_today=('CPC_today', 'first'),
                                            ocean_consump_damage_coef=('ocean_consump_damage_coef', 'sum'),
                                            ocean_consump_damage_coef_sq=('ocean_consump_damage_coef_sq', 'sum'),
                                            ocean_consump_damage_coefs_TATM=('ocean_consump_damage_coefs_TATM', 'sum')).reset_index()
        _CPC['CPC_OCEAN_DAM_base'] = _CPC.CPC_today * (1 + _CPC.ocean_consump_damage_coefs_TATM)
        CPC_OCEAN_DAM_base = _CPC[['n', 't', 'CPC_OCEAN_DAM_base']].sort_values(['t', 'n'])
        # Aggregate unaffected values
        OCEAN_USENM_VALUE_base = OCEAN_USENM_VALUE_today.groupby(['t', 'n']).OCEAN_USENM_VALUE_today.sum().reset_index().rename(columns={'OCEAN_USENM_VALUE_today': 'OCEAN_USENM_VALUE_base'})
        OCEAN_NONUSE_VALUE_base = OCEAN_NONUSE_VALUE_today.groupby(['t', 'n']).OCEAN_NONUSE_VALUE_today.sum().reset_index().rename(columns={'OCEAN_NONUSE_VALUE_today': 'OCEAN_NONUSE_VALUE_base'})

    if target_valuation == 'usenm':
        # OCEAN_USENM_VALUE_pulse
        _OCEAN_USENM_VALUE = OCEAN_USENM_VALUE_today.merge(OCEAN_USENM_VALUE_base, on=['t', 'n', 'oc_capital'])
        # OCEAN_USENM_VALUE_pulse is set to OCEAN_USENM_VALUE_base (i.e. no pulse) where oc_capital is not the target
        _OCEAN_USENM_VALUE['OCEAN_USENM_VALUE_base'] = np.where(_OCEAN_USENM_VALUE.oc_capital == target_oc_capital,
                                                                 _OCEAN_USENM_VALUE.OCEAN_USENM_VALUE_base,
                                                                 _OCEAN_USENM_VALUE.OCEAN_USENM_VALUE_today)
        # Aggregate
        OCEAN_USENM_VALUE_base = _OCEAN_USENM_VALUE.groupby(['t', 'n']).OCEAN_USENM_VALUE_base.sum().reset_index()
        # Aggregate unaffected values
        CPC_OCEAN_DAM_base = CPC_OCEAN_DAM_today[['n', 't', 'CPC_OCEAN_DAM_today']].sort_values(['t', 'n']).rename(columns={'CPC_OCEAN_DAM_today': 'CPC_OCEAN_DAM_base'})
        OCEAN_NONUSE_VALUE_base = OCEAN_NONUSE_VALUE_today.groupby(['t', 'n']).OCEAN_NONUSE_VALUE_today.sum().reset_index().rename(columns={'OCEAN_NONUSE_VALUE_today': 'OCEAN_NONUSE_VALUE_base'})

    if target_valuation == 'nonuse':
        # OCEAN_NONUSE_VALUE_pulse
        _OCEAN_NONUSE_VALUE = OCEAN_NONUSE_VALUE_today.merge(OCEAN_NONUSE_VALUE_base, on=['t', 'n', 'oc_capital'])
        # OCEAN_NONUSE_VALUE_pulse is set to OCEAN_NONUSE_VALUE_base (i.e. no pulse) where oc_capital is not the target
        _OCEAN_NONUSE_VALUE['OCEAN_NONUSE_VALUE_base'] = np.where(_OCEAN_NONUSE_VALUE.oc_capital == target_oc_capital,
                                                                   _OCEAN_NONUSE_VALUE.OCEAN_NONUSE_VALUE_base,
                                                                   _OCEAN_NONUSE_VALUE.OCEAN_NONUSE_VALUE_today)
        # Aggregate
        OCEAN_NONUSE_VALUE_base = _OCEAN_NONUSE_VALUE.groupby(['t', 'n']).OCEAN_NONUSE_VALUE_base.sum().reset_index()
        # Aggregate unaffected values
        CPC_OCEAN_DAM_base = CPC_OCEAN_DAM_today[['n', 't', 'CPC_OCEAN_DAM_today']].sort_values(['t', 'n']).rename(columns={'CPC_OCEAN_DAM_today': 'CPC_OCEAN_DAM_base'})
        OCEAN_USENM_VALUE_base = OCEAN_USENM_VALUE_today.groupby(['t', 'n']).OCEAN_USENM_VALUE_today.sum().reset_index().rename(columns={'OCEAN_USENM_VALUE_today': 'OCEAN_USENM_VALUE_base'})
    else:
        # CPC_OCEAN_DAM_pulse = CPC_OCEAN_DAM_base[['n', 't', 'CPC_OCEAN_DAM_base']].sort_values(['t', 'n']).rename(columns={'CPC_OCEAN_DAM_base': 'CPC_OCEAN_DAM_pulse'})
        OCEAN_USENM_VALUE_base = OCEAN_USENM_VALUE_base.groupby(['t', 'n']).OCEAN_USENM_VALUE_base.sum().reset_index()
        OCEAN_NONUSE_VALUE_base = OCEAN_NONUSE_VALUE_base.groupby(['t', 'n']).OCEAN_NONUSE_VALUE_base.sum().reset_index()

    # Aggregate reference values
    OCEAN_USENM_VALUE_today = OCEAN_USENM_VALUE_today.groupby(['t', 'n']).OCEAN_USENM_VALUE_today.sum().reset_index().rename(columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_today'})
    OCEAN_USENM_VALUE_base = OCEAN_USENM_VALUE_base.groupby(['t', 'n']).OCEAN_USENM_VALUE_base.sum().reset_index().rename(columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_base'})
    OCEAN_NONUSE_VALUE_today = OCEAN_NONUSE_VALUE_today.groupby(['t', 'n']).OCEAN_NONUSE_VALUE_today.sum().reset_index().rename(columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_today'})
    OCEAN_NONUSE_VALUE_base = OCEAN_NONUSE_VALUE_base.groupby(['t', 'n']).OCEAN_NONUSE_VALUE_base.sum().reset_index().rename(columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_base'})

    # Combine column-wise (wide) into a dataframe
    df = functools.reduce(lambda l, r: pd.merge(l, r, how='inner', on=['t', 'n']),
                          [UTARG_today, UTARG_base, UTARG_pulse, OCEAN_USENM_VALUE_today, OCEAN_USENM_VALUE_base,
                           OCEAN_USENM_VALUE_pulse, OCEAN_NONUSE_VALUE_today, OCEAN_NONUSE_VALUE_base,
                           OCEAN_NONUSE_VALUE_pulse, CPC_today, CPC_base, CPC_pulse, CPC_OCEAN_DAM_today,
                           CPC_OCEAN_DAM_base, CPC_OCEAN_DAM_pulse, popu])
    df['t'] = df['t'].astype(int)

    #  Compute the SCC
    # Convert use-nonmarket and nonuse values into per capita values
    for col in df.filter(regex='OCEAN_USENM_VALUE|OCEAN_NONUSE_VALUE').columns:
        df[f'{col}_PC'] = df[col] / df.popu * 1e6

    # eta, s1_1, s1_2, s2_1, s2_2, C, V, W, theta1, theta2 = symbols('eta, s1_1, s1_2, s2_1, s2_2, C, V, W, theta1, theta2')

    eta = elasmu
    s1_1 = ocean_s1_1
    s1_2 = ocean_s1_2
    s2_1 = ocean_s2_1
    s2_2 = ocean_s2_2
    theta1 = ocean_theta_1
    theta2 = ocean_theta_2

    if target_oc_capital is not None:
        # Pulse for only one type of oc_capital
        C = df.CPC_OCEAN_DAM_base
        V = df.OCEAN_USENM_VALUE_base_PC
        W = df.OCEAN_NONUSE_VALUE_base_PC
        _UTARG_base = \
            (
                    s2_1 * (s1_1 * (C ** theta1) + s1_2 * (V ** theta1)) ** (theta2 / theta1) +
                    s2_2 * (W ** theta2)
            ) ** (1 / theta2)
        df['UTARG_base'] = _UTARG_base
        
    muc = (s1_1 * s2_1 * C ** (theta1 - 1) *
                 (s1_1 * C ** theta1 + V ** theta1 * s1_2) ** ((theta2 / theta1) - 1) *
                 ((s2_1 * (s1_1 * C ** theta1 + V ** theta1 * s1_2) ** (theta2 / theta1) +
                   W ** theta2 * s2_2) ** (1 / theta2)) ** (1 - eta)) /\
    (s2_1 * (s1_1 * C ** theta1 + V ** theta1 * s1_2) ** (theta2 / theta1) +
                   W ** theta2 * s2_2)

    df['muc'] = muc

    # Delta utility
    #df = df.assign(delta_UTARG=1 / (1 - eta) * (df.UTARG_base ** (1 - eta) - df.UTARG_today** (1 - eta)))

    # Marginal utility of consumption
    C = df.CPC_OCEAN_DAM_today
    V = df.OCEAN_USENM_VALUE_today_PC
    W = df.OCEAN_NONUSE_VALUE_today_PC
    _UTARG_today = \
            (
                    s2_1 * (s1_1 * (C ** theta1) + s1_2 * (V ** theta1)) ** (theta2 / theta1) +
                    s2_2 * (W ** theta2)
            ) ** (1 / theta2)
    df['UTARG_today'] = _UTARG_today

    
    # Calculate delta_UTARG as the change in utility
    UTARG_base_values = df['UTARG_base'] ** (1 - eta)
    UTARG_today_values = df['UTARG_today'] ** (1 - eta)
    df['delta_UTARG'] = (1 / (1 - eta)) * (UTARG_base_values - UTARG_today_values)

    
    df['delta_UTARG_in_consumption'] = (df['delta_UTARG'] / df['muc'])*113.647 / 87.504  # in 2020 US-Dollars

    # Add target information to each row
    df['target_valuation'] = target_valuation
    df['target_oc_capital'] = target_oc_capital

    # Select the relevant columns to return
    results = df[['t', 'delta_UTARG', 'muc','delta_UTARG_in_consumption', 'target_valuation', 'target_oc_capital','n']]

    return results.sort_values('t')

if __name__ == '__main__':
    ocean_damage_gdx = gdxpds.read_gdx.to_dataframes(r"C:\Users\Granella\Downloads\results_ocean_damage_9999.gdx")
    ocean_damage_pulse_gdx = gdxpds.read_gdx.to_dataframes(
        r"C:\Users\Granella\Downloads\results_ocean_damage_pulse_9999.gdx")
    ocean_today_gdx = gdxpds.read_gdx.to_dataframes(r"C:\Users\Granella\Downloads\results_ocean_today_9999.gdx")

    # Define target sectors and valuations
    targets = [('coral', 'consumption'), ('coral', 'usenm'), ('coral', 'nonuse'), 
               ('mangrove', 'consumption'), ('mangrove', 'usenm'), ('mangrove', 'nonuse'), 
               ('ports', 'consumption'), ('fisheries', 'consumption'), ('fisheries', 'usenm')]

    # Collect results
    results_list = []
    for target in targets:
        # Run the function for each target
        result = sectoral_market_equivalent(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, target[0], target[1])
        
        # Add target information to the result DataFrame
        result = result.assign(oc_capital=target[0], valuation=target[1])
        
        # Append to list
        results_list.append(result)
    
    # Combine results into a single DataFrame
    final_results = pd.concat(results_list)
    
    # Print results for a specific year, e.g., 2020, and save to CSV
    print(final_results[final_results['t'] == 2020].round(3))
    print(final_results)
    # final_results.to_csv('C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/analysis_output/MarketEquivalentValues.csv', index=False)
