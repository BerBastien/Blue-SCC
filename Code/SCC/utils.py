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

def palette(pub=True):
    if pub:
        value_color = {'Market value': '#89cc1f', 'Non-market use value': '#1d4cce', 'Nonuse value': '#1dcece',
                       'Total': '#ff005c'}
        capital_color = {'Normative parameter': 'silver', 'Corals': '#ff00c5', 'Corals/Mangroves': '#ff00c5',
                         'Mangroves': '#1ad29b', 'Fisheries': '#0a3e7b', 'Ports': '#ffbf00'}
        capital_color_edge = {'Normative parameter': 'silver', 'Corals': '#ff00c5', 'Corals/Mangroves': '#1ad29b',
                              'Mangroves': '#1ad29b', 'Fisheries': '#0a3e7b', 'Ports': '#ffbf00'}
    else:
        value_color = {'Market value': 'tab:blue', 'Non-market use value': 'tab:orange', 'Nonuse value': 'tab:green','Total': 'tab:red'}
        capital_color = {'Normative parameter': 'silver', 'Corals': 'gold', 'Corals/Mangroves': 'gold', 'Mangroves': 'yellowgreen', 'Fisheries': 'tab:cyan', 'Ports': 'tab:purple'}
        capital_color_edge = {'Normative parameter': 'silver', 'Corals': 'gold', 'Corals/Mangroves': 'yellowgreen', 'Mangroves': 'yellowgreen', 'Fisheries': 'tab:cyan', 'Ports': 'tab:purple'}
    return value_color, capital_color, capital_color


def var_from_gdx(gdx_dict, var, vars=[]):
    df = gdx_dict[var]
    n = ['n'] if 'n' in df.columns else []
    if 'Level' in df.columns:
        df = df.filter(n + ['t', 'Level'] + vars).rename(columns={'Level': var})
    else:
        df = df.filter(n + ['t', 'Value'] + vars).rename(columns={'Value': var})
    return df


def scc(damage_gdx, damage_pulse_gdx, today_gdx):
    UTARG_today = var_from_gdx(today_gdx, 'UTARG').rename(columns={'UTARG': 'UTARG_today'})
    UTARG_base = var_from_gdx(damage_gdx, 'UTARG').rename(columns={'UTARG': 'UTARG_base'})
    UTARG_pulse = var_from_gdx(damage_pulse_gdx, 'UTARG').rename(columns={'UTARG': 'UTARG_pulse'})

    OCEAN_USENM_VALUE_today = var_from_gdx(today_gdx, 'OCEAN_USENM_VALUE', vars=['oc_capital']).groupby(
        ['n', 't']).OCEAN_USENM_VALUE.sum().reset_index().rename(
        columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_today'})
    OCEAN_USENM_VALUE_base = var_from_gdx(damage_gdx, 'OCEAN_USENM_VALUE', vars=['oc_capital']).groupby(
        ['n', 't']).OCEAN_USENM_VALUE.sum().reset_index().rename(
        columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_base'})
    OCEAN_USENM_VALUE_pulse = var_from_gdx(damage_pulse_gdx, 'OCEAN_USENM_VALUE', vars=['oc_capital']).groupby(
        ['n', 't']).OCEAN_USENM_VALUE.sum().reset_index().rename(
        columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_pulse'})

    OCEAN_NONUSE_VALUE_today = var_from_gdx(today_gdx, 'OCEAN_NONUSE_VALUE', vars=['oc_capital']).groupby(
        ['n', 't']).OCEAN_NONUSE_VALUE.sum().reset_index().rename(
        columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_today'})
    OCEAN_NONUSE_VALUE_base = var_from_gdx(damage_gdx, 'OCEAN_NONUSE_VALUE', vars=['oc_capital']).groupby(
        ['n', 't']).OCEAN_NONUSE_VALUE.sum().reset_index().rename(
        columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_base'})
    OCEAN_NONUSE_VALUE_pulse = var_from_gdx(damage_pulse_gdx, 'OCEAN_NONUSE_VALUE', vars=['oc_capital']).groupby(
        ['n', 't']).OCEAN_NONUSE_VALUE.sum().reset_index().rename(
        columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_pulse'})

    CPC_today = var_from_gdx(today_gdx, 'CPC').rename(columns={'CPC': 'CPC_today'})
    CPC_base = var_from_gdx(damage_gdx, 'CPC').rename(columns={'CPC': 'CPC_base'})
    CPC_pulse = var_from_gdx(ocean_damage_gdx, 'CPC').rename(columns={'CPC': 'CPC_pulse'})

    CPC_OCEAN_DAM_today = var_from_gdx(today_gdx, 'CPC_OCEAN_DAM').rename(
        columns={'CPC_OCEAN_DAM': 'CPC_OCEAN_DAM_today'})
    CPC_OCEAN_DAM_base = var_from_gdx(damage_gdx, 'CPC_OCEAN_DAM').rename(
        columns={'CPC_OCEAN_DAM': 'CPC_OCEAN_DAM_base'})
    CPC_OCEAN_DAM_pulse = var_from_gdx(damage_pulse_gdx, 'CPC_OCEAN_DAM').rename(
        columns={'CPC_OCEAN_DAM': 'CPC_OCEAN_DAM_pulse'})

    popu = var_from_gdx(damage_gdx, 'pop').rename(columns={'pop': 'popu'})
    rr = var_from_gdx(damage_gdx, 'rr').assign(t=lambda x: x.t.astype(int)).set_index('t')

    ocean_theta_1 = var_from_gdx(damage_gdx, 'ocean_theta_1').values[0][0]
    ocean_theta_2 = var_from_gdx(damage_gdx, 'ocean_theta_2').values[0][0]
    ocean_s1_1 = var_from_gdx(damage_gdx, 'ocean_s1_1').values[0][0]
    ocean_s1_2 = var_from_gdx(damage_gdx, 'ocean_s1_2').values[0][0]
    ocean_s2_1 = var_from_gdx(damage_gdx, 'ocean_s2_1').values[0][0]
    ocean_s2_2 = var_from_gdx(damage_gdx, 'ocean_s2_2').values[0][0]

    eta = 1.45

    # Combine column-wise (wide) into a dataframe
    df = functools.reduce(lambda l, r: pd.merge(l, r, how='inner', on=['t', 'n']),
                          [UTARG_today, UTARG_base, UTARG_pulse, OCEAN_USENM_VALUE_today, OCEAN_USENM_VALUE_base,
                           OCEAN_USENM_VALUE_pulse, OCEAN_NONUSE_VALUE_today, OCEAN_NONUSE_VALUE_base,
                           OCEAN_NONUSE_VALUE_pulse, CPC_today, CPC_base, CPC_pulse, CPC_OCEAN_DAM_today,
                           CPC_OCEAN_DAM_base, CPC_OCEAN_DAM_pulse, popu])

    df['t'] = df['t'].astype(int)

    # Convert use-nonmarket and nonuse values into per capita values
    for col in df.filter(regex='OCEAN_USENM_VALUE|OCEAN_NONUSE_VALUE').columns:
        df[f'{col}_PC'] = df[col] / df.popu * 1e6

    # Delta utility
    df = df.assign(delta_UTARG=1 / (1 - eta) * (df.UTARG_pulse ** (1 - eta) - df.UTARG_base ** (1 - eta)))

    # Marginal utility of consumption
    eta, s1, s2, C, V, W, theta1, theta2 = symbols('eta s1 s2 C V W theta1 theta2')

    eta = 1.45
    s1_1 = ocean_s1_1
    s1_2 = ocean_s1_2
    s2_1 = ocean_s2_1
    s2_2 = ocean_s1_2
    C = df.CPC_OCEAN_DAM_today
    V = df.OCEAN_USENM_VALUE_today_PC
    W = df.OCEAN_NONUSE_VALUE_today_PC
    theta1 = ocean_theta_1
    theta2 = ocean_theta_2

    muc = ((1 - eta) * s1_1 * s2_1 * C ** (theta1 - 1) *
           (s1_1 * C ** theta1 + V ** theta1 * s1_2) ** ((theta2 / theta1) - 1) *
           ((s2_1 * (s1_1 * C ** theta1 + V ** theta1 * s1_2) ** (theta2 / theta1) + W ** theta2 * s2_2) ** (
                       1 / theta2)) ** (1 - eta) /
           ((1 - s2_2) * (s1_1 * C ** theta1 + V ** theta1 * s1_2) ** (theta2 / theta1) + W ** theta2 * s2_2))
    # muc = ((1 - eta) * (1 - s1) * (1 - s2) * C ** (theta1 - 1) *
    #        ((1 - s1) * C ** theta1 + V ** theta1 * s1) ** ((theta2 / theta1) - 1) *
    #        (((1 - s2) * ((1 - s1) * C ** theta1 + V ** theta1 * s1) ** (theta2 / theta1) + W ** theta2 * s2) ** (
    #                    1 / theta2)) ** (1 - eta) /
    #        ((1 - s2) * ((1 - s1) * C ** theta1 + V ** theta1 * s1) ** (theta2 / theta1) + W ** theta2 * s2))

    df['muc'] = muc

    # Ramsey discount factor:
    #   1.1 weighted consumption: (weighted avg of CPC_today)**-eta. Use population weights
    weighted_consumption = df.groupby('t').apply(lambda x: np.average(x.CPC_today, weights=x.popu),
                                                 include_groups=False).rename('weighted_consumption_t') ** -eta
    #   1.2 Normalized with respect to time=1
    normalized_weighted_consumption = weighted_consumption / weighted_consumption.iloc[0]
    #   2. rr * normalized_weighted_consumption
    ramsey_discount_factor = rr.mul(normalized_weighted_consumption, axis=0)

    # Weighted mean of (Delta utility / Marginal utility of consumption), with population weights
    delta_UTARG_in_consumption = df.groupby('t').apply(lambda x: np.average(x.delta_UTARG / x.muc, weights=x.popu))

    # SCC: (cumulative) sum of discount factor * wmean of consumption utility * world population, divided by 1e6 because
    # the pulse is one million tons
    world_popu = df.groupby('t').popu.sum() * 1e6
    scc = ramsey_discount_factor.mul(delta_UTARG_in_consumption, axis=0).mul(world_popu, axis=0).sort_index(
        ascending=False).cumsum() / 1e6
    # Convert to present values for each t (not present value of 2005 at each t)
    scc = scc / ramsey_discount_factor
    # Convert to 2020 dollars
    conv2005toyear = 113.647 / 87.504  # in 2020 US-Dollars
    scc = scc * conv2005toyear

    scc = scc.sort_index(ascending=True)
    scc.index = scc.index * 5 + 2010
    return scc


def sectoral_scc(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, target_oc_capital=None, target_valuation=None, return_data=False):
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

    OCEAN_USENM_VALUE_today = var_from_gdx(ocean_today_gdx, 'OCEAN_USENM_VALUE', vars=['oc_capital']).assign(OCEAN_USENM_VALUE=lambda x: x.OCEAN_USENM_VALUE * 1e-6).rename(columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_today'})
    OCEAN_USENM_VALUE_base = var_from_gdx(ocean_damage_gdx, 'OCEAN_USENM_VALUE', vars=['oc_capital']).assign(OCEAN_USENM_VALUE=lambda x: x.OCEAN_USENM_VALUE * 1e-6).rename(columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_base'})
    OCEAN_USENM_VALUE_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'OCEAN_USENM_VALUE', vars=['oc_capital']).assign(OCEAN_USENM_VALUE=lambda x: x.OCEAN_USENM_VALUE * 1e-6).rename(columns={'OCEAN_USENM_VALUE': 'OCEAN_USENM_VALUE_pulse'})

    OCEAN_NONUSE_VALUE_today = var_from_gdx(ocean_today_gdx, 'OCEAN_NONUSE_VALUE', vars=['oc_capital']).assign(OCEAN_NONUSE_VALUE=lambda x: x.OCEAN_NONUSE_VALUE * 1e-6).rename(columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_today'})
    OCEAN_NONUSE_VALUE_base = var_from_gdx(ocean_damage_gdx, 'OCEAN_NONUSE_VALUE', vars=['oc_capital']).assign(OCEAN_NONUSE_VALUE=lambda x: x.OCEAN_NONUSE_VALUE * 1e-6).rename(columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_base'})
    OCEAN_NONUSE_VALUE_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'OCEAN_NONUSE_VALUE', vars=['oc_capital']).assign(OCEAN_NONUSE_VALUE=lambda x: x.OCEAN_NONUSE_VALUE * 1e-6).rename(columns={'OCEAN_NONUSE_VALUE': 'OCEAN_NONUSE_VALUE_pulse'})

    CPC_today = var_from_gdx(ocean_today_gdx, 'CPC').rename(columns={'CPC': 'CPC_today'})
    CPC_base = var_from_gdx(ocean_damage_gdx, 'CPC').rename(columns={'CPC': 'CPC_base'})
    CPC_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'CPC').rename(columns={'CPC': 'CPC_pulse'})

    CPC_OCEAN_DAM_today = var_from_gdx(ocean_today_gdx, 'CPC_OCEAN_DAM').rename(columns={'CPC_OCEAN_DAM': 'CPC_OCEAN_DAM_today'})
    CPC_OCEAN_DAM_base = var_from_gdx(ocean_damage_gdx, 'CPC_OCEAN_DAM').rename(columns={'CPC_OCEAN_DAM': 'CPC_OCEAN_DAM_base'})
    CPC_OCEAN_DAM_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'CPC_OCEAN_DAM').rename(columns={'CPC_OCEAN_DAM': 'CPC_OCEAN_DAM_pulse'})

    TATM_base = var_from_gdx(ocean_damage_gdx, 'TATM').rename(columns={'TATM': 'TATM_base'})
    TATM_pulse = var_from_gdx(ocean_damage_pulse_gdx, 'TATM').rename(columns={'TATM': 'TATM_pulse'})

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
        _CPC = CPC_base \
            .merge(ocean_consump_damage_coefs, on=['n'], how='outer') \
            .merge(TATM_base.assign(delta_TATM_base=lambda x: x.TATM_base - x.at[0, 'TATM_base']), on=['t'], how='outer') \
            .merge(TATM_pulse.assign(delta_TATM_pulse=lambda x: x.TATM_pulse - x.at[0, 'TATM_pulse']), how='outer', on='t').fillna(0)

        # delta_TATM set to delta_TATM_base (i.e. no pulse) where oc_capital is not the target on
        _CPC['delta_TATM'] = np.where(_CPC.oc_capital == target_oc_capital, _CPC.delta_TATM_pulse, _CPC.delta_TATM_base)
        _CPC['ocean_consump_damage_coefs_TATM'] = _CPC.ocean_consump_damage_coef * _CPC.delta_TATM + _CPC.ocean_consump_damage_coef_sq * _CPC.delta_TATM ** 2
        # Sum coefficients
        _CPC = _CPC.groupby(['t', 'n']).agg(CPC_base=('CPC_base', 'first'),
                                            ocean_consump_damage_coef=('ocean_consump_damage_coef', 'sum'),
                                            ocean_consump_damage_coef_sq=('ocean_consump_damage_coef_sq', 'sum'),
                                            ocean_consump_damage_coefs_TATM=('ocean_consump_damage_coefs_TATM', 'sum')).reset_index()
        _CPC['CPC_OCEAN_DAM_pulse'] = _CPC.CPC_base * (1 + _CPC.ocean_consump_damage_coefs_TATM)
        CPC_OCEAN_DAM_pulse = _CPC[['n', 't', 'CPC_OCEAN_DAM_pulse']].sort_values(['t', 'n'])
        # Aggregate unaffected values
        OCEAN_USENM_VALUE_pulse = OCEAN_USENM_VALUE_base.groupby(['t', 'n']).OCEAN_USENM_VALUE_base.sum().reset_index().rename(columns={'OCEAN_USENM_VALUE_base': 'OCEAN_USENM_VALUE_pulse'})
        OCEAN_NONUSE_VALUE_pulse = OCEAN_NONUSE_VALUE_base.groupby(['t', 'n']).OCEAN_NONUSE_VALUE_base.sum().reset_index().rename(columns={'OCEAN_NONUSE_VALUE_base': 'OCEAN_NONUSE_VALUE_pulse'})

    elif target_valuation == 'usenm':
        # OCEAN_USENM_VALUE_pulse
        _OCEAN_USENM_VALUE = OCEAN_USENM_VALUE_base.merge(OCEAN_USENM_VALUE_pulse, on=['t', 'n', 'oc_capital'])
        # OCEAN_USENM_VALUE_pulse is set to OCEAN_USENM_VALUE_base (i.e. no pulse) where oc_capital is not the target
        _OCEAN_USENM_VALUE['OCEAN_USENM_VALUE_pulse'] = np.where(_OCEAN_USENM_VALUE.oc_capital == target_oc_capital,
                                                                 _OCEAN_USENM_VALUE.OCEAN_USENM_VALUE_pulse,
                                                                 _OCEAN_USENM_VALUE.OCEAN_USENM_VALUE_base)
        # Aggregate
        OCEAN_USENM_VALUE_pulse = _OCEAN_USENM_VALUE.groupby(['t', 'n']).OCEAN_USENM_VALUE_pulse.sum().reset_index()
        # Aggregate unaffected values
        CPC_OCEAN_DAM_pulse = CPC_OCEAN_DAM_base[['n', 't', 'CPC_OCEAN_DAM_base']].sort_values(['t', 'n']).rename(columns={'CPC_OCEAN_DAM_base': 'CPC_OCEAN_DAM_pulse'})
        OCEAN_NONUSE_VALUE_pulse = OCEAN_NONUSE_VALUE_base.groupby(['t', 'n']).OCEAN_NONUSE_VALUE_base.sum().reset_index().rename(columns={'OCEAN_NONUSE_VALUE_base': 'OCEAN_NONUSE_VALUE_pulse'})

    elif target_valuation == 'nonuse':
        # OCEAN_NONUSE_VALUE_pulse
        _OCEAN_NONUSE_VALUE = OCEAN_NONUSE_VALUE_base.merge(OCEAN_NONUSE_VALUE_pulse, on=['t', 'n', 'oc_capital'])
        # OCEAN_NONUSE_VALUE_pulse is set to OCEAN_NONUSE_VALUE_base (i.e. no pulse) where oc_capital is not the target
        _OCEAN_NONUSE_VALUE['OCEAN_NONUSE_VALUE_pulse'] = np.where(_OCEAN_NONUSE_VALUE.oc_capital == target_oc_capital,
                                                                   _OCEAN_NONUSE_VALUE.OCEAN_NONUSE_VALUE_pulse,
                                                                   _OCEAN_NONUSE_VALUE.OCEAN_NONUSE_VALUE_base)
        # Aggregate
        OCEAN_NONUSE_VALUE_pulse = _OCEAN_NONUSE_VALUE.groupby(['t', 'n']).OCEAN_NONUSE_VALUE_pulse.sum().reset_index()
        # Aggregate unaffected values
        CPC_OCEAN_DAM_pulse = CPC_OCEAN_DAM_base[['n', 't', 'CPC_OCEAN_DAM_base']].sort_values(['t', 'n']).rename(columns={'CPC_OCEAN_DAM_base': 'CPC_OCEAN_DAM_pulse'})
        OCEAN_USENM_VALUE_pulse = OCEAN_USENM_VALUE_base.groupby(['t', 'n']).OCEAN_USENM_VALUE_base.sum().reset_index().rename(columns={'OCEAN_USENM_VALUE_base': 'OCEAN_USENM_VALUE_pulse'})
    else:
        # CPC_OCEAN_DAM_pulse = CPC_OCEAN_DAM_base[['n', 't', 'CPC_OCEAN_DAM_base']].sort_values(['t', 'n']).rename(columns={'CPC_OCEAN_DAM_base': 'CPC_OCEAN_DAM_pulse'})
        OCEAN_USENM_VALUE_pulse = OCEAN_USENM_VALUE_pulse.groupby(['t', 'n']).OCEAN_USENM_VALUE_pulse.sum().reset_index()
        OCEAN_NONUSE_VALUE_pulse = OCEAN_NONUSE_VALUE_pulse.groupby(['t', 'n']).OCEAN_NONUSE_VALUE_pulse.sum().reset_index()

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
        C = df.CPC_OCEAN_DAM_pulse
        V = df.OCEAN_USENM_VALUE_pulse_PC
        W = df.OCEAN_NONUSE_VALUE_pulse_PC
        _UTARG_pulse = \
            (
                    s2_1 * (s1_1 * (C ** theta1) + s1_2 * (V ** theta1)) ** (theta2 / theta1) +
                    s2_2 * (W ** theta2)
            ) ** (1 / theta2)
        df['UTARG_pulse'] = _UTARG_pulse

    # Delta utility
    df = df.assign(delta_UTARG=1 / (1 - eta) * (df.UTARG_pulse ** (1 - eta) - df.UTARG_base ** (1 - eta)))

    # Marginal utility of consumption
    C = df.CPC_OCEAN_DAM_today
    V = df.OCEAN_USENM_VALUE_today_PC
    W = df.OCEAN_NONUSE_VALUE_today_PC

    muc = (s1_1 * s2_1 * C ** (theta1 - 1) *
                 (s1_1 * C ** theta1 + V ** theta1 * s1_2) ** ((theta2 / theta1) - 1) *
                 ((s2_1 * (s1_1 * C ** theta1 + V ** theta1 * s1_2) ** (theta2 / theta1) +
                   W ** theta2 * s2_2) ** (1 / theta2)) ** (1 - eta)) /\
    (s2_1 * (s1_1 * C ** theta1 + V ** theta1 * s1_2) ** (theta2 / theta1) +
                   W ** theta2 * s2_2)

    df['muc'] = muc

    # Ramsey discount factor:
    #   1.1 weighted consumption: (weighted avg of CPC_today)**-eta. Use population weights
    weighted_consumption = df.groupby('t').apply(lambda x: np.average(x.CPC_today, weights=x.popu),
                                                 include_groups=False).rename('weighted_consumption_t') ** -eta
    #   1.2 Normalized with respect to time=1
    normalized_weighted_consumption = weighted_consumption / weighted_consumption.iloc[0]
    #   2. rr * normalized_weighted_consumption
    ramsey_discount_factor = rr.mul(normalized_weighted_consumption, axis=0)

    # Weighted mean of (Delta utility / Marginal utility of consumption), with population weights
    delta_UTARG_in_consumption = df.groupby('t').apply(lambda x: np.average(x.delta_UTARG / x.muc, weights=x.popu))

    # SCC: (cumulative) sum of: discount factor * wmean of consumption utility * world population, divided by 1e6 because
    # the pulse is one million tons
    world_popu = df.groupby('t').popu.sum() * 1e6
    scc = ramsey_discount_factor.mul(delta_UTARG_in_consumption, axis=0).mul(world_popu, axis=0).sort_index(
        ascending=False).cumsum() / 1e6  # divide by 1e6 because pulse is one million tons
    # Convert to present values for each t (not present value of 2005 at each t)
    scc = scc / ramsey_discount_factor
    # Convert to 2020 dollars
    conv2005toyear = 113.647 / 87.504  # in 2020 US-Dollars
    scc = scc * conv2005toyear
    # Make it a positive number (cost)
    scc = -scc

    scc = scc.sort_index(ascending=True)
    scc.index = scc.index * 5 + 2010
    scc.columns = ['scc']

    if return_data:
        return scc, df
    else:
        return scc #, (s1_1, s1_2, s2_1, s2_2, theta1, theta2, ocean_income_elasticity)


if __name__ == '__main__':
    try:
        ocean_damage_gdx = gdxpds.read_gdx.to_dataframes('C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_damage_BASELINE.gdx')
        ocean_damage_pulse_gdx = gdxpds.read_gdx.to_dataframes('C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_damage_pulse_BASELINE.gdx')
        ocean_today_gdx = gdxpds.read_gdx.to_dataframes('C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_today_BASELINE.gdx')
    except OSError:
        ocean_damage_gdx = gdxpds.read_gdx.to_dataframes('C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_damage_BASELINE.gdx')
        ocean_damage_pulse_gdx = gdxpds.read_gdx.to_dataframes('C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_damage_pulse_BASELINE.gdx')
        ocean_today_gdx = gdxpds.read_gdx.to_dataframes('C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_today_BASELINE.gdx')
    except:
        raise OSError('Files could not be saved.')

    targets = [('coral', 'consumption'), ('coral', 'usenm'), ('coral', 'nonuse'), ('mangrove', 'consumption'),
               ('mangrove', 'usenm'), ('mangrove', 'nonuse'), ('ports', 'consumption'), ('fisheries', 'consumption'),
               ('fisheries', 'usenm'), (None, None)]
    # targets = [(None, None)]
    l = []
    for target in targets:
        _scc = sectoral_scc(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, target[0], target[1])
        _scc = _scc.assign(oc_capital=target[0], valuation=target[1])
        l.append(_scc)
    df = pd.concat(l)
    print(df[df.index==2020].round(3))
    print(df)
    print(df[df.index==2100].round(3))
    #df.to_csv('C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/analysis_output/BlueSCC_updated_Mu.csv')
    
# print(target, sectoral_scc(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, target[0], target[1], return_data=True))
#    df2=sectoral_scc(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, 'ports', target[1], return_data=True)
#    print(df2)
#    df_scc = df2[0]  # Access the first DataFrame
#    df_utarg = df2[1]  # Access the second DataFrame
#    df_utarg.to_csv('C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/analysis_output/ports_utarg.csv')