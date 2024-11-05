import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import context

context.pdsettings()


# %% Uncertainty around ocean_area_damage_coef
df = pd.read_parquet(context.projectpath().parent / "RICE50x/input/data/ocean_data.parquet")

fig, axs = plt.subplots(nrows=1, ncols=2, figsize=(8, 8), sharex=False, sharey=False)
axs = axs.flatten()
for ax, oc_capital in zip(axs, ['coral', 'mangrove']):
    _df = df.query(f'oc_capital=="{oc_capital}"')\
        .filter(['oc_capital', 'iso3', 'ocean_area_damage_coef', 'ocean_area_damage_se'])\
        .sort_values(by='ocean_area_damage_coef').reset_index(drop=True).query('ocean_area_damage_coef!=0')
    ax.errorbar(_df.ocean_area_damage_coef, _df.index, yerr=None, xerr=_df.ocean_area_damage_se, fmt='none', ecolor=None)
    ax.scatter(_df.ocean_area_damage_coef, _df.index, s=1)
    ax.axvline(0, c='k', linewidth=.75)
    ax.spines[['top', 'right', 'left']].set_visible(False)
    ax.set_yticks([], [])
    ax.set_title(oc_capital)
fig.suptitle('Area damage coefficient')
plt.show()
# %% Uncertainty around ocean_consump_damage_coef
df = pd.read_parquet(context.projectpath().parent / "RICE50x/input/data/ocean_data.parquet")

fig, axs = plt.subplots(nrows=2, ncols=2, figsize=(8, 8), sharex=False, sharey=False,)
axs = axs.flatten()
for ax, oc_capital in zip(axs, ['coral', 'mangrove', 'ports', 'fisheries']):
    _df = df.query(f'oc_capital=="{oc_capital}"')\
        .filter(['oc_capital', 'iso3', 'ocean_consump_damage_coef', 'ocean_consump_damage_se'])\
        .sort_values(by='ocean_consump_damage_coef').reset_index(drop=True).query('ocean_consump_damage_coef!=0')
    ax.errorbar(_df.ocean_consump_damage_coef, _df.index, yerr=None, xerr=_df.ocean_consump_damage_se, fmt='none', ecolor=None)
    ax.scatter(_df.ocean_consump_damage_coef, _df.index, s=1)
    ax.axvline(0, c='k', linewidth=.75)
    ax.spines[['top', 'right', 'left']].set_visible(False)
    ax.set_yticks([], [])
    ax.set_title(oc_capital)
fig.suptitle('Consumption damage coefficient')
plt.show()
