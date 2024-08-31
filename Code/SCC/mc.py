# %% Imports
import re
from joblib import Parallel, delayed
from pathlib import Path
from sympy import symbols
import functools
import numpy as np
import seaborn as sns
import gdxpds
import matplotlib.pyplot as plt
from tqdm import tqdm
import pandas as pd
from utils import sectoral_scc
from matplotlib import ticker

from utils import var_from_gdx
import context

context.pdsettings()


def scc_mc(mc_id):
    if not (Path(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x\results_ocean") / f'results_ocean_damage_pulse_{mc_id}.gdx').is_file():
        return pd.DataFrame()
    ocean_damage_gdx = gdxpds.read_gdx.to_dataframes(results_folder / f'results_ocean_damage_{mc_id}.gdx')
    ocean_damage_pulse_gdx = gdxpds.read_gdx.to_dataframes(results_folder / f'results_ocean_damage_pulse_{mc_id}.gdx')
    ocean_today_gdx = gdxpds.read_gdx.to_dataframes(results_folder / f'results_ocean_today_{mc_id}.gdx')
    _l = []
    targets = [('coral', 'consumption'), ('coral', 'usenm'), ('coral', 'nonuse'), ('mangrove', 'consumption'),
               ('mangrove', 'usenm'), ('mangrove', 'nonuse'), ('ports', 'consumption'), ('fisheries', 'consumption'),
               ('fisheries', 'usenm'), (None, None)]
    for target in targets:
        try:
            _scc = sectoral_scc(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, target[0], target[1])
        except:
            return pd.DataFrame()
        _target = target if target != (None, None) else ('total', 'total')
        _scc = _scc.assign(oc_capital=_target[0], valuation=_target[1], id=mc_id)
        _l.append(_scc)
    mc_sampling_df = pd.read_parquet(Path.cwd() / 'Data/SCC/out/lhs.parquet').query(f'id=={mc_id}')
    mc_sampling_df['id'] = mc_sampling_df['id'].astype(str)
    return pd.concat(_l).reset_index().merge(mc_sampling_df, on='id', how='outer')

# %%
results_folder = Path(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x\results_ocean")
files = list(results_folder.glob('*.gdx'))
mc_ids = set(re.sub('results_ocean_damage_pulse_|results_ocean_damage_|results_ocean_today_', '', x.stem) for x in files)
mc_ids = mc_ids - set([9999])  # Baseline
l = Parallel(n_jobs=5)(delayed(scc_mc)(mc_id) for mc_id in tqdm(mc_ids))
df = pd.concat(l)
df = df[df.t==2020]

df['oc_capital'] = df.oc_capital.replace({'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves',
                       'total': 'Total'})
df.to_parquet(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\blue_rice\data\out\MC.parquet")
df = pd.read_parquet(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\blue_rice\data\out\MC.parquet")
# %% Baseline
baseline = scc_mc(9999).query('t==2020')
baseline['oc_capital'] = baseline.oc_capital.replace({'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves',
                       'total': 'Total'})
baseline['valuation'] = baseline['valuation']\
    .replace({'consumption': 'Market value', 'usenm': 'Non-market use value', 'nonuse': 'Nonuse value', 'total': 'Total'})
oc_capital_totals = baseline.query('oc_capital!="Total"').groupby('oc_capital').scc.sum().reset_index()
valuation_totals = baseline.query('oc_capital!="Total"').groupby('valuation').scc.sum().reset_index()
# %% SCC distribution
plot_df = df.groupby(['oc_capital', 'id']).scc.sum().unstack().T
plot_df = plot_df[plot_df.median().sort_values(ascending=False).index]
color_dict = {'Market value': 'tab:blue', 'Non-market use value': 'tab:orange', 'Nonuse value': 'tab:green', 'Total': 'tab:red'}
marker_dict = {'Market value': 'x', 'Non-market use value': '+', 'Nonuse value': '*', 'Total': 'o'}
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(8,4), sharex=False, sharey=False)
ax.boxplot(plot_df, vert=False)
ax.set_yticklabels(plot_df.columns)
posdict = {x._text: x._y for x in ax.get_yticklabels()}
for i, row in baseline.iterrows():
    ax.scatter(row.scc, posdict[row.oc_capital], c=color_dict[row['valuation']], label=row['valuation'], marker=marker_dict[row.valuation])
for i, row in oc_capital_totals.iterrows():
    ax.scatter(row.scc, posdict[row.oc_capital], c=color_dict['Total'], marker=marker_dict['Total'])
for i, row in valuation_totals.iterrows():
    ax.scatter(row.scc, posdict['Total'], c=color_dict[row['valuation']], marker=marker_dict[row.valuation])
ax.spines[['top', 'right']].set_visible(False)
for line in ["left","bottom"]:
    ax.spines[line].set_position(("outward", 10))
ax.axvline(0)
ax.set_xlim(0, ax.get_xlim()[1])
ax.set_xlabel('Social Cost of Carbon (2020 USD)')
ax.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
h, l = ax.get_legend_handles_labels()
ax.legend(h[:3] + h[-1:], l[:3] + l[-1:])
plt.tight_layout()
plt.show()

# %% SCC distribution
plot_df = df.groupby(['oc_capital', 'id']).scc.sum().unstack().T
plot_df = plot_df[plot_df.median().sort_values(ascending=False).index]
plot_df = plot_df.rename(columns={'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves', 'total': 'Total'})
color_dict = {'Market value': 'tab:blue', 'Non-market use value': 'tab:orange', 'Nonuse value': 'tab:green', 'Total': 'tab:red'}
marker_dict = {'Market value': 'x', 'Non-market use value': '+', 'Nonuse value': '*', 'Total': 'o'}
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(8,4), sharex=False, sharey=False)
ax.boxplot(plot_df, vert=False)
ax.set_yticklabels(plot_df.columns)
posdict = {x._text: x._y for x in ax.get_yticklabels()}
ax.spines[['top', 'right']].set_visible(False)
for line in ["left","bottom"]:
    ax.spines[line].set_position(("outward", 10))
ax.axvline(0, c='k')
ax.set_xlim(0, ax.get_xlim()[1])
ax.set_xlabel('Social Cost of Carbon (2020 USD)')
ax.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
# ax.set_xscale('asinh')
# ax.axvline(baseline.loc[baseline.oc_capital=='Total', 'scc'].iat[0], c='tab:red', label='Total SCC, main result')
# h, l = ax.get_legend_handles_labels()
# ax.legend(h[:3] + h[-1:], l[:3] + l[-1:])
ax.legend(frameon=False)
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/scc_distribution.png')
plt.show()

# %% Baseline
cmap = plt.get_cmap('tab20c')
color_dict = {'Market value': 'tab:blue', 'Non-market use value': 'tab:orange', 'Nonuse value': 'tab:green', 'Total': 'tab:red'}
marker_dict = {'Market value': 'x', 'Non-market use value': '+', 'Nonuse value': '*', 'Total': 'o'}
baseline['scc_sum'] = baseline.groupby('oc_capital').scc.transform('sum')
baseline = baseline.sort_values(['scc_sum', 'valuation'], ascending=[False, True])
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(8,4), sharex=False, sharey=False)
for h, (v, g) in enumerate(baseline.groupby('oc_capital', sort=False)):
    left = 0
    for i, row in g.iterrows():
        ax.barh(v, row.scc, left=left, color=color_dict[row['valuation']], label=row['valuation'])
        left += row.scc
        print(v, row['valuation'], left)
    ax.text(left + 3, h, f'${left:.1f}')
ax.spines[['top', 'right']].set_visible(False)
for line in ["left","bottom"]:
    ax.spines[line].set_position(("outward", 10))
ax.axhline(0.5, c='k', linewidth=1)
ax.set_xlim(0, ax.get_xlim()[1])
ax.set_xlabel('Social Cost of Carbon (2020 USD)')
ax.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
h, l = ax.get_legend_handles_labels()
ax.legend(h[1:4] + h[:1], l[1:4] + l[:1], frameon=False,  title_fontproperties={'weight':'demibold'})
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/scc_breakdown.png')
plt.show()

# %%
total = df[df.oc_capital == 'total']
total20 = total[(total.index == 2020) & (total.id.str.startswith('theta')==False)]
theta20 = df[(df.oc_capital == 'total') & (df.index==2020) & (df.id.str.startswith('theta'))].sort_values('theta1')
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(5,5), sharex=False, sharey=False)
ax.plot(theta20.theta1, theta20.scc)
ax.set_xlabel(r'$\theta_1, \theta_2$')
ax.set_ylabel('2020 USD', loc='top', rotation='horizontal', fontweight='normal')
ax.yaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
ax.set_ylim(0, ax.get_ylim()[1])
ax.set_title('2020 SCC')
ax.spines[['top', 'right']].set_visible(False)
plt.tight_layout()
plt.show()


fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(6,5), sharex=False, sharey=False)
ax.scatter(total20.theta1, total20.scc)
ax.scatter(total20[total20.id==0].theta1, total20[total20.id==0].scc)
ax.set_xlabel(r'$\theta_1, \theta_2$')
ax.set_ylabel('2020 USD', loc='top', rotation='horizontal', fontweight='normal')
ax.yaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
ax.set_ylim(0, ax.get_ylim()[1])
ax.set_title('2020 SCC')
ax.spines[['top', 'right']].set_visible(False)
plt.tight_layout()
plt.show()

fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(6,5), sharex=False, sharey=False)
ax.hist(total20[total20.id!='0'].scc, bins=10)
# ax.axvline(total20[total20.id==0].at[2020, 'scc'], color='tab:orange')
ax.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
ax.annotate('$28.9', (32, 20), color='k')
# ax.set_xlabel('2020 USD', loc='center', rotation='horizontal', fontweight='normal')
ax.set_ylabel('Density', loc='center')
ax.set_title('2020 SCC')
ax.spines[['top', 'right']].set_visible(False)
plt.tight_layout()
plt.show()

fig, (ax1, ax2) = plt.subplots(nrows=1, ncols=2, figsize=(8,5), sharex=False, sharey=False)
ax1.hist(total20[total20.id!='0'].scc, bins=7, color='silver')
ax1.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
ax1.annotate('$28.9', (32, 20), color='k')
ax1.set_ylabel('A', loc='top', rotation='horizontal', fontweight='bold')
ax1.set_yticklabels([])
ax1.spines[['top', 'right']].set_visible(False)
ax2.plot(theta20.theta1, theta20.scc)
ax2.set_xlabel(r'$\theta_1, \theta_2$')
ax2.set_ylabel('B', loc='top', rotation='horizontal', fontweight='bold')
ax2.yaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
ax2.spines[['top', 'right']].set_visible(False)
fig.suptitle('2020 SCC in 2020 USD')
plt.tight_layout()
plt.show()

# %% GSA
from SALib.analyze import delta, pawn
gsa_df = df.query('oc_capital=="Total" & t==2020 & baseline=="ssp3"')

gsa_vars = list(set(df.columns) - set(['t', 'scc', 'oc_capital', 'valuation', 'id', 'baseline', 'ocean_theta_2']))
problem = {
    'num_vars': len(gsa_vars),
    'names': gsa_vars,
    # 'bounds': [[0,1], [0 , 1]]
}
x = gsa_df[gsa_vars].values
y = gsa_df.scc.values
print(x.shape, y.shape)

# analyse
delta_df = pd.DataFrame(delta.analyze(problem, x, y, seed=3465)).sort_values(by='delta', ascending=False)
pawn_df = pd.DataFrame(pawn.analyze(problem, x, y)).sort_values(by='mean', ascending=False)

# Plot
params_names_dict = {'tcre': 'TCRE', 'prstp': 'PRTP', 'vsl_start': 'VSL',
                     'ocean_theta_1': r'$\theta$',
                     'ocean_theta_2': r'$\theta_2$',
                     'ocean_health_tame': 'Total avoided\nmortality effect',
                     'ocean_consump_damage_coef': 'Consumption damage coef',
                     'ocean_area_damage_coef': 'Area damage coef',
                     'ocean_income_elasticity': 'Income elasticity\nof coral value',
                     'ocean_health_eta': '$\eta$',
                     'ocean_value_exp_nu': '$a$ (Mangroves\nnonuse)',
                     'ocean_value_intercept_nu': '$b$ (Mangroves\nnonuse)',
                     'ocean_health_mu': r'$\mu$',
                     'ocean_health_beta': r'$\beta$ (Fisheries)',
                     'ocean_value_intercept_unm': '$a$ (Mangroves\nnonmarket use)',
                     'elasmu': 'Elasticity of marginal utility\nof consumption',
                     'ocean_value_exp_unm': '$b$ (Mangroves\nnonmarket use)'
                     }
delta_df = delta_df.replace(params_names_dict)
pawn_df = pawn_df.replace(params_names_dict)

fig, (ax1, ax2) = plt.subplots(nrows=1, ncols=2, figsize=(8,5), sharex=False, sharey=True)
ax1.bar(delta_df.names, delta_df.delta)
ax1.set_xticklabels(ax1.get_xticklabels(), rotation='vertical')
ax1.axhline(0, c='k')
ax2.bar(pawn_df.names, pawn_df['mean'])
ax1.set_ylabel('A', loc='top', rotation='horizontal', fontweight='bold')
ax1.set_title('Delta Moment-Independent Measure ')
ax2.set_xticklabels(ax2.get_xticklabels(), rotation='vertical')
ax2.axhline(0, c='k')
for ax in (ax1, ax2):
    ax.spines[['top', 'right']].set_visible(False)
ax2.set_ylabel('B', loc='top', rotation='horizontal', fontweight='bold')
ax2.set_title('PAWN index (mean)')
fig.suptitle('Global sensitivity analysis for the 2020 SCC in 2020 USD')
plt.tight_layout()
plt.show()

fig, ax2 = plt.subplots(nrows=1, ncols=1, figsize=(10,5), sharex=False, sharey=True)
ax2.bar(pawn_df.names, pawn_df['mean'])
ax2.set_xticklabels(ax2.get_xticklabels(), rotation=90, ha='center')
ax2.axhline(0, c='k')
ax2.spines[['top', 'right']].set_visible(False)
# ax2.set_ylabel('B', loc='top', rotation='horizontal', fontweight='bold')
ax2.set_title('PAWN index (mean)')
fig.suptitle('Global sensitivity analysis for the 2020 SCC in 2020 USD')
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/pawn.png')
plt.show()
# %% GSA no fisheries
df_nofisheries = df.query("oc_capital!='Total' & oc_capital!='Fisheries'")
df_nofisheries['scc_sum'] = df_nofisheries.groupby(['oc_capital', 'id']).scc.transform('sum')
df_nofisheries.groupby(['oc_capital', 'id']).first()
gsa_df = df_nofisheries.copy()
gsa_vars = list(set(df_nofisheries.columns) - set(['t', 'scc', 'scc_sum', 'oc_capital', 'valuation', 'id', 'baseline', 'ocean_theta_2']))
problem = {
    'num_vars': len(gsa_vars),
    'names': gsa_vars,
    # 'bounds': [[0,1], [0 , 1]]
}
x = gsa_df[gsa_vars].values
y = gsa_df.scc_sum.values
print(x.shape, y.shape)
pawn_df = pd.DataFrame(pawn.analyze(problem, x, y)).sort_values(by='mean', ascending=False).replace(params_names_dict)

fig, ax2 = plt.subplots(nrows=1, ncols=1, figsize=(10,5), sharex=False, sharey=True)
ax2.bar(pawn_df.names, pawn_df['mean'])
ax2.set_xticklabels(ax2.get_xticklabels(), rotation=90, ha='center')
ax2.axhline(0, c='k')
ax2.spines[['top', 'right']].set_visible(False)
# ax2.set_ylabel('B', loc='top', rotation='horizontal', fontweight='bold')
ax2.set_title('PAWN index (mean)\n\nNo fisheries')
fig.suptitle('Global sensitivity analysis for the 2020 SCC in 2020 USD')
plt.tight_layout()
plt.show()

# %% GSA only fisheries
df_fisheries = df.query("oc_capital=='Fisheries'")
df_fisheries['scc_sum'] = df_fisheries.groupby(['oc_capital', 'id']).scc.transform('sum')
df_fisheries.groupby(['oc_capital', 'id']).first()
gsa_df = df_fisheries.copy()
gsa_vars = list(set(df_fisheries.columns) - set(['t', 'scc', 'scc_sum', 'oc_capital', 'valuation', 'id', 'baseline', 'ocean_theta_2']))
problem = {
    'num_vars': len(gsa_vars),
    'names': gsa_vars,
    # 'bounds': [[0,1], [0 , 1]]
}
x = gsa_df[gsa_vars].values
y = gsa_df.scc_sum.values
print(x.shape, y.shape)
pawn_df = pd.DataFrame(pawn.analyze(problem, x, y)).sort_values(by='mean', ascending=False).replace(params_names_dict)

fig, ax2 = plt.subplots(nrows=1, ncols=1, figsize=(10,5), sharex=False, sharey=True)
ax2.bar(pawn_df.names, pawn_df['mean'])
ax2.set_xticklabels(ax2.get_xticklabels(), rotation=90, ha='center')
ax2.axhline(0, c='k')
ax2.spines[['top', 'right']].set_visible(False)
# ax2.set_ylabel('B', loc='top', rotation='horizontal', fontweight='bold')
ax2.set_title('PAWN index (mean)\n\nOnly fisheries')
fig.suptitle('Global sensitivity analysis for the 2020 SCC in 2020 USD')
plt.tight_layout()
plt.show()

# %%
fig, axs = plt.subplots(nrows=4, ncols=4, figsize=(10,10), sharex=False, sharey=True)
for ax, col in zip(axs.flatten(), gsa_vars):
    ax.scatter(df[col], df.scc, color='b',  marker='+', alpha=0.1)
    ax.set_title(col, fontsize=7)
plt.tight_layout()
plt.show()
# %% Missing / nonfeasiblue runs
import statsmodels.formula.api as smf
from pathlib import Path
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib import ticker

import context

context.pdsettings()

lhs = pd.read_parquet(Path.cwd() / 'Data/SCC/out/lhs.parquet')

debug_folder = Path(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x\debug_ocean")
l = []
for file in list(debug_folder.glob('debug_ocean_damage*.gdx')):
    id = int(file.stem.split('_')[-1])
    l.append(lhs[lhs.id==id])
debug = pd.concat(l)

ok_folder = Path(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x\results_ocean")
l = []
for file in list(ok_folder.glob('results_ocean_damage_pulse*.gdx')):
    id = int(file.stem.split('_')[-1])
    l.append(lhs[lhs.id==id])
ok = pd.concat(l)

df = pd.concat([ok.assign(missing=0), debug.assign(missing=1)])

xvars =  list(set(df.select_dtypes('number').columns) - set(['missing', 'id']))
for xvar in xvars:
    mod = smf.ols(f"{xvar} ~ missing", df).fit()
    print(mod.summary2())

# %%
fig, axs = plt.subplots(nrows=5, ncols=4, figsize=(10,10), sharex=False, sharey=True)
for ax, xvar in zip(axs.flatten(), df.select_dtypes('number').columns):
    if xvar in ['missing', 'id']:
        continue
    ax.hist(df.loc[df.missing==0, xvar], histtype='step')
    ax.hist(df.loc[df.missing==1, xvar], histtype='step')
    ax.axvline(df.loc[df.missing==0, xvar].mean(), color='tab:blue')
    ax.axvline(df.loc[df.missing==1, xvar].mean(), color='tab:orange')
    ax.set_title(params_names_dict[xvar])
for i in [0, -2, -1]:
    axs.flatten()[i].set_axis_off()
plt.tight_layout()
plt.show()
mod = smf.ols(f"tcre ~ missing", df).fit()
mod.summary2()
mod.params['missing'] / df.tcre.mean()
# %%
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
"""Probability of missing by TCRE"""
df['tcre_bin'] = pd.qcut(df.tcre, [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 1]).apply(lambda x: x.mid)
avg = df.groupby('tcre_bin').missing.mean()
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(5,5), sharex=False, sharey=False)
ax.plot(avg.index, avg, zorder=1)
ax.axhline(0, color='k', linewidth=0.75, zorder=0)
ax.yaxis.set_major_formatter(ticker.PercentFormatter(1))
ax.set_xlabel('TCRE')
ax.set_ylabel('Fraction unfeasible')
ax2 = ax.twinx()
ax2.hist(df.tcre, alpha=0.5)
plt.show()