# %% Imports
import re
from joblib import Parallel, delayed
from pathlib import Path
import numpy as np
import gdxpds
import matplotlib.pyplot as plt
from tqdm import tqdm
import pandas as pd
from utils import sectoral_scc, palette
from matplotlib import ticker
import platform
import context

context.pdsettings()

if platform.system() == 'Windows':
    root = Path(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x")
else:
    root = Path('/work/seme/fg12520/RICE50x')


def scc_mc(mc_id, run_type, baseline=False, country_level=False):

    if not (root / 'bluerice' / run_type / f'results/results_ocean_damage_pulse_{mc_id}.gdx').is_file():
        return pd.DataFrame()

    ocean_damage_gdx = gdxpds.read_gdx.Translator(root / 'bluerice' /  run_type / f'results/results_ocean_damage_{mc_id}.gdx', lazy_load=True)
    ocean_damage_pulse_gdx = gdxpds.read_gdx.Translator(root / 'bluerice' /  run_type / f'results/results_ocean_damage_pulse_{mc_id}.gdx', lazy_load=True)
    ocean_today_gdx = gdxpds.read_gdx.Translator(root / 'bluerice' /  run_type / f'results/results_ocean_today_{mc_id}.gdx', lazy_load=True)
    _l = []
    targets = [(None, None), ('coral', 'consumption'), ('coral', 'usenm'), ('coral', 'nonuse'), ('mangrove', 'consumption'),
               ('mangrove', 'usenm'), ('mangrove', 'nonuse'), ('ports', 'consumption'), ('fisheries', 'consumption'),
               ('fisheries', 'usenm'), ]
    for target in targets:
        try:
            _scc = sectoral_scc(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, target[0], target[1], country_level=country_level)
        except:
            pass
            return pd.DataFrame()
        _target = target if target != (None, None) else ('total', 'total')
        _scc = _scc.assign(oc_capital=_target[0], valuation=_target[1], id=mc_id)
        _l.append(_scc)
    if baseline:
        return pd.concat(_l).reset_index()
    else:
        mc_sampling_df = pd.read_parquet(Path.cwd() / 'Data/SCC/out/lhs.parquet').query(f'id=={mc_id}')
        mc_sampling_df['id'] = mc_sampling_df['id'].astype(str)
        return pd.concat(_l).reset_index().merge(mc_sampling_df, on='id', how='outer')


def get_distribution_data():
    df = pd.read_parquet(root / 'bluerice/distribution/scc.parquet')
    df = df[df.t == 2020]
    df['oc_capital'] = df.oc_capital.replace(
        {'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves',
         'total': 'Total'})

    df_sum = df.query('oc_capital!="Total"').groupby('id').scc.sum().reset_index().rename(
        columns={'scc': 'scc_sum'})
    df_tot = df.query('oc_capital=="Total"').filter(['id', 'scc']).rename(columns={'scc': 'scc_tot'})
    ids = pd.merge(df_sum, df_tot).assign(d=lambda x: x.scc_sum - x.scc_tot).query('d<1').id.unique()
    df = df[df.id.isin(ids)]
    df = df[df.groupby('id').scc.transform('min') >= 0]
    return df


def get_gsa_data():
    gsa_df = pd.read_parquet(root / 'bluerice/gsa/scc.parquet').query('oc_capital=="total" & t==2020')
    gsa_df['oc_capital'] = gsa_df.oc_capital.replace(
        {'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves',
         'total': 'Total'})
    return gsa_df


def get_baseline_data():
    baseline = scc_mc('BASELINE', 'baseline', baseline=True).query('t==2020')
    baseline['oc_capital'] = baseline.oc_capital.replace(
        {'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves',
         'total': 'Total'})
    baseline['valuation'] = baseline['valuation'] \
        .replace(
        {'consumption': 'Market value', 'usenm': 'Non-market use value', 'nonuse': 'Nonuse value', 'total': 'Total'})
    return baseline

# %% Baseline
baseline = get_baseline_data()
baseline[['oc_capital', 'valuation', 'scc']].round(2) \
    .rename(columns={'oc_capital': 'Asset type', 'valuation': 'Valuation type', 'scc': 'SCC'}) \
    .to_csv(context.projectpath() / 'Data/SCC/out/scc.csv', index=False)

plot_df = baseline[['oc_capital', 'valuation', 'scc']].round(2)
plot_df.columns = ['Asset type', 'Valuation type', 'SCC']
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(5,5), sharex=False, sharey=False)
ax.table(cellText=plot_df.values, colLabels=plot_df.columns, loc='center')
fig.tight_layout()
fig.patch.set_visible(False)
ax.axis('off')
ax.axis('tight')
plt.show()

# %% SCC distribution: Plot
plot_df = get_distribution_data().groupby(['oc_capital', 'id']).scc.sum().unstack().T
plot_df = plot_df[plot_df.median().sort_values(ascending=False).index]
plot_df = plot_df.rename(columns={'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves', 'total': 'Total'})
plot_df = plot_df[['Total', 'Fisheries', 'Corals', 'Mangroves', 'Ports']]
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
color_dict, _, _ = palette()
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
ax.legend(h[3:6] + h[:1], l[3:6] + l[:1], frameon=False,  title_fontproperties={'weight':'demibold'})
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/scc_breakdown.pdf')
plt.show()

# %% Baseline and distribution, one figure with two panels
baseline = get_baseline_data()
baseline['Group'] = pd.Categorical(baseline['oc_capital'], categories=['Total', 'Fisheries', 'Corals','Mangroves', 'Ports'], ordered=True)
baseline = baseline.sort_values('Group')

distribution = pd.read_parquet(context.projectpath() / 'Data/SCC/out/scc_distribution.parquet')
distribution = distribution[distribution.t==2020]
distribution['oc_capital'] = distribution.oc_capital.replace({'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves','total': 'Total'})
distribution_sum = distribution.query('oc_capital!="Total"').groupby('id').scc.sum().reset_index().rename(columns={'scc': 'scc_sum'})
distribution_tot = distribution.query('oc_capital=="Total"').filter(['id', 'scc']).rename(columns={'scc': 'scc_tot'})
ids = pd.merge(distribution_sum, distribution_tot).assign(d = lambda x: x.scc_sum - x.scc_tot).query('d<1').id.unique()
distribution = distribution[distribution.id.isin(ids)]

distribution = distribution.groupby(['oc_capital', 'id']).scc.sum().unstack().T
distribution = distribution[distribution.median().sort_values(ascending=False).index]
distribution = distribution.rename(columns={'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves', 'total': 'Total'})

distribution = get_distribution_data().groupby(['oc_capital', 'id']).scc.sum().unstack().T
distribution = distribution[distribution.median().sort_values(ascending=False).index]
distribution = distribution.rename(columns={'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves', 'total': 'Total'})

distribution = distribution[['Total', 'Fisheries', 'Corals', 'Mangroves', 'Ports']]

fig, axs = plt.subplots(nrows=2, ncols=1, figsize=(8,8), sharex=False, sharey=False)
ax1, ax2 = axs
# Upper panel
color_dict, _, _ = palette()
for h, (v, g) in enumerate(baseline.groupby('oc_capital', sort=False)):
    left = 0
    for i, row in g.iterrows():
        ax1.barh(v, row.scc, left=left, color=color_dict[row['valuation']], label=row['valuation'])
        left += row.scc
        print(v, row['valuation'], left)
    ax1.text(left + 3, h, f'${left:.1f}')
ax1.spines[['top', 'right']].set_visible(False)
for line in ["left","bottom"]:
    ax1.spines[line].set_position(("outward", 10))
ax1.axhline(0.5, c='k', linewidth=1)
ax1.set_xlim(0, ax1.get_xlim()[1])
ax1.set_xlabel('Social Cost of Carbon (2020 USD)')
ax1.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
h, l = ax1.get_legend_handles_labels()
ax1.legend(h[3:6] + h[:1], l[3:6] + l[:1], frameon=False,  title_fontproperties={'weight':'demibold'})
ax1.text(-0.1, 1.1, 'A. Blue social cost of carbon in 2020', fontsize=12, transform=ax1.transAxes, fontweight='bold', color='k') #, transform=ax.transAxes, fontsize=12, va='top', ha='right')
# Lower panel
color_dict = {'Market value': 'tab:blue', 'Non-market use value': 'tab:orange', 'Nonuse value': 'tab:green', 'Total': 'tab:red'}
marker_dict = {'Market value': 'x', 'Non-market use value': '+', 'Nonuse value': '*', 'Total': 'o'}
ax2.boxplot(distribution, vert=False)
ax2.set_yticklabels(distribution.columns)
posdict = {x._text: x._y for x in ax2.get_yticklabels()}
ax2.spines[['top', 'right']].set_visible(False)
for line in ["left","bottom"]:
    ax2.spines[line].set_position(("outward", 10))
ax2.axvline(0, c='k')
ax2.set_xlim(0, ax2.get_xlim()[1])
ax2.set_xlabel('Social Cost of Carbon (2020 USD)')
ax2.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
ax2.legend(frameon=False)
ax2.text(-0.1, 1.1, 'B. Uncertatinty in the Blue social cost of carbon in 2020', fontsize=12, transform=ax2.transAxes, fontweight='bold', color='k') #, transform=ax.transAxes, fontsize=12, va='top', ha='right')

plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/scc.pdf')
plt.show()

# %% Other SSPs
l = []
for i in range(1,6):
    l.append(scc_mc(f'ssp{i}', run_type='ssp', baseline=True).assign(ssp=f'SSP{i}'))
ssps = pd.concat(l).query('t==2020').replace({'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves',
                        'total': 'Total'})\
    .replace({'consumption': 'Market value', 'usenm': 'Non-market use value', 'nonuse': 'Nonuse value', 'total': 'Total'})

color_dict = palette()[0]
marker_dict = {'Market value': 'x', 'Non-market use value': '+', 'Nonuse value': '*', 'Total': 'o'}
ssps['scc_sum'] = ssps.groupby(['ssp', 'oc_capital']).scc.transform('sum')
ssps = ssps.sort_values(['ssp', 'scc_sum', 'valuation'], ascending=[True, False, True])

fig, axs = plt.subplots(nrows=3, ncols=2, figsize=(12,8), sharex=True, sharey=True)
axs = axs.flatten()
for ax, (ssp_i, ssp_df) in zip(axs, ssps.groupby('ssp')):
    for h, (v, g) in enumerate(ssp_df.groupby('oc_capital', sort=False)):
        left = 0
        for i, row in g.iterrows():
            ax.barh(v, row.scc, left=left, color=color_dict[row['valuation']], label=row['valuation'])
            left += row.scc
        ax.text(left + 3, h, f'${left:.1f}')
    ax.spines[['top', 'right']].set_visible(False)
    for line in ["left","bottom"]:
        ax.spines[line].set_position(("outward", 10))
    ax.axhline(0.5, c='k', linewidth=1)
    # ax.set_xlim(0, ax.get_xlim()[1])
    ax.set_xlabel('Social Cost of Carbon (2020 USD)')
    ax.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
    h, l = ax.get_legend_handles_labels()
    ax.set_ylabel(f'{ssp_i}', fontweight='demibold', loc='top', rotation=0, fontsize=15)
axs[-1].set_visible(False)
ax.legend(h[3:6] + h[:1], l[3:6] + l[:1], frameon=False,  title_fontproperties={'weight':'demibold'})
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/scc_breakdown_ssps.pdf')
plt.show()

# %%
# total = df[df.oc_capital == 'total']
# total20 = total[(total.index == 2020) & (total.id.str.startswith('theta')==False)]
# theta20 = df[(df.oc_capital == 'total') & (df.index==2020) & (df.id.str.startswith('theta'))].sort_values('theta1')
# fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(5,5), sharex=False, sharey=False)
# ax.plot(theta20.theta1, theta20.scc)
# ax.set_xlabel(r'$\theta_1, \theta_2$')
# ax.set_ylabel('2020 USD', loc='top', rotation='horizontal', fontweight='normal')
# ax.yaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
# ax.set_ylim(0, ax.get_ylim()[1])
# ax.set_title('2020 SCC')
# ax.spines[['top', 'right']].set_visible(False)
# plt.tight_layout()
# plt.show()
#
#
# fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(6,5), sharex=False, sharey=False)
# ax.scatter(total20.theta1, total20.scc)
# ax.scatter(total20[total20.id==0].theta1, total20[total20.id==0].scc)
# ax.set_xlabel(r'$\theta_1, \theta_2$')
# ax.set_ylabel('2020 USD', loc='top', rotation='horizontal', fontweight='normal')
# ax.yaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
# ax.set_ylim(0, ax.get_ylim()[1])
# ax.set_title('2020 SCC')
# ax.spines[['top', 'right']].set_visible(False)
# plt.tight_layout()
# plt.show()
#
# fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(6,5), sharex=False, sharey=False)
# ax.hist(total20[total20.id!='0'].scc, bins=10)
# # ax.axvline(total20[total20.id==0].at[2020, 'scc'], color='tab:orange')
# ax.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
# ax.annotate('$28.9', (32, 20), color='k')
# # ax.set_xlabel('2020 USD', loc='center', rotation='horizontal', fontweight='normal')
# ax.set_ylabel('Density', loc='center')
# ax.set_title('2020 SCC')
# ax.spines[['top', 'right']].set_visible(False)
# plt.tight_layout()
# plt.show()
#
# fig, (ax1, ax2) = plt.subplots(nrows=1, ncols=2, figsize=(8,5), sharex=False, sharey=False)
# ax1.hist(total20[total20.id!='0'].scc, bins=7, color='silver')
# ax1.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
# ax1.annotate('$28.9', (32, 20), color='k')
# ax1.set_ylabel('A', loc='top', rotation='horizontal', fontweight='bold')
# ax1.set_yticklabels([])
# ax1.spines[['top', 'right']].set_visible(False)
# ax2.plot(theta20.theta1, theta20.scc)
# ax2.set_xlabel(r'$\theta_1, \theta_2$')
# ax2.set_ylabel('B', loc='top', rotation='horizontal', fontweight='bold')
# ax2.yaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
# ax2.spines[['top', 'right']].set_visible(False)
# fig.suptitle('2020 SCC in 2020 USD')
# plt.tight_layout()
# plt.show()
pass
# %% Rennert et al
rennert_et_al = scc_mc('rennert_et_al', run_type='rennert_et_al', baseline=True).query('t==2020')
rennert_et_al['oc_capital'] = rennert_et_al.oc_capital.replace(
    {'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves',
     'total': 'Total'})
rennert_et_al['valuation'] = rennert_et_al['valuation'] \
    .replace(
    {'consumption': 'Market value', 'usenm': 'Non-market use value', 'nonuse': 'Nonuse value', 'total': 'Total'})
rennert_et_al\
    .filter(['scc', 'oc_capital', 'valuation'])\
    .reset_index(drop=True)\
    .to_csv(context.projectpath() / 'Data/SCC/out/rennert_et_al.csv', index=False)

cmap = plt.get_cmap('tab20c')
color_dict, _, _ = palette()
marker_dict = {'Market value': 'x', 'Non-market use value': '+', 'Nonuse value': '*', 'Total': 'o'}
rennert_et_al['scc_sum'] = rennert_et_al.groupby('oc_capital').scc.transform('sum')
rennert_et_al = rennert_et_al.sort_values(['scc_sum', 'valuation'], ascending=[False, True])
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(8,4), sharex=False, sharey=False)
for h, (v, g) in enumerate(rennert_et_al.groupby('oc_capital', sort=False)):
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
ax.legend(h[3:6] + h[:1], l[3:6] + l[:1], frameon=False,  title_fontproperties={'weight':'demibold'})
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/rennert_et_al.pdf')
plt.show()

# %% Country-level
country_level = scc_mc('BASELINE', run_type='baseline', baseline=True, country_level=True).query('t==2020').drop(columns=['id', 't'])
country_level['oc_capital'] = country_level.oc_capital.replace(
    {'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves',
     'total': 'Total'})
country_level['valuation'] = country_level['valuation'] \
    .replace(
    {'consumption': 'Market value', 'usenm': 'Non-market use value', 'nonuse': 'Nonuse value', 'total': 'Total'})

country_level.assign(iso3=lambda x: x.n.str.upper())\
    .sort_values(['iso3', 'oc_capital', 'valuation'])\
    .filter(['iso3', 'scc', 'oc_capital', 'valuation'])\
    .reset_index(drop=True)\
    .to_csv(context.projectpath() / 'Data/SCC/out/country_level_scc.csv', index=False)

# Plot
import geopandas as gpd
import geodatasets
import matplotlib as mpl
world = gpd.read_file(r"Data\SCC\tmp\ne_110m_admin_0_countries\ne_110m_admin_0_countries.shp")\
    .assign(iso3=lambda x: x.ADM0_A3).filter(['iso3', 'geometry'])
plot_df = country_level.assign(iso3=lambda x: x.n.str.upper())\
    .sort_values(['iso3', 'oc_capital', 'valuation'])\
    .filter(['iso3', 'scc', 'oc_capital', 'valuation'])\
    .reset_index(drop=True)
plot_df['scc_share'] = plot_df.scc / plot_df.groupby(['oc_capital', 'valuation']).scc.transform('sum')
gdf = world.merge(plot_df).query('valuation=="Total"')

fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(10, 5), sharex=False, sharey=False)
gdf.query('valuation=="Total"').plot(ax=ax, column='scc', cmap='Blues', legend=True,
                                     norm=mpl.colors.LogNorm(vmin=1.0001, vmax=5)),
world.boundary.plot(ax=ax, linewidth=0.5, color='k')
plt.show()
# %% GSA
import safepython.PAWN as PAWN
from safepython.util import aggregate_boot
import os

gsa_df = get_gsa_data()
os.system(r"Rscript C:\Users\Granella\Dropbox (CMCC)\PhD\Research\Blue-SCC\Code\SCC\OT.R")
gsa_vars = list(set(gsa_df.columns) - set(['t', 'scc', 'oc_capital', 'valuation', 'id', 'baseline', 'ocean_theta_2', 'Tecs', 'prstp', 'elasmu']))
problem = {
    'num_vars': len(gsa_vars),
    'names': gsa_vars,
    # 'bounds': [[0,1], [0 , 1]]
}
x = gsa_df[gsa_vars].values
y = gsa_df.scc.values
print(x.shape, y.shape)

# analyse
# delta_df = pd.DataFrame(delta.analyze(problem, x, y, seed=3465)).sort_values(by='delta', ascending=False)
# pawn_df = pd.DataFrame(pawn.analyze(problem, x, y)).sort_values(by='mean', ascending=False)
KS_median, KS_mean, KS_max, KS_dummy  = PAWN.pawn_indices(x, y, 5, Nboot=500, dummy=True)
# Compute mean and confidence intervals of the sensitivity indices across the bootstrap resamples:
KS_mean_m, KS_mean_lb, KS_mean_ub = aggregate_boot(KS_mean)
pawn_df = pd.DataFrame([KS_mean_m, KS_mean_lb, KS_mean_ub], columns=gsa_vars, index=['mean', 'lb', 'ub']).T
pawn_df = pawn_df.sort_values('mean').reset_index(names='names')
ot_df = pd.read_parquet(context.projectpath() / 'Data/SCC/out/OT.parquet')

# Plot
cat_dict = {
    'ocean_theta_1': 'Normative parameter',
    'ocean_health_mu': 'Fisheries',
    'ocean_health_tame': 'Fisheries',
    'ocean_area_damage_coef_coral': 'Corals',
    'ocean_consump_damage_coef_mangrove': 'Mangroves',
    'ocean_consump_damage_coef_ports': 'Ports',
    'ocean_unm_start': 'Corals',
    'ocean_value_intercept_nu': 'Mangroves',
    'ocean_consump_damage_coef_coral': 'Corals',
    'ocean_value_exp_nu': 'Mangroves',
    'ocean_income_elasticity': 'Corals',
    'ocean_income_elasticity_usenm': 'Corals',
    'ocean_income_elasticity_nonuse': 'Corals',
    'ocean_area_damage_coef': 'Corals/Mangroves',
    'ocean_consump_damage_coef_sq': 'Mangroves',
    'ocean_value_exp_unm': 'Mangroves',
    'ocean_value_intercept_unm': 'Mangroves',
    'ocean_consump_damage_coef_fisheries': 'Fisheries',
    'ocean_health_beta': 'Fisheries',
    'vsl_start': 'Fisheries',
    'ocean_nu_start': 'Corals',
    'ocean_consump_damage_coef': 'All',
    'ocean_area_damage_coef_mangrove': 'Mangroves',
    'ocean_health_eta': 'Fisheries'
}

params_names_dict = {'Tecs': 'ECS', 'prstp': 'PRTP', 'vsl_start': 'VSL',
                     'ocean_theta_1': r'$\theta$',
                     'ocean_theta_2': r'$\theta_2$',
                     'ocean_health_tame': 'Total avoided\nmortality effect',
                     'ocean_consump_damage_coef': 'Consumption damage\ncoef',
                     'ocean_consump_damage_coef_coral': 'Consumption damage\ncoef (Corals)',
                     'ocean_consump_damage_coef_fisheries': 'Consumption damage\ncoef (Fisheries)',
                     'ocean_consump_damage_coef_ports': 'Consumption damage\ncoef (Ports)',
                     'ocean_consump_damage_coef_mangrove': 'Consumption damage\ncoef (Mangroves)',
                     'ocean_consump_damage_coef_sq': 'Consumption damage\nsquared coef (Mangroves)',
                     'ocean_area_damage_coef': 'Area damage coef',
                     'ocean_area_damage_coef_coral': 'Area damage coef (Corals)',
                     'ocean_area_damage_coef_mangrove': 'Area damage coef (Mangroves)',
                     'ocean_income_elasticity_usenm': 'Income elasticity of\nnon-market use value',
                     'ocean_income_elasticity_nonuse': 'Income elasticity of\nnon-use value',
                     'ocean_health_eta': '$\eta$',
                     'ocean_value_exp_nu': '$a$ (Mangroves\nnonuse)',
                     'ocean_value_intercept_nu': '$b$ (Mangroves\nnonuse)',
                     'ocean_health_mu': r'$\mu$',
                     'ocean_health_beta': r'$\beta$ (Fisheries)',
                     'ocean_value_intercept_unm': '$a$ (Mangroves\nnonmarket use)',
                     'elasmu': 'Elasticity of marginal utility\nof consumption',
                     'ocean_value_exp_unm': '$b$ (Mangroves\nnonmarket use)',
                     'ocean_unm_start': 'Non-market value per km$^2$ at t=0',
                     'ocean_nu_start': 'Non-use value per km$^2$ at t=0',
                     }
_, cat_color_dict, cat_edgecolor_dict = palette()
# cat_color_dict = {'Normative parameter': 'silver', 'Corals': 'gold', 'Corals/Mangroves': 'gold', 'Mangroves': 'yellowgreen', 'Fisheries': 'tab:cyan', 'Ports': 'tab:purple'}
# cat_edgecolor_dict = {'Normative parameter': 'silver', 'Corals': 'gold', 'Corals/Mangroves': 'yellowgreen', 'Mangroves': 'yellowgreen', 'Fisheries': 'tab:cyan', 'Ports': 'tab:purple'}
cat_hatch_dict = {'Normative parameter': None, 'Corals': None, 'Corals/Mangroves': '/', 'Mangroves': None, 'Fisheries': None, 'Ports': None}

# delta_df = delta_df.replace(params_names_dict)
# delta_df['cat'] = delta_df.names.replace(cat_dict)
# delta_df = delta_df.sort_values(['cat', 'delta'])
pawn_df['cat'] = pawn_df.names.replace(cat_dict)
pawn_df = pawn_df.replace(params_names_dict)
pawn_df['cat_color'] = pawn_df.cat.replace(cat_color_dict)
pawn_df['cat_edgecolor'] = pawn_df.cat.replace(cat_edgecolor_dict)
pawn_df['cat_hatch'] = pawn_df.cat.replace(cat_hatch_dict)
pawn_df['cat'] = pd.Categorical(pawn_df.cat, ['Normative parameter', 'Corals', 'Corals/Mangroves', 'Mangroves', 'Fisheries', 'Ports'])
pawn_df = pawn_df.sort_values(['cat', 'mean'], ascending=[True, True])

ot_df['cat'] = ot_df.names.replace(cat_dict)
ot_df = ot_df.replace(params_names_dict)
ot_df['cat_color'] = ot_df.cat.replace(cat_color_dict)
ot_df['cat_edgecolor'] = ot_df.cat.replace(cat_edgecolor_dict)
ot_df['cat_hatch'] = ot_df.cat.replace(cat_hatch_dict)
ot_df['cat'] = pd.Categorical(ot_df.cat, ['Normative parameter', 'Corals', 'Corals/Mangroves', 'Mangroves', 'Fisheries', 'Ports'])
ot_df = ot_df.sort_values(['cat', 'ot'])

# fig, (ax1, ax2) = plt.subplots(nrows=1, ncols=2, figsize=(10,5), sharex=False, sharey=True)
# ax1.bar(delta_df.names, delta_df.delta)
# ax1.set_xticklabels(ax1.get_xticklabels(), rotation='vertical')
# ax1.axhline(0, c='k')
# ax2.bar(pawn_df.names, pawn_df['mean'])
# ax1.set_ylabel('A', loc='top', rotation='horizontal', fontweight='bold')
# ax1.set_title('Delta Moment-Independent Measure ')
# ax2.set_xticklabels(ax2.get_xticklabels(), rotation='vertical')
# ax2.axhline(0, c='k')
# for ax in (ax1, ax2):
#     ax.spines[['top', 'right']].set_visible(False)
# ax2.set_ylabel('B', loc='top', rotation='horizontal', fontweight='bold')
# ax2.set_title('PAWN index (mean)')
# fig.suptitle('Global sensitivity analysis for the 2020 SCC in 2020 USD')
# plt.tight_layout()
# plt.show()

from matplotlib.lines import Line2D
plt.rcParams['hatch.linewidth'] = 9
pawn_df = pawn_df.sort_values(['cat', 'mean'], ascending=[False, True])
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(10,7), sharex=False, sharey=True)
ax.bar(pawn_df.names, pawn_df['mean'], color=pawn_df.cat_color, hatch=pawn_df.cat_hatch, edgecolor=pawn_df.cat_edgecolor)
ax.bar(pawn_df.names, pawn_df['mean'], color='none', edgecolor=pawn_df.cat_color)
for i, row in pawn_df.reset_index(drop=True).iterrows():
    # ax.plot([i,i], [row.loc['lb'], row.loc['ub']], c='k')
    # ax.scatter([i], row.loc['mean'], c='k')
    ax.errorbar([i], row.loc['mean'], [[row.loc['mean']-row.loc['lb']], [row.loc['ub']-row.loc['mean']]], fmt='_', capsize=6, c='k')
ax.set_xticklabels(ax.get_xticklabels(), rotation=90, ha='center')
ax.axhline(0, c='k')
ax.spines[['top', 'right']].set_visible(False)
ax.set_ylabel('PAWN index', loc='top', rotation='vertical', fontweight='bold')
legend_lines = [Line2D([0], [0], color=x, lw=8) for x in cat_color_dict.values()]
del legend_lines[2]
legend_labels = list(cat_color_dict.keys())
del legend_labels[2]
ax.legend(legend_lines, legend_labels, frameon=False)
# ax.set_title('PAWN index (mean)')
# fig.suptitle('Global sensitivity analysis for the 2020 SCC in 2020 USD')
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/pawn_h.png')
plt.show()

pawn_df = pawn_df.sort_values(['cat', 'mean'], ascending=[False, True])
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(7,10), sharex=False, sharey=True)
ax.barh(pawn_df.names, pawn_df['mean'], color=pawn_df.cat_color, hatch=pawn_df.cat_hatch, edgecolor=pawn_df.cat_edgecolor)
ax.barh(pawn_df.names, pawn_df['mean'], color='none', edgecolor=pawn_df.cat_color)
for i, row in pawn_df.reset_index(drop=True).iterrows():
    # ax.plot([i,i], [row.loc['lb'], row.loc['ub']], c='k')
    # ax.scatter([i], row.loc['mean'], c='k')
    ax.errorbar(row.loc['mean'], [i], xerr=[[row.loc['mean']-row.loc['lb']], [row.loc['ub']-row.loc['mean']]], fmt='_', capsize=6, c='k')
ax.set_xticklabels(ax.get_xticklabels(), rotation=90, ha='center')
ax.spines[['top', 'right']].set_visible(False)
# ax.set_ylabel('B', loc='top', rotation='horizontal', fontweight='bold')
legend_lines = [Line2D([0], [0], color=x, lw=8) for x in cat_color_dict.values()]
del legend_lines[2]
legend_labels = list(cat_color_dict.keys())
del legend_labels[2]
ax.legend(legend_lines, legend_labels, frameon=False)
ax.set_xlabel('PAWN index (mean)')
ax.set_title('PAWN index (mean)')
fig.suptitle('Global sensitivity analysis for the 2020 SCC in 2020 USD')
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/pawn.png')
plt.show()



from matplotlib.lines import Line2D
plt.rcParams['hatch.linewidth'] = 9
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(10,7), sharex=False, sharey=True)
ax.bar(ot_df.names, ot_df['ot'], color=ot_df.cat_color, hatch=ot_df.cat_hatch, edgecolor=ot_df.cat_edgecolor)
ax.bar(ot_df.names, ot_df['ot'], color='none', edgecolor=ot_df.cat_color)
for i, row in ot_df.reset_index(drop=True).iterrows():
    ax.plot([i,i], [row.loc['ot_lb'], row.loc['ot_ub']], c='k')
    # ax.scatter([i], row.loc['mean'], c='k')
ax.set_xticklabels(ax.get_xticklabels(), rotation=90, ha='center')
ax.axhline(0, c='k')
ax.spines[['top', 'right']].set_visible(False)
# ax.set_ylabel('B', loc='top', rotation='horizontal', fontweight='bold')
legend_lines = [Line2D([0], [0], color=x, lw=8) for x in cat_color_dict.values()]
del legend_lines[2]
legend_labels = list(cat_color_dict.keys())
del legend_labels[2]
ax.legend(legend_lines, legend_labels, frameon=False)
ax.set_title('OT index')
fig.suptitle('Global sensitivity analysis for the 2020 SCC in 2020 USD')
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/OT.png')
plt.show()


# %%
from safepython.PAWN import pawn_indices
KS_median, KS_mean, KS_max, KS_dummy = pawn_indices(x, y, 5, Nboot=1, dummy=False)
pawn_bs = pd.DataFrame(np.vstack([KS_mean.mean(axis=0), np.quantile(KS_mean, 0.025, axis=0), np.quantile(KS_mean, 0.975, axis=0)]), columns=gsa_vars, index=['mean', '0.025', '0.975']).T.sort_values('mean')
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(8,5), sharex=False, sharey=False)
ax.bar(pawn_bs.index, pawn_bs['mean'])
ax.scatter(pawn_bs.index, pawn_bs['0.025'], c='k')
ax.scatter(pawn_bs.index, pawn_bs['0.975'], c='k')
ax.set_xticklabels(ax2.get_xticklabels(), rotation=90, ha='center')
ax.axhline(0, c='k')
ax.spines[['top', 'right']].set_visible(False)
plt.tight_layout()
plt.show()
# %%
fig, axs = plt.subplots(nrows=4, ncols=4, figsize=(10,10), sharex=False, sharey=True)
for ax, col in zip(axs.flatten(), gsa_vars):
    ax.scatter(df[col], df.scc, color='b',  marker='+', alpha=0.1)
    ax.set_title(col, fontsize=7)
plt.tight_layout()
plt.show()
# %% eta = 0
eta0 = scc_mc(f'eta0', baseline=True).query('t==2020')\
    .replace({'coral': 'Corals', 'fisheries': 'Fisheries', 'ports': 'Ports', 'mangrove': 'Mangroves', 'total': 'Total',
              'consumption': 'Market value', 'usenm': 'Non-market use value', 'nonuse': 'Nonuse value', 'total': 'Total'})

cmap = plt.get_cmap('tab20c')
color_dict = palette()[0]
marker_dict = {'Market value': 'x', 'Non-market use value': '+', 'Nonuse value': '*', 'Total': 'o'}
eta0['scc_sum'] = eta0.groupby('oc_capital').scc.transform('sum')
eta0 = eta0.sort_values(['scc_sum', 'valuation'], ascending=[False, True])
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(8,4), sharex=False, sharey=False)
for h, (v, g) in enumerate(eta0.groupby('oc_capital', sort=False)):
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
ax.legend(h[3:6] + h[:1], l[3:6] + l[:1], frameon=False,  title_fontproperties={'weight':'demibold'})
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/scc_eta0.png')
plt.show()
# %% PRSTP and SCC
ids = [x.stem.split('_')[-1] for x in (root / f'bluerice/prstp/results').glob('results_ocean_damage_pulse_*.gdx')]
l = []
for id in ids:
    ocean_damage_gdx = gdxpds.read_gdx.to_dataframes(root / f'results_prstp_ocean/results_ocean_damage_{id}.gdx')
    prstp = ocean_damage_gdx['prstp'].iat[0,0]
    l.append(scc_mc(id, 'prstp', baseline=True).query('t==2020 & oc_capital=="total"').assign(prstp=prstp))
scc_prstp = pd.concat(l).reset_index()

fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(8,4), sharex=False, sharey=False)
color_dict, _, _ = palette()
ax.barh(scc_prstp.index, scc_prstp.scc, color=color_dict['Total'])
for i, row in scc_prstp.iterrows():
    ax.text(row.scc + 3, i, f'${row.scc:.1f}')
ax.spines[['top', 'right']].set_visible(False)
ax.set_yticks(scc_prstp.index, scc_prstp.prstp)
for line in ["left","bottom"]:
    ax.spines[line].set_position(("outward", 10))
ax.set_xlim(0, ax.get_xlim()[1])
ax.set_xlabel('Social Cost of Carbon (2020 USD)')
ax.set_ylabel('Pure rate of time preference')
ax.xaxis.set_major_formatter(ticker.FormatStrFormatter("$%d"))
plt.tight_layout()
plt.savefig(Path().cwd() / 'Figures/SCC/scc_prstp.pdf')
plt.show()

ocean_damage_gdx = gdxpds.read_gdx.to_dataframes(root / f'results_ocean_damage_BASELINE.gdx')
ocean_damage_pulse_gdx = gdxpds.read_gdx.to_dataframes(root / f'results_ocean_damage_pulse_BASELINE.gdx')
ocean_today_gdx = gdxpds.read_gdx.to_dataframes(root / f'results_ocean_today_BASELINE.gdx')
server_scale_factor = ocean_damage_gdx['server_scale_factor'].iat[0, 0]
prstp = ocean_damage_gdx['prstp'].iat[0, 0]
_scc = sectoral_scc(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, None, None,
                    server_scale_factor=server_scale_factor).query('t==2020').assign(prstp=prstp)
print(_scc)
