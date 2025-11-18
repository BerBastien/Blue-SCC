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