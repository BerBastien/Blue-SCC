# %% Imports
from pathlib import Path
import gdxpds
import matplotlib.pyplot as plt
import geopandas as gpd
import pandas as pd
from utils import var_from_gdx
import platform
import context

context.pdsettings()

if platform.system() == 'Windows':
    root = Path(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x")
else:
    root = Path('/work/seme/fg12520/RICE50x')
results_folder = root

dam = gdxpds.to_dataframes(context.projectpath() / 'Data/SCC/out/results_ocean_baseline.gdx')
nodam = gdxpds.to_dataframes(context.projectpath() / 'Data/SCC/out/results_ocean_no_cc.gdx')

# Get temperature
TATM = var_from_gdx(dam, 'TATM')
# Variation from time=1
TATM['TATM'] = TATM['TATM'] - TATM.at[0, 'TATM']
# Consumption damage ceof
coef = var_from_gdx(dam, 'ocean_consump_damage_coef', vars=['oc_capital']).query('oc_capital=="ports"')
# GDP in time=1
YGROSS = var_from_gdx(dam, 'YGROSS').query('t=="1"').drop(columns='t')
# Consumption
C = var_from_gdx(dam, 'C')
C = C.merge(TATM).merge(coef[['n', 'ocean_consump_damage_coef']])
# Consumption loss to ports
C['ports'] = C.TATM * C.ocean_consump_damage_coef * C.C
#
server_scale_factor = var_from_gdx(dam, 'server_scale_factor').values[0][0]

l = []
for v in ['coral', 'mangrove', 'fisheries']:
    # OCEAN_USENM_VALUE with and without climate damages
    df_dam = var_from_gdx(dam, 'OCEAN_USENM_VALUE', vars=['oc_capital']).query(f'oc_capital == "{v}"')
    df_nodam = var_from_gdx(nodam, 'OCEAN_USENM_VALUE', vars=['oc_capital']).query(f'oc_capital == "{v}"')
    # Difference (divided by the scale factor used for Zeus)
    df_diff = (df_dam.OCEAN_USENM_VALUE - df_nodam.OCEAN_USENM_VALUE)/server_scale_factor
    df = df_dam.filter(['n', 't'])
    df[f'{v}'] = df_diff
    l.append(df.set_index(['n', 't']))

df = pd.concat(l, axis=1).reset_index()
df = pd.merge(df, C[['n', 't', 'ports']])
df = pd.merge(df, YGROSS[['n', 'YGROSS']])

df['t'] = df['t'].astype(int) * 5 + 2010
df = df[df.t<=2100]
df['iso_a3'] = df.n.str.upper()

df['ports_cumsum'] = df.groupby('n').ports.cumsum()
df['coral_cumsum'] = df.groupby('n').coral.cumsum()
df['mangrove_cumsum'] = df.groupby('n').mangrove.cumsum()
df['fisheries_cumsum'] = df.groupby('n').fisheries.cumsum()

for v in ['ports', 'coral', 'mangrove', 'fisheries']:
    df[f'{v}_cumsum_as_share'] = df[f'{v}_cumsum'] / df['YGROSS']

df[df['ports_cumsum_as_share'] < - 0.01].groupby('n').first().reset_index().sort_values(by='t').head(49)
df[df['coral_cumsum_as_share'] < - 0.01].groupby('n').first().reset_index().sort_values(by='t').head(49)
df[df['mangrove_cumsum_as_share'] < - 0.01].groupby('n').first().reset_index().sort_values(by='t').head(49)
df[df['fisheries_cumsum_as_share'] < - 0.01].groupby('n').first().reset_index().sort_values(by='t').head(49)

# Plot
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
for v in ['ports', 'coral', 'mangrove', 'fisheries']:
    gdf = world.merge(df[df[f'{v}_cumsum_as_share'] < - 0.01].groupby('n').first().reset_index(), how='outer')
    fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(10,5), sharex=False, sharey=False)
    gdf.plot(ax=ax, column='t', cmap='tab20c', legend=True)
    world.boundary.plot(ax=ax, linewidth=0.5, color='k')
    plt.title(v)
    plt.show()

# Export
df.drop(columns='iso_a3').to_parquet(context.projectpath() / 'Data/other/projected_damages.parquet')