# %% Imports
import sys
sys.path.append('bluerice_server')
from pathlib import Path
import gdxpds
import pandas as pd
from utils import sectoral_scc
import platform
import context

context.pdsettings()

if platform.system() == 'Windows':
    root = Path(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x")
else:
    root = Path('/work/seme/fg12520/RICE50x')


def scc_mc(mc_id, input_df):
    if not (root / f'results_ocean/results_ocean_damage_pulse_{mc_id}.gdx').is_file():
        return pd.DataFrame()
    ocean_damage_gdx = gdxpds.read_gdx.to_dataframes(root / f'results_ocean/results_ocean_damage_{mc_id}.gdx')
    ocean_damage_pulse_gdx = gdxpds.read_gdx.to_dataframes(root / f'results_ocean/results_ocean_damage_pulse_{mc_id}.gdx')
    ocean_today_gdx = gdxpds.read_gdx.to_dataframes(root / f'results_ocean/results_ocean_today_{mc_id}.gdx')
    _l = []
    targets = [('coral', 'consumption'), ('coral', 'usenm'), ('coral', 'nonuse'), ('mangrove', 'consumption'),
               ('mangrove', 'usenm'), ('mangrove', 'nonuse'), ('ports', 'consumption'), ('fisheries', 'consumption'),
               ('fisheries', 'usenm'), (None, None)]
    for target in targets:
        try:
            _scc = sectoral_scc(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, target[0], target[1], server_scale_factor=10_000)
        except:
            return pd.DataFrame()
        _target = target if target != (None, None) else ('total', 'total')
        _scc = _scc.assign(oc_capital=_target[0], valuation=_target[1], id=mc_id)
        _l.append(_scc)
    mc_sampling_df = pd.read_parquet(f'bluerice_server/{input_df}.parquet').query(f'id=={mc_id}')
    mc_sampling_df['id'] = mc_sampling_df['id'].astype(str)
    return pd.concat(_l).reset_index().merge(mc_sampling_df, on='id', how='outer')


if __name__ == "__main__":
    if len(sys.argv) > 3:
        folder = str(sys.argv[1])
        input_df = str(sys.argv[2])
        mc_id = str(sys.argv[3])
        try:
            scc_mc(mc_id, input_df).to_parquet(root / f'{folder}/{mc_id}.parquet')
        except:
            pass
    else:
        folder = str(sys.argv[1])
        files = list((root / f'{folder}').glob('*'))
        pd.concat([pd.read_parquet(f) for f in files]).to_parquet(root / f'{folder}.parquet')