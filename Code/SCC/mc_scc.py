# %% Imports
import sys
sys.path.append('bluerice_server')
from pathlib import Path
import gdxpds
import pandas as pd
from utils import sectoral_scc
import platform
import context

import logging

context.pdsettings()

if platform.system() == 'Windows':
    root = Path(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x/bluerice")
else:
    root = Path('/work/cmcc/fg12520/RICE50x/bluerice')

logging.basicConfig(level=logging.DEBUG)


def scc_mc(run_type, mc_id):
    if not (root / run_type / f'results/results_ocean_damage_pulse_{mc_id}.gdx').is_file():
        return pd.DataFrame()
    ocean_damage_gdx = gdxpds.read_gdx.Translator(root / f'{run_type}/results/results_ocean_damage_{mc_id}.gdx', lazy_load=True)
    ocean_damage_pulse_gdx = gdxpds.read_gdx.Translator(root / f'{run_type}/results/results_ocean_damage_pulse_{mc_id}.gdx', lazy_load=True)
    ocean_today_gdx = gdxpds.read_gdx.Translator(root / f'{run_type}/results/results_ocean_today_{mc_id}.gdx', lazy_load=True)
    _l = []
    targets = [('coral', 'consumption'), ('coral', 'usenm'), ('coral', 'nonuse'), ('mangrove', 'consumption'),
               ('mangrove', 'usenm'), ('mangrove', 'nonuse'), ('ports', 'consumption'), ('fisheries', 'consumption'),
               ('fisheries', 'usenm'), (None, None)]
    for target in targets:
        try:
            _scc = sectoral_scc(ocean_today_gdx, ocean_damage_gdx, ocean_damage_pulse_gdx, target[0], target[1])
        except:
            pass
            # return pd.DataFrame()
        _target = target if target != (None, None) else ('total', 'total')
        _scc = _scc.assign(oc_capital=_target[0], valuation=_target[1], id=mc_id)
        _l.append(_scc)
    mc_sampling_df = pd.read_parquet(root / f'{run_type}/input.parquet').query(f'id=={mc_id}')
    mc_sampling_df['id'] = mc_sampling_df['id'].astype(str)
    return pd.concat(_l).reset_index().merge(mc_sampling_df, on='id', how='outer')


if __name__ == "__main__":
    if len(sys.argv) > 2:
        run_type = str(sys.argv[1])
        mc_id = str(sys.argv[2])
        logging.info(f'Computing SCC. Root:{sys.argv[1]}. ID: {sys.argv[2]}. File: {root} / {run_type}/scc/{mc_id}.parquet')
        try:
            scc_mc(run_type, mc_id).to_parquet(root / f'{run_type}/scc/{mc_id}.parquet')
        except Exception as e:
            logging.error(e)
    else:
        run_type = str(sys.argv[1])
        files = list((root / run_type / 'scc').glob('*'))
        logging.info(f'Concatenating SCCs from {root / run_type}')
        logging.info(f'Writing to {root}/scc.parquet')
        pd.concat([pd.read_parquet(f) for f in files]).to_parquet(root / f'{run_type}/scc.parquet')