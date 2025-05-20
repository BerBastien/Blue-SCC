"""Create .bat files to run a Monte Carlo analysis"""

import logging
import os
import shutil
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from pathlib import Path
from scipy.stats import qmc, truncnorm, lognorm

import context


def run(run_type, n=10_000, experiment_id='', chunk=100):
    """

    :param run_type: ['distribution', 'gsa']
    :param n:
    :param experiment_id:
    :param chunk:
    :return:
    """
    n = int(n)

    if experiment_id != '':
        experiment_id = experiment_id + '_'

    chunk = int(chunk)

    assert run_type in ['distribution', 'gsa'], f'Invalid run type: {run_type}'

    local_rice_path = "C:/Users/Granella/Dropbox (CMCC)/PhD/Research/RICE50x"
    remote_rice_path = "/work/cmcc/fg12520/RICE50x"
    root = Path(local_rice_path)
    # Create main folder
    os.makedirs(root / 'bluerice', exist_ok=True)
    # Empty and create all others
    for folder in [
        root / 'bluerice/baseline',
        root / 'bluerice/baseline/results',
        root / 'bluerice/baseline/debug',
        root / 'bluerice/ssp',
        root / 'bluerice/ssp/results',
        root / 'bluerice/ssp/debug',
        root / 'bluerice/rennert_et_al',
        root / 'bluerice/rennert_et_al/results',
        root / 'bluerice/rennert_et_al/debug',
        root / 'bluerice/prstp',
        root / 'bluerice/prstp/results',
        root / 'bluerice/prstp/debug',
    ]:
        # Create the folder
        os.makedirs(folder, exist_ok=True)

    # %% Baseline run
    baseline_run = rf"""
        cd "{local_rice_path}"
                gams run_rice50x.gms --policy=bau --n=maxiso3 --workdir=bluerice/baseline/results --debugdir=bluerice/baseline/debug --nameout=ocean_damage_BASELINE --mod_ocean=1
                gams run_rice50x.gms --policy=bau --n=maxiso3 --workdir=bluerice/baseline/results --debugdir=bluerice/baseline/debug --nameout=ocean_damage_pulse_BASELINE --mod_ocean=1 --mod_emission_pulse=ocean_damage_BASELINE
                gams run_rice50x.gms --policy=bau --n=maxiso3 --workdir=bluerice/baseline/results --debugdir=bluerice/baseline/debug --nameout=ocean_today_BASELINE --mod_ocean=1 --policy=simulation_tatm_exogen --climate_of_today=1
    """
    with open(context.projectpath() / 'Data/SCC/tmp/BASELINE.bat', 'w') as f:
        f.write(baseline_run)
    with open(root / 'bluerice/baseline/BASELINE.bat', 'w') as f:
        f.write(baseline_run)

    # SSPs
    l = []
    for i in range(1, 6):
        txt = rf"""
        cd "{local_rice_path}"
                gams run_rice50x.gms --policy=bau --n=maxiso3 --workdir=bluerice/ssp/results --debugdir=bluerice/ssp/debug --nameout=ocean_damage_ssp{i} --mod_ocean=1  --baseline=ssp{i}
                gams run_rice50x.gms --policy=bau --n=maxiso3 --workdir=bluerice/ssp/results --debugdir=bluerice/ssp/debug --nameout=ocean_damage_pulse_ssp{i} --mod_ocean=1 --mod_emission_pulse=ocean_damage_ssp{i}  --baseline=ssp{i}
                gams run_rice50x.gms --policy=bau --n=maxiso3 --workdir=bluerice/ssp/results --debugdir=bluerice/ssp/debug --nameout=ocean_today_ssp{i} --mod_ocean=1 --policy=simulation_tatm_exogen --climate_of_today=1  --baseline=ssp{i}
        """
        l.append(txt)
    with open(context.projectpath() / 'Data/SCC/tmp/SSPs.bat', 'w') as f:
        f.write('\n'.join(l))
    with open(root / 'bluerice/ssp/SSPs.bat', 'w') as f:
        f.write('\n'.join(l))

    # Rennert et al parameters
    rennert_et_al = rf"""
        cd "{local_rice_path}"
                gams run_rice50x.gms --policy=bau --n=maxiso3 --workdir=bluerice/rennert_et_al/results --debugdir=bluerice/rennert_et_al/debug --nameout=ocean_damage_rennert_et_al --mod_ocean=1 --elasmu=1.24 --prstp=0.02
                gams run_rice50x.gms --policy=bau --n=maxiso3 --workdir=bluerice/rennert_et_al/results --debugdir=bluerice/rennert_et_al/debug --nameout=ocean_damage_pulse_rennert_et_al --mod_ocean=1 --mod_emission_pulse=ocean_damage_rennert_et_al --elasmu=1.24 --prstp=0.02
                gams run_rice50x.gms --policy=bau --n=maxiso3 --workdir=bluerice/rennert_et_al/results --debugdir=bluerice/rennert_et_al/debug --nameout=ocean_today_rennert_et_al --mod_ocean=1 --policy=simulation_tatm_exogen --climate_of_today=1 --elasmu=1.24 --prstp=0.02
    """
    with open(context.projectpath() / 'Data/SCC/tmp/rennert_et_al.bat', 'w') as f:
        f.write(rennert_et_al)
    with open(root / 'bluerice/rennert_et_al/rennert_et_al.bat', 'w') as f:
        f.write(rennert_et_al)


    # Pure rate of time preference
    l = [rf'cd "{local_rice_path}"']
    for i, prstp in enumerate(np.linspace(0.01, 0.02, 3)):
        txt = fr"""\
        gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --workdir=bluerice/ssp/results --debugdir=bluerice/baseline/debug --nameout=ocean_today_{i} --policy=simulation_tatm_exogen --climate_of_today=1 --prstp={prstp} 
        gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --workdir=bluerice/ssp/results --debugdir=bluerice/baseline/debug --nameout=ocean_damage_{i} --prstp={prstp} 
        gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --workdir=bluerice/ssp/results --debugdir=bluerice/baseline/debug --nameout=ocean_damage_pulse_{i} --mod_emission_pulse=ocean_damage_{i} --prstp={prstp} 
        """
        l.append(txt)

    with open(context.projectpath() / f'Data/SCC/tmp/prstp.cmd', 'w', newline='\n') as f:
        f.write('\n'.join(l))
    with open(root / 'bluerice/prstp/prstp.bat', 'w') as f:
        f.write('\n'.join(l))


    # %% Monte Carlo for (1) GSA and (2) Distribution of SCC: Settings

    # GSA or Distribution
    if run_type == 'gsa':
        sh_folder = 'gsa' + experiment_id
        scc_folder = 'scc_gsa' + experiment_id
        uniform_params = {
            'ocean_health_mu': [-0.5, 0.5],
            'ocean_health_eta': [0.01, 0.2]
        }
        lognormal_params = {}
    if run_type == 'distribution':
        sh_folder = 'distribution' + experiment_id
        scc_folder = 'scc_distribution' + experiment_id
        uniform_params = {
            'prstp': [0.01, 0.02],
            'elasmu': [1.1, 1.6],
            # 'baseline': [0, 1],
            'ocean_health_mu': [-0.5, 0.5],
            'ocean_health_eta': [0.01, 0.2]
        }
        lognormal_params = {
            'Tecs': [0.4, np.log(3)]
        }

    for folder in [
        context.projectpath() / f'Data/SCC/out/{sh_folder}',
        context.projectpath() / f'Data/SCC/tmp/{sh_folder}',
        context.projectpath() / f'Data/SCC/tmp/{sh_folder}/runs',
        context.projectpath() / f'Data/SCC/tmp/{sh_folder}/bsub',
        context.projectpath() / f'Data/SCC/tmp/{sh_folder}/results',
        context.projectpath() / f'Data/SCC/tmp/{sh_folder}/debug',
        context.projectpath() / f'Data/SCC/tmp/{sh_folder}/scc',
        root / f'bluerice/{sh_folder}',
        root / f'bluerice/{sh_folder}/runs',
        root / f'bluerice/{sh_folder}/bsub',
        root / f'bluerice/{sh_folder}/results',
        root / f'bluerice/{sh_folder}/debug',
        root / f'bluerice/{sh_folder}/scc',
    ]:
       # Create the folder
        os.makedirs(folder, exist_ok=True)

    # %% Latin hypercube sampling
    normal_params = [
        'ocean_value_intercept_unm',
        'ocean_value_exp_unm',
        'ocean_value_intercept_nu',
        'ocean_value_exp_nu',

        'ocean_area_damage_coef_coral',
        'ocean_area_damage_coef_mangrove',

        'ocean_consump_damage_coef_coral',
        'ocean_consump_damage_coef_mangrove',
        'ocean_consump_damage_coef_ports',
        'ocean_consump_damage_coef_fisheries',
        'ocean_consump_damage_coef_sq',

        'ocean_health_tame',
        'ocean_health_beta',

        'ocean_unm_start',
        'ocean_nu_start',
    ]
    positive_normal_params = {
        'vsl_start': [7.4 / 1e6, 4.7 / 1e6],
        'theta': [0.21, 0.09],
        'ocean_income_elasticity_usenm': [0.222, 0.058],
        'ocean_income_elasticity_nonuse': [0.243, 0.068],
    }
    params = list(uniform_params.keys()) + normal_params + list(positive_normal_params.keys()) + list(
        lognormal_params.keys())
    seed = 1234
    sampler = qmc.LatinHypercube(d=len(params), optimization="random-cd", seed=seed)
    sample = pd.DataFrame(sampler.random(n=n), columns=params)

    # Uniformly distributed parameters
    l_bounds = [x[0] for x in uniform_params.values()]
    u_bounds = [x[1] for x in uniform_params.values()]
    uniform_params_sample = qmc.scale(sample[uniform_params.keys()], l_bounds, u_bounds)
    # Normally distributed parameters. Distribution truncated at +/- 3 SD
    mean, std = 0, 1
    clip_a, clip_b = -3, 3
    a, b = (clip_a - mean) / std, (clip_b - mean) / std
    normal_params_sample = truncnorm(a, b, loc=mean, scale=std).ppf(sample[normal_params])
    # Normally distributed parameters but truncated at 0 and +3 SD
    positive_normal_params_values = np.array(list(positive_normal_params.values()))
    mean, std = positive_normal_params_values[:, 0], positive_normal_params_values[:, 1]
    a, b = -mean / std, 3
    positive_normal_params_sample = truncnorm(a, 3, loc=mean, scale=std).ppf(sample[positive_normal_params.keys()])

    if run_type == 'gsa':
        lognormal_params_df = pd.DataFrame()
    elif run_type == 'distribution':
        # Log-normally distributed params.
        lognormal_params_values = np.array(list(lognormal_params.values()))
        s, mu = lognormal_params_values[:, 0], lognormal_params_values[:, 1]
        lognormal_params_sample = lognorm(s=s, scale=np.exp(mu)).ppf(sample[lognormal_params.keys()])
        lognormal_params_df = pd.DataFrame(lognormal_params_sample, columns=list(lognormal_params))

    # Combine
    sample_df = pd.concat([
        pd.DataFrame(uniform_params_sample, columns=list(uniform_params.keys())),
        pd.DataFrame(normal_params_sample, columns=list(normal_params)),
        pd.DataFrame(positive_normal_params_sample, columns=list(positive_normal_params)),
        lognormal_params_df
    ], axis=1)
    # sample_df['baseline'] = 'ssp' + (sample_df['baseline']*5+0.5).round(0).astype(int).astype(str)

    # Plot parameter distributions
    plot_dist = False
    if plot_dist:
        sample_df.hist(figsize=(12, 12), grid=False)
        fig = plt.gcf()
        fig.suptitle('MC parameter distribution')
        plt.savefig(context.projectpath() / 'Figures/SCC/MC_distributions.png')
        plt.show()

    # ocean_theta_1 = ocean_theta_2 = theta
    if 'theta' in sample_df.columns:
        sample_df['ocean_theta_1'] = sample_df['theta']
        sample_df['ocean_theta_2'] = sample_df['theta']
        sample_df = sample_df.drop(columns='theta')

    # Save inputs
    sample_df.reset_index().rename(columns={'index': 'id'}).to_parquet(
        context.projectpath() / f'Data/SCC/out/{sh_folder}/input_{run_type}.parquet')
    sample_df.reset_index().rename(columns={'index': 'id'}).to_parquet(
        root / f'bluerice/{run_type}/input.parquet')

    # %% Create scripts for parallel computing
    # Remote
    l = [rf"""cd {remote_rice_path}
    conda activate bluerice"""]
    l2 = []
    for i, row in sample_df.iterrows():
        s = ' '.join(('--' + row.index + '=' + row.astype(str)).tolist())
        txt = fr"""
        do_stuff=false
        if [ -f "{scc_folder}/{i}.parquet" ]; then
            do_stuff=true
        fi
        
        if [ "$do_stuff" = true ]; then
            echo 0.
        else\
            echo not
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --workdir=bluerice/{sh_folder}/results --debugdir=bluerice/=bluerice/{sh_folder}/debug --nameout=ocean_today_{i} --policy=simulation_tatm_exogen --climate_of_today=1 --ocean_sensitivity=1 {s} 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --workdir=bluerice/{sh_folder}/results --debugdir=bluerice/=bluerice/{sh_folder}/debug --nameout=ocean_damage_{i} --ocean_sensitivity=1 {s} 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --workdir=bluerice/{sh_folder}/results --debugdir=bluerice/=bluerice/{sh_folder}/debug --nameout=ocean_damage_pulse_{i} --mod_emission_pulse=ocean_damage_{i} --ocean_sensitivity=1 --ocean_sensitivity=1 {s} 
            python bluerice/mc_scc.py {run_type} {i} \
    
        fi
        """
        l.append(txt)
        if (i + 1) % chunk == 0:

            with open(context.projectpath() / f'Data/SCC/tmp/{sh_folder}/runs/mc_lhs_{i + 1}.sh', 'w', newline='\n') as f:
                f.write('\n'.join(l))
            with open(root / f'bluerice/{sh_folder}/runs/mc_lhs_{i + 1}.sh', 'w', newline='\n') as f:
                f.write('\n'.join(l))

            l = [rf"""cd {remote_rice_path}
            conda activate bluerice"""]
            l2.append(f'bsub -q s_medium -M 32G -P 0635 bash bluerice/{sh_folder}/runs/mc_lhs_{i + 1}.sh')

    with open(context.projectpath() / f'Data/SCC/tmp/{sh_folder}/bsub/mc_bsubs.sh', 'w', newline='\n') as f:
        f.write('\n'.join(l2))
    with open(root / f'bluerice/{sh_folder}/bsub/mc_bsubs.sh', 'w', newline='\n') as f:
        f.write('\n'.join(l2))

    # Copy mc_scc.py to RICE50x folder
    shutil.copy(context.projectpath() / 'Code/SCC/mc_scc.py', root / 'bluerice')
    shutil.copy(context.projectpath() / 'Code/SCC/utils.py', root / 'bluerice')
    shutil.copy(context.projectpath() / 'Code/SCC/context.py', root / 'bluerice')

if __name__ == "__main__":
    import sys
    try:
        run(*sys.argv[1:])
    except:
        run('distribution', n=300)
