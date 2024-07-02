"""
Create .bat files to run a Monte Carlo analysis
"""

from pathlib import Path
import pandas as pd
import numpy as np
from scipy.stats import qmc, truncnorm, lognorm

import context

step = 25
for i in list(range(0,1000,step)):
    txt = fr"""
    @echo off
    setlocal enabledelayedexpansion
    cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"
    REM Loop from X=1 to 10
    for /L %%X in ({i+1},1,{i+step}) do (
        echo Running GAMS with X=%%X
        IF EXIST "results_ocean\ocean_damage_%%X.gdx" (
            echo File exists.
        ) ELSE (
            echo not 
            gams run_rice50x.gms --policy=bau --workdir=results_ocean --nameout=ocean_damage_%%X --mod_ocean=1 --ocean_mc_seed=%%X  --ocean_theta_1=0.21 --ocean_theta_2=0.21
            gams run_rice50x.gms --policy=bau --workdir=results_ocean --nameout=ocean_damage_pulse_%%X --mod_ocean=1 --ocean_mc_seed=%%X --mod_emission_pulse=ocean_damage_%%X  --ocean_theta_1=0.21 --ocean_theta_2=0.21
            gams run_rice50x.gms --policy=bau --workdir=results_ocean --nameout=ocean_today_%%X --mod_ocean=1 --ocean_mc_seed=%%X --policy=simulation_tatm_exogen --climate_of_today=1  --ocean_theta_1=0.21 --ocean_theta_2=0.21
        )
    )   
    """
    with open(fr"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\blue_rice\data\tmp\mc_seed_{i+1}_{i+step}.bat", 'w') as f:
        f.write(txt)
files = list(Path(r"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\blue_rice\data\tmp").glob("mc_seed*.bat"))
main_bat = ('\n').join([f'start "1" "{x}"' for x in files])
with open(fr"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\blue_rice\data\tmp\main.bat", 'w') as f:
    f.write(main_bat)

# Deterministic run
deterministic_run = r"""
    cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"
            gams run_rice50x.gms --policy=bau --workdir=results_ocean --nameout=ocean_damage_9999 --mod_ocean=1
            gams run_rice50x.gms --policy=bau --workdir=results_ocean --nameout=ocean_damage_pulse_9999 --mod_ocean=1 --mod_emission_pulse=ocean_damage_9999
            gams run_rice50x.gms --policy=bau --workdir=results_ocean --nameout=ocean_today_9999 --mod_ocean=1 --policy=simulation_tatm_exogen --climate_of_today=1
"""
with open(fr"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\blue_rice\data\tmp\mc_seed_9999.bat", 'w') as f:
    f.write(deterministic_run)

# Senstivity to theta1, theta2
l = [r'cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"']
for i, theta in enumerate([0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]):
    l.append(rf"""
        gams run_rice50x.gms --workdir=results_ocean --nameout=ocean_damage_theta_iter{i} --mod_ocean=1 --ocean_theta_1={theta} --ocean_theta_2={theta}
        gams run_rice50x.gms --workdir=results_ocean --nameout=ocean_damage_pulse_theta_iter{i} --mod_ocean=1  --mod_emission_pulse=ocean_damage_theta_iter{i} --ocean_theta_1={theta} --ocean_theta_2={theta}
        gams run_rice50x.gms --workdir=results_ocean --nameout=ocean_today_theta_iter{i} --mod_ocean=1  --policy=simulation_tatm_exogen --climate_of_today=1 --ocean_theta_1={theta} --ocean_theta_2={theta}
        """)

with open(fr"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\blue_rice\data\tmp\mc_thetas.bat", 'w') as f:
    f.write('\n'.join(l))

# Senstivity to climate sensitivity
l = [r'cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"']
for i, theta in enumerate([0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]):
    l.append(rf"""
        gams run_rice50x.gms --workdir=results_ocean --nameout=ocean_damage_theta_iter{i} --mod_ocean=1 --ocean_theta_1={theta} --ocean_theta_2={theta}
        gams run_rice50x.gms --workdir=results_ocean --nameout=ocean_damage_pulse_theta_iter{i} --mod_ocean=1  --mod_emission_pulse=ocean_damage_theta_iter{i} --ocean_theta_1={theta} --ocean_theta_2={theta}
        gams run_rice50x.gms --workdir=results_ocean --nameout=ocean_today_theta_iter{i} --mod_ocean=1  --policy=simulation_tatm_exogen --climate_of_today=1 --ocean_theta_1={theta} --ocean_theta_2={theta}
        """)

with open(fr"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\blue_rice\data\tmp\mc_thetas.bat", 'w') as f:
    f.write('\n'.join(l))

#%% Latin hypercube sampling

uniform_params = {
                  'prstp': [0.01, 0.02],
                  'elasmu': [1.1, 1.6],
                  'baseline': [0, 1],
                  }
normal_params = [
    'ocean_income_elasticity',
    'ocean_value_intercept_unm',
    'ocean_value_exp_unm',
    'ocean_value_intercept_nu',
    'ocean_value_exp_nu',
    'ocean_area_damage_coef',
    'ocean_consump_damage_coef',
    'ocean_health_damage_coef',
    'ocean_health_damage_intercept',
    'theta',
]
lognormal_params = [
    'climsens'
]
n = 500  # Sample size

# Uniformly distributed parameters
sampler = qmc.LatinHypercube(d=len(uniform_params), seed=1234)
sample = sampler.random(n=n)
l_bounds = [x[0] for x in uniform_params.values()]
u_bounds = [x[1] for x in uniform_params.values()]
sample_scaled = qmc.scale(sample, l_bounds, u_bounds)
uniform_params_sample = sample_scaled
# Normally distributed parameters. Distribution truncated at +/- 3 SD
mean, std = 0, 1
clip_a, clip_b = -3, 3
a, b = (clip_a - mean) / std, (clip_b - mean) / std
lhd = qmc.LatinHypercube(d=len(normal_params), optimization="random-cd", seed=1234).random(n=n)
sample = truncnorm(a, b, loc=mean, scale=std).ppf(lhd)
normal_params_sample = sample
# Log-normally distributed params.
s, mu = 0.3, 1.1
lhd = qmc.LatinHypercube(d=len(lognormal_params), optimization="random-cd", seed=1234).random(n=n)
sample = lognorm(s=s, scale=np.exp(mu)).ppf(lhd)
lognormal_params_sample = sample

# Combine
sample_df = pd.concat([
    pd.DataFrame(uniform_params_sample, columns=list(uniform_params.keys())),
    pd.DataFrame(normal_params_sample, columns=list(normal_params)),
    pd.DataFrame(lognormal_params_sample, columns=list(lognormal_params)),
], axis=1)
sample_df['baseline'] = 'ssp' + (sample_df['baseline']*5+0.5).round(0).astype(int).astype(str)

# ocean_theta_1 = ocean_theta_2 = theta
if 'theta' in sample_df.columns:
    sample_df['ocean_theta_1'] = sample_df['theta']
    sample_df['ocean_theta_2'] = sample_df['theta']
    sample_df = sample_df.drop(columns='theta')
# l = [r'cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"']
# for i, row in sample_df.iterrows():
#     s = ' '.join(('--' + row.index + '=' + row.astype(str)).tolist())
#     l += [
#         f"gams run_rice50x.gms --mod_ocean=1 --workdir=ocean_rice --nameout=ocean_today_{i} --policy=simulation_tatm_exogen --climate_of_today=1 " + s,
#         f"gams run_rice50x.gms --mod_ocean=1 --workdir=ocean_rice --nameout=ocean_damage_{i} " + s,
#         f"gams run_rice50x.gms --mod_ocean=1 --workdir=ocean_rice --nameout=ocean_damage_pulse_{i} --workdir=ocean --mod_emission_pulse=--nameout=ocean_damage_{i} " + s]
#
# with open(fr"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\blue_rice\data\tmp\mc_lhs.bat", 'w') as f:
#     f.write('\n'.join(l))

l = [r'cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"']
for i, row in sample_df.iterrows():
    s = ' '.join(('--' + row.index + '=' + row.astype(str)).tolist())
    txt = f"""        
    IF EXIST "results_ocean\ocean_damage_{i}.gdx" (
            echo File exists.
        ) ELSE (
            echo not 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1 --workdir=results_ocean --nameout=ocean_today_{i} --policy=simulation_tatm_exogen --climate_of_today=1 {s} 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1 --workdir=results_ocean --nameout=ocean_damage_{i} {s} 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1 --workdir=results_ocean --nameout=ocean_damage_pulse_{i} --mod_emission_pulse=ocean_damage_{i} {s} 
        )
    """
    l.append(txt)
with open(fr"C:\Users\Granella\Dropbox (CMCC)\PhD\Research\blue_rice\data\tmp\mc_lhs.bat", 'w') as f:
    f.write('\n'.join(l))
sample_df.reset_index().rename(columns={'index':'id'}).to_parquet(context.projectpath() / 'data/out/lhs.parquet')
