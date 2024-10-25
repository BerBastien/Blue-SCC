"""
Create .bat files to run a Monte Carlo analysis
"""

from pathlib import Path
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from scipy.stats import qmc, truncnorm, lognorm

import context

#%% Baseline run
baseline_run = r"""
    cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"
            gams run_rice50x.gms --policy=bau --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_BASELINE --mod_ocean=1
            gams run_rice50x.gms --policy=bau --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_pulse_BASELINE --mod_ocean=1 --mod_emission_pulse=ocean_damage_BASELINE
            gams run_rice50x.gms --policy=bau --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_today_BASELINE --mod_ocean=1 --policy=simulation_tatm_exogen --climate_of_today=1
"""
with open(context.projectpath() / 'Data/SCC/tmp/mc_seed_BASELINE.bat', 'w') as f:
    f.write(baseline_run)

l = []
for i in range(1,6):
    txt = rf"""
    cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"
            gams run_rice50x.gms --policy=bau --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_ssp{i} --mod_ocean=1  --baseline=ssp{i}
            gams run_rice50x.gms --policy=bau --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_pulse_ssp{i} --mod_ocean=1 --mod_emission_pulse=ocean_damage_ssp{i}  --baseline=ssp{i}
            gams run_rice50x.gms --policy=bau --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_today_ssp{i} --mod_ocean=1 --policy=simulation_tatm_exogen --climate_of_today=1  --baseline=ssp{i}
    """
    l.append(txt)
with open(context.projectpath() / 'Data/SCC/tmp/mc_seed_SSPs.bat', 'w') as f:
    f.write('\n'.join(l))

#%% Latin hypercube sampling
uniform_params = {
                  # 'prstp': [0.01, 0.02],
                  # 'elasmu': [1.1, 1.6],
                  # 'baseline': [0, 1],
                  'ocean_health_mu': [-0.5, 0.5],
                  'ocean_health_eta': [0.01, 0.2]
                  }
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
lognormal_params = {
    # 'tcre': [0.5, 0.43]
}
params = list(uniform_params.keys()) + normal_params + list(positive_normal_params.keys()) + list(lognormal_params.keys())
n = 10000  # Sample size
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
mean, std = positive_normal_params_values[:,0], positive_normal_params_values[:,1]
a, b = -mean/std, 3
positive_normal_params_sample = truncnorm(a, 3, loc=mean, scale=std).ppf(sample[positive_normal_params.keys()])
# # Log-normally distributed params.
# lognormal_params_values = np.array(list(lognormal_params.values()))
# s, mu = lognormal_params_values[:, 0], lognormal_params_values[:, 1]
# lognormal_params_sample = lognorm(s=s, scale=np.exp(mu)).ppf(sample[lognormal_params.keys()])

# Combine
sample_df = pd.concat([
    pd.DataFrame(uniform_params_sample, columns=list(uniform_params.keys())),
    pd.DataFrame(normal_params_sample, columns=list(normal_params)),
    pd.DataFrame(positive_normal_params_sample, columns=list(positive_normal_params)),
    # pd.DataFrame(lognormal_params_sample, columns=list(lognormal_params)),
], axis=1)
# sample_df['baseline'] = 'ssp' + (sample_df['baseline']*5+0.5).round(0).astype(int).astype(str)

# Plot parameter distributions
plot_dist = False
if plot_dist:
    sample_df.hist(figsize=(12,12), grid=False)
    fig = plt.gcf()
    fig.suptitle('MC parameter distribution')
    plt.savefig(context.projectpath() / 'Figures/SCC/MC_distributions.png')
    plt.show()

# ocean_theta_1 = ocean_theta_2 = theta
if 'theta' in sample_df.columns:
    sample_df['ocean_theta_1'] = sample_df['theta']
    sample_df['ocean_theta_2'] = sample_df['theta']
    sample_df = sample_df.drop(columns='theta')

l = [r'cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"']
for i, row in sample_df.iterrows():
    s = ' '.join(('--' + row.index + '=' + row.astype(str)).tolist())
    txt = fr"""        
    SET do_stuff=false
    IF EXIST "results_ocean\results_ocean_damage_{i}.gdx" SET do_stuff=true
    IF EXIST "debug_ocean\debug_ocean_damage_{i}.gdx" SET do_stuff=true
    IF %do_stuff% == true (
            echo {i}.
        ) ELSE (
            echo not 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_today_{i} --policy=simulation_tatm_exogen --climate_of_today=1 {s} 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_{i} {s} 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_pulse_{i} --mod_emission_pulse=ocean_damage_{i} {s} 
        )
    """
    l.append(txt)
with open(context.projectpath() / 'Data/SCC/tmp/mc_lhs.bat', 'w') as f:
    f.write('\n'.join(l))
sample_df.reset_index().rename(columns={'index':'id'}).to_parquet(context.projectpath() / 'Data/SCC/out/lhs.parquet')
sample_df.reset_index().rename(columns={'index':'id'}).to_parquet(r'C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x\bluerice_server\lhs.parquet')

# Create minibatches for parallel computations
l = [r"""cd /work/seme/fg12520/RICE50x
conda activate bluerice"""]
l2 = []
# sample_df.index +=1
for i, row in sample_df.iterrows():

    s = ' '.join(('--' + row.index + '=' + row.astype(str)).tolist())
    txt = fr"""
    do_stuff=false
    if [ -f "scc/{i}.parquet" ]; then
        do_stuff=true
    fi
    
    if [ "$do_stuff" = true ]; then
        echo 0.
    else\
        echo not
        gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_today_{i} --policy=simulation_tatm_exogen --climate_of_today=1 {s} 
        gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_{i} {s} 
        gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_pulse_{i} --mod_emission_pulse=ocean_damage_{i} {s} 
        python bluerice_server/mc_arg.py {i} \

    fi
    """
    # txt = fr"""
    # SET do_stuff=false
    # IF EXIST "results_ocean\results_ocean_damage_pulse_{i}.gdx" SET do_stuff=true
    # IF %do_stuff% == true (
    #         echo {i}.
    #     ) ELSE (
    #         echo not
    #         gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_today_{i} --policy=simulation_tatm_exogen --climate_of_today=1 {s}
    #         gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_{i} {s}
    #         gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_pulse_{i} --mod_emission_pulse=ocean_damage_{i} {s}
    #     )
    # """
    l.append(txt)
    if (i+1) % 100 == 0:
        with open(context.projectpath() / f'Data/SCC/tmp/mc_lhs_{i+1}.sh', 'w', newline='\n') as f:
            f.write('\n'.join(l))
        l = [r"""cd /work/seme/fg12520/RICE50x
        conda activate bluerice"""]
        l2.append(f'bsub -q s_medium -P 0635 bash bluerice_server/mc_lhs_{i+1}.sh')

with open(context.projectpath() / f'Data/SCC/tmp/mc_lhs_bsubs.sh', 'w', newline='\n') as f:
    f.write('\n'.join(l2))
#
# l = [r'cd "/work/seme/fg12520/RICE50x"']
# for i, row in sample_df.iterrows():
#     s = ' '.join(('--' + row.index + '=' + row.astype(str)).tolist())
#     txt = fr"""bsub -J "today{i}" -n 18 -R "span[ptile=18]" -q p_short -P 0635 gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_today_{i} --policy=simulation_tatm_exogen --climate_of_today=1 {s}
# bsub -J "damage{i}" -n 18 -R "span[ptile=18]" -q p_short -P 0635 gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_{i} {s}
# bsub -w "ended(damage{i})" -n 18 -R "span[ptile=18]" -q p_short -P 0635 gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --n=maxiso3 --climate=cbsimple --workdir=results_ocean --debugdir=debug_ocean --nameout=ocean_damage_pulse_{i} --mod_emission_pulse=ocean_damage_{i} {s} """
#     l.append(txt)
# with open(context.projectpath() / 'Data/SCC/tmp/mc_lhs_zeus.sh', 'w', newline='\n') as f:
#     f.write('\n'.join(l))

l = [r'cd /work/seme/fg12520/RICE50x']
l2 = []
for i, row in sample_df.iterrows():
    txt = f'python bluerice_server/mc_arg.py {i}'
    l.append(txt)
    if (i+1) % 100 == 0:
        with open(context.projectpath() / f'Data/SCC/tmp/scc_{i+1}.sh', 'w', newline='\n') as f:
            f.write('\n'.join(l))
        l = [r'cd /work/seme/fg12520/RICE50x']
        l2.append(f'bsub -q s_medium -P 0635 bash bluerice_server/scc_{i+1}.sh')
    
with open(context.projectpath() / f'Data/SCC/tmp/scc.sh', 'w', newline='\n') as f:
    f.write('\n'.join(l2))

